{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Haskell.Ide.Engine.Plugin.HsImport where

import           Control.Lens.Operators
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.Aeson
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified GHC.Generics                  as Generics
import qualified GhcModCore                    as GM ( mkRevRedirMapFunc, withMappedFile )
import qualified HsImport
import           Haskell.Ide.Engine.Config
import           Haskell.Ide.Engine.MonadTypes
import qualified Haskell.Ide.Engine.Support.HieExtras as Hie
import qualified Language.Haskell.LSP.Types      as J
import qualified Language.Haskell.LSP.Types.Lens as J
import           Haskell.Ide.Engine.PluginUtils
import qualified Haskell.Ide.Engine.Plugin.Hoogle
                                               as Hoogle
import           System.Directory
import           System.IO
import qualified Safe as S

hsimportDescriptor :: PluginId -> PluginDescriptor
hsimportDescriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginName = "HsImport"
  , pluginDesc = "A tool for extending the import list of a Haskell source file."
  , pluginCommands = [PluginCommand "import" "Import a module" importCmd]
  , pluginCodeActionProvider = Just codeActionProvider
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  , pluginFormattingProvider = Nothing
  }

data SymbolType
  = Symbol
  | Constructor
  | Type
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)


-- | What of the symbol should be taken.
data SymbolKind
  = Only  SymbolName -- ^ only the symbol should be taken
  | AllOf DatatypeName -- ^ all constructors or methods of the symbol should be taken: Symbol(..)
  | OneOf DatatypeName SymbolName -- ^ some constructors or methods of the symbol should be taken: Symbol(X, Y)
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

-- | The imported or from the import hidden symbol.
data SymbolImport a
  = Import a -- ^ the symbol to import
  | Hiding a -- ^ the symbol to hide from the import
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)


extractSymbolImport :: SymbolImport a -> a
extractSymbolImport (Hiding s) = s
extractSymbolImport (Import s) = s

type ModuleName = T.Text
type SymbolName = T.Text
type DatatypeName = T.Text

data ImportStyle
    = Simple -- ^ Import the whole module
    | Complex (SymbolImport SymbolKind) -- ^ Complex operation, import module hiding symbols or import only selected symbols.
    deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

data ImportDiagnostic = ImportDiagnostic
  { diagnostic :: J.Diagnostic
  , term :: SymbolName
  , termType :: SymbolImport SymbolType
  }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)


-- | Import Parameters for Modules.
-- Can be used to import every symbol from a module,
-- or to import only a specific function from a module.
data ImportParams = ImportParams
  { file           :: Uri -- ^ Uri to the file to import the module to.
  , importStyle    :: ImportStyle -- ^ How to import the module
  , moduleToImport :: ModuleName -- ^ Name of the module to import.
  }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

importCmd :: CommandFunc ImportParams J.WorkspaceEdit
importCmd = CmdSync $ \(ImportParams uri style modName) ->
  importModule uri style modName

-- | Import the given module for the given file.
-- May take an explicit function name to perform an import-list import.
-- Multiple import-list imports will result in merged imports,
-- e.g. two consecutive imports for the same module will result in a single
-- import line.
importModule
  :: Uri -> ImportStyle -> ModuleName -> IdeGhcM (IdeResult J.WorkspaceEdit)
importModule uri impStyle modName =
  pluginGetFile "hsimport cmd: " uri $ \origInput -> do
    shouldFormat <- formatOnImportOn <$> getConfig
    fileMap      <- GM.mkRevRedirMapFunc
    GM.withMappedFile origInput $ \input -> do

      tmpDir            <- liftIO getTemporaryDirectory
      (output, outputH) <- liftIO $ openTempFile tmpDir "hsimportOutput"
      liftIO $ hClose outputH
      let args = importStyleToHsImportArgs input output modName impStyle
      -- execute hsimport on the given file and write into a temporary file.
      maybeErr <- liftIO $ HsImport.hsimportWithArgs HsImport.defaultConfig args
      case maybeErr of
        Just err -> do
          liftIO $ removeFile output
          let msg = T.pack $ show err
          return $ IdeResultFail (IdeError PluginError msg Null)
        Nothing -> do
          -- Since no error happened, calculate the differences of
          -- the original file and after the import has been done.
          newText <- liftIO $ T.readFile output
          liftIO $ removeFile output
          J.WorkspaceEdit mChanges mDocChanges <- liftToGhc
            $ makeDiffResult input newText fileMap

          -- If the client wants its import formatted,
          -- it can be configured in the config.
          if shouldFormat
            then do
              config  <- getConfig
              plugins <- getPlugins
              let mprovider = Hie.getFormattingPlugin config plugins
              case mprovider of
                -- Client may have no formatter selected
                -- but still the option to format on import.
                Nothing ->
                  return $ IdeResultOk (J.WorkspaceEdit mChanges mDocChanges)

                Just (_, provider) -> do
                  let
                      -- | Dirty little hack.
                      -- Necessary in the following case:
                      -- We want to add an item to an existing import-list.
                      -- The diff algorithm does not count the newline character
                      -- as part of the diff between new and old text.
                      -- However, some formatters (Brittany), add a trailing
                      -- newline nevertheless.
                      -- This leads to the problem that an additional
                      -- newline is inserted into the source.
                      -- This function makes sure, that if the original text
                      -- did not have a newline, none will be added, assuming
                      -- that the diff algorithm continues to not count newlines
                      -- as part of the diff.
                      -- This is only save to do in this very specific environment.
                      -- In any other case, this function may not be copy-pasted
                      -- to solve a similar problem.
                      renormalise :: T.Text -> T.Text -> T.Text
                      renormalise orig formatted
                        | T.null orig || T.null formatted = orig <> formatted
                        | T.last orig /= '\n' && T.last formatted == '\n' = T.init formatted
                        | otherwise = formatted

                      formatEdit :: J.TextEdit -> IdeGhcM J.TextEdit
                      formatEdit origEdit@(J.TextEdit r t) = do
                        -- TODO: are these default FormattingOptions ok?
                        formatEdits <-
                          liftToGhc $ provider t uri FormatText (FormattingOptions 2 True) >>= \case
                            IdeResultOk xs -> return xs
                            _              -> return [origEdit]
                        -- let edits = foldl' J.editTextEdit origEdit formatEdits -- TODO: this seems broken.
                        return (J.TextEdit r (renormalise t . J._newText $ head formatEdits))

                  -- behold: the legendary triple mapM
                  newChanges <- (mapM . mapM . mapM) formatEdit mChanges

                  newDocChanges <- forM mDocChanges $ \change -> do
                    let cmd (J.TextDocumentEdit vids edits) = do
                          newEdits <- mapM formatEdit edits
                          return $ J.TextDocumentEdit vids newEdits
                    mapM cmd change

                  return
                    $ IdeResultOk (J.WorkspaceEdit newChanges newDocChanges)
            else return $ IdeResultOk (J.WorkspaceEdit mChanges mDocChanges)

importStyleToHsImportArgs
  :: FilePath -> FilePath -> ModuleName -> ImportStyle -> HsImport.HsImportArgs
importStyleToHsImportArgs input output modName style =
  let defaultArgs =
        HsImport.defaultArgs { HsImport.moduleName = T.unpack modName
                             , HsImport.inputSrcFile = input
                             , HsImport.outputSrcFile = output
                             }
      kindToArgs kind = case kind of
        Only sym     -> defaultArgs { HsImport.symbolName = T.unpack sym }
        OneOf dt sym -> defaultArgs { HsImport.symbolName = T.unpack dt
                                    , HsImport.with = [T.unpack sym]
                                    }
        AllOf dt     -> defaultArgs { HsImport.symbolName = T.unpack dt
                                    , HsImport.all = True
                                    }
  in case style of
       Simple    -> defaultArgs
       Complex s -> case s of
         Hiding kind -> kindToArgs kind {- TODO: wait for hsimport version bump -}
         Import kind -> kindToArgs kind


-- | Search style for Hoogle.
-- Can be used to look either for the exact term,
-- only the exact name or a relaxed form of the term.
data SearchStyle
  = Exact -- ^ If you want to match exactly the search string.
  | ExactName -- ^ If you want to match exactly a function name.
              -- Same as @Exact@ if the term is just a function name.
  | Relax (T.Text -> T.Text) -- ^ Relax the search term to match even more.

-- | Produces code actions.
codeActionProvider :: CodeActionProvider
codeActionProvider plId docId _ context = do
  let J.List diags = context ^. J.diagnostics
      terms        = mapMaybe getImportables diags
  -- Search for the given diagnostics and produce appropiate import actions.
  actions <- importActionsForTerms Exact terms

  if null actions
    then do
      -- If we didn't find any exact matches, relax the search terms.
      -- Only looks for the function names, not the exact siganture.
      relaxedActions <- importActionsForTerms ExactName terms
      return $ IdeResultOk relaxedActions
    else return $ IdeResultOk actions

 where
  -- | Creates CodeActions from the diagnostics to add imports.
  -- Takes a relaxation Function. Used to relax the search term,
  -- e.g. instead of `take :: Int -> [a] -> [a]` use `take` as the search term.
  --
  -- List of Diagnostics with the associated term to look for.
  -- Diagnostic that is supposed to import the appropiate term.
  --
  -- Result may produce several import actions, or none.
  importActionsForTerms
    :: SearchStyle -> [ImportDiagnostic] -> IdeM [J.CodeAction]
  importActionsForTerms style importDiagnostics = do
    let searchTerms   = map (applySearchStyle style . term) importDiagnostics
    searchResults <- mapM Hoogle.searchModules searchTerms
    let importTerms = zip searchResults importDiagnostics
    concat <$> mapM (uncurry (termToActions style)) importTerms

  -- | Apply the search style to given term.
  -- Can be used to look for a term that matches exactly the search term,
  -- or one that matches only the exact name.
  -- At last, a custom relaxation function can be passed for more control.
  applySearchStyle :: SearchStyle -> T.Text -> T.Text
  applySearchStyle Exact termName = "is:exact " <> termName
  applySearchStyle ExactName termName = case T.words termName of
    [] -> termName
    (x : _) -> "is:exact " <> x
  applySearchStyle (Relax relax) termName = relax termName

  -- | Turn a search term with function name into Import Actions.
  -- Function name may be of only the exact phrase to import.
  -- The resulting CodeAction's contain a general import of a module or
  -- uses an Import-List.
  --
  -- Note, that repeated use of the Import-List will add imports to
  -- the appropriate import line, e.g. no module import is duplicated, except
  -- for qualified imports.
  --
  -- If the search term is relaxed in a custom way,
  -- no import list can be offered, since the function name
  -- may be not the one we expect.
  termToActions
    :: SearchStyle -> [ModuleName] -> ImportDiagnostic -> IdeM [J.CodeAction]
  termToActions style modules impDiagnostic =
    concat <$> mapM (importModuleAction style impDiagnostic) modules

  importModuleAction
    :: SearchStyle -> ImportDiagnostic -> ModuleName -> IdeM [J.CodeAction]
  importModuleAction searchStyle impDiagnostic moduleName =
    catMaybes <$> sequenceA codeActions
    where
      importListActions :: [IdeM (Maybe J.CodeAction)]
      importListActions = case searchStyle of
        Relax _ -> []
        _       -> catMaybes
          $ case extractSymbolImport $ termType impDiagnostic of
            Symbol
              -> [ mkImportAction moduleName impDiagnostic . Just . Only
                     <$> symName (term impDiagnostic)
                ]
            Constructor
              -> [ mkImportAction moduleName impDiagnostic . Just . AllOf
                     <$> datatypeName (term impDiagnostic)
                 , (\dt sym -> mkImportAction moduleName impDiagnostic . Just
                    $ OneOf dt sym)
                     <$> datatypeName (term impDiagnostic)
                     <*> symName (term impDiagnostic)
                 ]
            Type
              -> [ mkImportAction moduleName impDiagnostic . Just . Only
                     <$> symName (term impDiagnostic)]

      codeActions :: [IdeM (Maybe J.CodeAction)]
      codeActions = case termType impDiagnostic of
        Hiding _ -> []
        Import _ -> [mkImportAction moduleName impDiagnostic Nothing]
        ++ importListActions

      signatureOf :: T.Text -> Maybe T.Text
      signatureOf sig = do
        let parts =  T.splitOn "::" sig
        typeSig <- S.tailMay parts
        S.headMay typeSig

      datatypeName :: T.Text -> Maybe T.Text
      datatypeName sig = do
        sig_ <- signatureOf sig
        let sigParts = T.splitOn "->" sig_
        lastPart <- S.lastMay sigParts
        let dtNameSig = T.words lastPart
        qualifiedDtName <- S.headMay dtNameSig
        let qualifiedDtNameParts = T.splitOn "." qualifiedDtName
        S.lastMay qualifiedDtNameParts

      symName :: T.Text -> Maybe SymbolName
      symName = S.headMay . T.words


  --TODO: Check if package is already installed
  mkImportAction
    :: ModuleName -> ImportDiagnostic -> Maybe SymbolKind -> IdeM (Maybe J.CodeAction)
  mkImportAction modName importDiagnostic symbolType = do
    cmd <- mkLspCommand plId "import" title (Just cmdParams)
    return (Just (codeAction cmd))
   where
    codeAction cmd = J.CodeAction title
                                  (Just J.CodeActionQuickFix)
                                  (Just (J.List [diagnostic importDiagnostic]))
                                  Nothing
                                  (Just cmd)
    title = "Import module "
      <> modName
      <> case termType importDiagnostic of
        Hiding _ -> "hiding "
        Import _ -> ""
      <> case symbolType of
        Just s  -> case s of
          Only sym  -> "(" <> sym <> ")"
          AllOf dt -> "(" <> dt <> " (..))"
          OneOf dt sym -> "(" <> dt <> " (" <> sym <> "))"
        Nothing -> ""

    importStyleParam :: ImportStyle
    importStyleParam = case symbolType of
        Nothing -> Simple
        Just k -> case termType importDiagnostic of
          Hiding _ -> Complex (Hiding k)
          Import _ -> Complex (Import k)

    cmdParams = [toJSON (ImportParams (docId ^. J.uri) importStyleParam modName)]


  -- | For a Diagnostic, get an associated function name.
  -- If Ghc-Mod can not find any candidates, Nothing is returned.
  getImportables :: J.Diagnostic -> Maybe ImportDiagnostic
  getImportables diag@(J.Diagnostic _ _ _ (Just "ghcmod") msg _) =
    uncurry (ImportDiagnostic diag) <$> extractImportableTerm msg
  getImportables _ = Nothing

-- | Extract from an error message an appropriate term to search for.
-- This looks at the error message and tries to extract the expected
-- signature of an unknown function.
-- If this is not possible, Nothing is returned.
extractImportableTerm :: T.Text -> Maybe (T.Text, (SymbolImport SymbolType) )
extractImportableTerm dirtyMsg =
  let extractedTerm  =
        asum
          [ (\name -> (name, Import Symbol)) <$> T.stripPrefix "Variable not in scope: " importMsg
          , (\name -> (T.init name, Import Type)) <$> T.stripPrefix "Not in scope: type constructor or class ‘" importMsg
          , (\name -> (name, Import Constructor)) <$> T.stripPrefix "Data constructor not in scope: " importMsg
          ]
  in do
    (n, s) <- extractedTerm
    let n' = T.strip n
    return (n', s)
 where
  importMsg =
    head
        -- Get rid of the rename suggestion parts
      $ T.splitOn "Perhaps you meant "
      $ T.replace "\n" " "
        -- Get rid of trailing/leading whitespace on each individual line
      $ T.unlines
      $ map T.strip
      $ T.lines
      $ T.replace "• " "" dirtyMsg
