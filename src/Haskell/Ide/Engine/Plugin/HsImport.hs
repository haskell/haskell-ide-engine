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

-- | Type of the symbol to import.
-- Important to offer the correct import list, or hiding code action.
data SymbolType
  = Symbol -- ^ Symbol is a simple function
  | Constructor -- ^ Symbol is a constructor
  | Type -- ^ Symbol is a type
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)


-- | What of the symbol should be taken.
-- Import a simple symbol, or a value constructor.
data SymbolKind
  = Only  SymbolName -- ^ Only the symbol should be taken
  | OneOf DatatypeName SymbolName -- ^ Some constructors or methods of the symbol should be taken: Symbol(X)
  | AllOf DatatypeName -- ^ All constructors or methods of the symbol should be taken: Symbol(..)
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

-- | Disambiguates between an import action and an hiding action.
-- Can be used to determine suggestion tpye from ghc-mod,
-- e.g. whether ghc-mod suggests to hide an identifier or to import an identifier.
-- Also important later, to know how the symbol shall be imported.
data SymbolImport a
  = Import a -- ^ the symbol to import
  | Hiding a -- ^ the symbol to hide from the import
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)


-- | Utility to retrieve the contents of the 'SymbolImport'.
-- May never fail.
extractSymbolImport :: SymbolImport a -> a
extractSymbolImport (Hiding s) = s
extractSymbolImport (Import s) = s

type ModuleName = T.Text
type SymbolName = T.Text
type DatatypeName = T.Text

-- | Wrapper for a FilePath that is used as an Input file for HsImport
newtype InputFilePath = MkInputFilePath { getInput :: FilePath }

-- | Wrapper for a FilePath that is used as an Output file for HsImport
newtype OutputFilePath = MkOutputFilePath { getOutput :: FilePath }

-- | How to import a module.
-- Can be used to express to import a whole module or only specific symbols
-- from a module.
-- Is used to either hide symbols from an import or use an import-list to
-- import only a specific symbol.
data ImportStyle
    = Simple -- ^ Import the whole module
    | Complex (SymbolImport SymbolKind) -- ^ Complex operation, import module hiding symbols or import only selected symbols.
    deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

-- | Contains information about the diagnostic, the symbol ghc-mod
-- complained about and what the kind of the symbol is and whether
-- to import or hide the symbol as suggested by ghc-mod.
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
    fileMap      <- reverseFileMap
    let resultFail = return $ IdeResultFail
          (IdeError PluginError
                    (T.pack $ "hsImport: no access to the persisted file.")
                    Null
          )
    withMappedFile origInput resultFail $ \input -> do
      tmpDir            <- liftIO getTemporaryDirectory
      (output, outputH) <- liftIO $ openTempFile tmpDir "hsimportOutput"
      liftIO $ hClose outputH
      let args = importStyleToHsImportArgs
                    (MkInputFilePath input)
                    (MkOutputFilePath output)
                    modName
                    impStyle
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

-- | Convert the import style arguments into HsImport arguments.
-- Takes an input and an output file as well as a module name.
importStyleToHsImportArgs
  :: InputFilePath -> OutputFilePath -> ModuleName -> ImportStyle -> HsImport.HsImportArgs
importStyleToHsImportArgs input output modName style =
  let defaultArgs = -- Default args, must be set every time.
        HsImport.defaultArgs { HsImport.moduleName = T.unpack modName
                             , HsImport.inputSrcFile = getInput input
                             , HsImport.outputSrcFile = getOutput output
                             }

      -- | Remove parenthesis for operators and infix operator cosntructors.
      -- HsImport demands it. E.g.
      -- > hsimport -m Data.Array.Repa -s :. -w :.
      -- import Data.Array.Repa ((:.)((:.)))
      --
      -- > hsimport -m Data.Function -s $
      -- import Data.Function (($))
      trimParenthesis :: T.Text -> T.Text
      trimParenthesis = T.dropAround isParenthesis

      isParenthesis = (`elem` ['(', ')'])

      kindToArgs :: SymbolKind -> HsImport.HsImportArgs
      kindToArgs kind = case kind of
        -- Only import a single symbol e.g. Data.Text (isPrefixOf)
        Only sym     -> defaultArgs { HsImport.symbolName = T.unpack $ trimParenthesis sym }
        -- Import a constructor e.g. Data.Mabye (Maybe(Just))
        OneOf dt sym -> defaultArgs { HsImport.symbolName = T.unpack $ trimParenthesis dt
                                    , HsImport.with = [T.unpack $ trimParenthesis sym]
                                    }
        -- Import all constructors e.g. Data.Maybe (Maybe(..))
        AllOf dt     -> defaultArgs { HsImport.symbolName = T.unpack $ trimParenthesis dt
                                    , HsImport.all = True
                                    }
  in case style of
       -- If the import style is simple, import thw whole module
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
    searchResults <- mapM Hoogle.searchModules' searchTerms
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

  -- | Turn a search term with function name into an Import Actions.
  -- The function name may be of only the exact phrase to import.
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
    :: SearchStyle -> [(ModuleName, SymbolName)] -> ImportDiagnostic -> IdeM [J.CodeAction]
  termToActions style modules impDiagnostic =
    concat <$> mapM (uncurry (importModuleAction style impDiagnostic)) modules

  -- | Creates various import actions for a module and the diagnostic.
  -- Possible import actions depend on the type of the symbol to import.
  -- It may be a 'Constructor', so the import actions need to be different
  -- to a simple function symbol.
  -- Thus, it may return zero, one or multiple import actions for a module.
  -- List of import actions does contain no duplicates.
  importModuleAction
    :: SearchStyle -> ImportDiagnostic -> ModuleName -> SymbolName -> IdeM [J.CodeAction]
  importModuleAction searchStyle impDiagnostic moduleName symbolTerm =
    catMaybes <$> sequenceA codeActions
    where
      importListActions :: [IdeM (Maybe J.CodeAction)]
      importListActions = case searchStyle of
        -- If the search has been relaxed by a custom function,
        -- we cant know how much the search query has been altered
        -- and how close the result terms are to the initial diagnostic.
        -- Thus, we cant offer more specific imports.
        Relax _ -> []
        _       -> catMaybes
          $ case extractSymbolImport $ termType impDiagnostic of
            -- If the term to import is a simple symbol, such as a function,
            -- import only this function
            Symbol
              -> [ mkImportAction moduleName impDiagnostic . Just . Only
                     <$> symName symbolTerm
                ]
            -- Constructors can be imported in two ways, either all
            -- constructors of a type or only a subset.
            -- We can only import a single constructor at a time though.
            Constructor
              -> [ mkImportAction moduleName impDiagnostic . Just . AllOf
                     <$> datatypeName symbolTerm
                 , (\dt sym -> mkImportAction moduleName impDiagnostic . Just
                    $ OneOf dt sym)
                     <$> datatypeName symbolTerm
                     <*> symName symbolTerm
                 ]
            -- If we are looking for a type, import it as just a symbol
            Type
              -> [ mkImportAction moduleName impDiagnostic . Just . Only
                     <$> symName symbolTerm]

      -- | All code actions that may be available
      -- Currently, omits all
      codeActions :: [IdeM (Maybe J.CodeAction)]
      codeActions = case termType impDiagnostic of
        Hiding _ -> [] {- If we are hiding an import, we can not import
                          a module hiding everything from it. -}
        Import _ -> [mkImportAction moduleName impDiagnostic Nothing]
                    -- ^ Simple import, import the whole module
        ++ importListActions

      -- | Retrieve the function signature of a term such as
      -- >>> signatureOf "take :: Int -> [a] -> [a]"
      -- Just " Int -> [a] -> [a]"
      signatureOf :: T.Text -> Maybe T.Text
      signatureOf sig = do
        let parts =  T.splitOn "::" sig
        typeSig <- S.tailMay parts
        S.headMay typeSig

      -- | Retrieve the datatype name of a Constructor.
      --
      -- >>> datatypeName "Null :: Data.Aeson.Internal.Types.Value"
      -- Just "Value"
      --
      -- >>> datatypeName "take :: Int -> [a] -> [a]" -- Not a constructor
      -- Just "[a]"
      --
      -- >>> datatypeName "Just :: a -> Maybe a"
      -- Just "Maybe"
      --
      -- Thus, the result of this function only makes sense,
      -- if the symbol kind of the diagnostic term is of type 'Constructor'
      datatypeName :: T.Text -> Maybe T.Text
      datatypeName sig = do
        sig_ <- signatureOf sig
        let sigParts = T.splitOn "->" sig_
        lastPart <- S.lastMay sigParts
        let dtNameSig = T.words lastPart
        qualifiedDtName <- S.headMay dtNameSig
        let qualifiedDtNameParts = T.splitOn "." qualifiedDtName
        S.lastMay qualifiedDtNameParts

      -- | Name of a symbol. May contain a function signature.
      --
      -- >>> symName "take :: Int -> [a] -> [a]"
      -- Just "take"
      --
      -- >>> symName "take"
      -- Just "take"
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
        Hiding _ -> "hiding"
        -- ^ Note, that it must never happen
        -- in combination with `symbolType == Nothing`
        Import _ -> ""
      <> case symbolType of
        Just s  -> case s of
          Only sym  -> " (" <> sym <> ")"
          AllOf dt -> " (" <> dt <> " (..))"
          OneOf dt sym -> " (" <> dt <> " (" <> sym <> "))"
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
  getImportables diag@(J.Diagnostic _ _ _ (Just "bios") msg _) =
    uncurry (ImportDiagnostic diag) <$> extractImportableTerm msg
  getImportables _ = Nothing

-- | Extract from an error message an appropriate term to search for.
-- This looks at the error message and tries to extract the expected
-- signature of an unknown function.
-- If this is not possible, Nothing is returned.
extractImportableTerm :: T.Text -> Maybe (T.Text, SymbolImport SymbolType)
extractImportableTerm dirtyMsg = do
  (n, s) <- extractedTerm
  let n' = T.strip n
  return (n', s)
  where
    importMsg = S.headMay
      -- Get rid of the rename suggestion parts
      $ T.splitOn "Perhaps you meant "
      $ T.replace "\n" " "
      -- Get rid of trailing/leading whitespace on each individual line
      $ T.unlines
      $ map T.strip
      $ T.lines
      $ T.replace "• " "" dirtyMsg

    extractedTerm = asum
      [ importMsg
          >>= T.stripPrefix "Variable not in scope: "
          >>= \name -> Just (name, Import Symbol)
      , importMsg
          >>= T.stripPrefix "Not in scope: type constructor or class ‘"
          >>= \name -> Just (T.init name, Import Type)
      , importMsg
          >>= T.stripPrefix "Data constructor not in scope: "
          >>= \name -> Just (name, Import Constructor)]


