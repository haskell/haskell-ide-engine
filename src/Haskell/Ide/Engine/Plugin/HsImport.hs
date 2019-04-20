{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
module Haskell.Ide.Engine.Plugin.HsImport where

import           Control.Lens.Operators
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.Aeson
import           Data.Bitraversable
import           Data.Bifunctor
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified GHC.Generics                  as Generics
import qualified GhcMod.Utils                  as GM
import           HsImport
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

-- | Import Parameters for Modules.
-- Can be used to import every symbol from a module,
-- or to import only a specific function from a module.
data ImportParams = ImportParams
  { file            :: Uri -- ^ Uri to the file to import the module to.
  , addToImportList :: Maybe T.Text -- ^ If set, an import-list will be created.
  , moduleToImport  :: T.Text -- ^ Name of the module to import.
  }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

importCmd :: CommandFunc ImportParams J.WorkspaceEdit
importCmd = CmdSync $ \(ImportParams uri importList modName) ->
  importModule uri importList modName

-- | Import the given module for the given file.
-- May take an explicit function name to perform an import-list import.
-- Multiple import-list imports will result in merged imports,
-- e.g. two consecutive imports for the same module will result in a single
-- import line.
importModule
  :: Uri -> Maybe T.Text -> T.Text -> IdeGhcM (IdeResult J.WorkspaceEdit)
importModule uri importList modName =
  pluginGetFile "hsimport cmd: " uri $ \origInput -> do
    shouldFormat <- formatOnImportOn <$> getConfig
    fileMap      <- GM.mkRevRedirMapFunc
    GM.withMappedFile origInput $ \input -> do

      tmpDir            <- liftIO getTemporaryDirectory
      (output, outputH) <- liftIO $ openTempFile tmpDir "hsimportOutput"
      liftIO $ hClose outputH

      let args = defaultArgs { moduleName    = T.unpack modName
                             , inputSrcFile  = input
                             , symbolName    = T.unpack $ fromMaybe "" importList
                             , outputSrcFile = output
                             }
      -- execute hsimport on the given file and write into a temporary file.
      maybeErr <- liftIO $ hsimportWithArgs defaultConfig args
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
                  let formatEdit :: J.TextEdit -> IdeGhcM J.TextEdit
                      formatEdit origEdit@(J.TextEdit r t) = do
                        -- TODO: are these default FormattingOptions ok?
                        res <- liftToGhc $ provider t uri FormatText (FormattingOptions 2 True)
                        let formatEdits = case res of
                                            IdeResultOk xs -> xs
                                            _ -> [origEdit]
                        -- let edits = foldl' J.editTextEdit origEdit formatEdits -- TODO: this seems broken.
                        -- liftIO $ hPutStrLn stderr $ "Text Edits: " ++ show formatEdits
                        return (J.TextEdit r (J._newText $ head formatEdits))

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
    :: SearchStyle -> [(J.Diagnostic, T.Text)] -> IdeM [J.CodeAction]
  importActionsForTerms style terms = do
    let searchTerms   = map (bimap id (applySearchStyle style)) terms
    -- Get the function names for a nice import-list title.
    let functionNames = map (head . T.words . snd) terms
    searchResults' <- mapM (bimapM return Hoogle.searchModules) searchTerms
    let searchResults = zip functionNames searchResults'
    let normalise =
          concatMap (\(a, b) -> zip (repeat a) (concatTerms b)) searchResults

    concat <$> mapM (uncurry (termToActions style)) normalise

  -- | Apply the search style to given term.
  -- Can be used to look for a term that matches exactly the search term,
  -- or one that matches only the exact name.
  -- At last, a custom relaxation function can be passed for more control.
  applySearchStyle :: SearchStyle -> T.Text -> T.Text
  applySearchStyle Exact term = "is:exact " <> term
  applySearchStyle ExactName term = case T.words term of
    [] -> term
    (x:_) -> "is:exact " <> x
  applySearchStyle (Relax relax) term = relax term

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
    :: SearchStyle -> T.Text -> (J.Diagnostic, T.Text) -> IdeM [J.CodeAction]
  termToActions style functionName (diagnostic, termName) = do
    let useImportList = case style of
          Relax _ -> Nothing
          _       -> Just (mkImportAction (Just functionName) diagnostic termName)
    catMaybes <$> sequenceA
      (mkImportAction Nothing diagnostic termName : maybeToList useImportList)

  concatTerms :: (a, [b]) -> [(a, b)]
  concatTerms (a, b) = zip (repeat a) b

  --TODO: Check if package is already installed
  mkImportAction
    :: Maybe T.Text -> J.Diagnostic -> T.Text -> IdeM (Maybe J.CodeAction)
  mkImportAction importList diag modName = do
    cmd <- mkLspCommand plId "import" title (Just cmdParams)
    return (Just (codeAction cmd))
   where
    codeAction cmd = J.CodeAction title
                                  (Just J.CodeActionQuickFix)
                                  (Just (J.List [diag]))
                                  Nothing
                                  (Just cmd)
    title =
      "Import module "
        <> modName
        <> maybe "" (\name -> " (" <> name <> ")") importList
    cmdParams = [toJSON (ImportParams (docId ^. J.uri) importList modName)]


  -- | For a Diagnostic, get an associated function name.
  -- If Ghc-Mod can not find any candidates, Nothing is returned.
  getImportables :: J.Diagnostic -> Maybe (J.Diagnostic, T.Text)
  getImportables diag@(J.Diagnostic _ _ _ (Just "ghcmod") msg _) =
    (diag, ) <$> extractImportableTerm msg
  getImportables _ = Nothing

-- | Extract from an error message an appropriate term to search for.
-- This looks at the error message and tries to extract the expected
-- signature of an unknown function.
-- If this is not possible, Nothing is returned.
extractImportableTerm :: T.Text -> Maybe T.Text
extractImportableTerm dirtyMsg = T.strip <$> asum
  [ T.stripPrefix "Variable not in scope: " msg
  , T.init <$> T.stripPrefix "Not in scope: type constructor or class ‘" msg
  , T.stripPrefix "Data constructor not in scope: " msg
  ]
 where
  msg =
    head
        -- Get rid of the rename suggestion parts
      $ T.splitOn "Perhaps you meant "
      $ T.replace "\n" " "
        -- Get rid of trailing/leading whitespace on each individual line
      $ T.unlines
      $ map T.strip
      $ T.lines
      $ T.replace "• " "" dirtyMsg
