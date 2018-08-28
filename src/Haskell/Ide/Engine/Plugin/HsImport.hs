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
import           Data.Either
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified GHC.Generics                  as Generics
import qualified GhcMod.Utils                  as GM
import           HsImport
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.MonadFunctions
import qualified Language.Haskell.LSP.Types    as J
import           Haskell.Ide.Engine.PluginUtils
import qualified Haskell.Ide.Engine.Plugin.Brittany
                                               as Brittany
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
  }

data ImportParams = ImportParams
  { file           :: Uri
  , moduleToImport :: T.Text
  }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

importCmd :: CommandFunc ImportParams J.WorkspaceEdit
importCmd = CmdSync $ \(ImportParams uri modName) -> importModule uri modName

importModule :: Uri -> T.Text -> IdeGhcM (IdeResult J.WorkspaceEdit)
importModule uri modName =
  pluginGetFile "hsimport cmd: " uri $ \origInput -> do
    fileMap <- GM.mkRevRedirMapFunc
    GM.withMappedFile origInput $ \input -> do

      tmpDir            <- liftIO getTemporaryDirectory
      (output, outputH) <- liftIO $ openTempFile tmpDir "hsimportOutput"
      liftIO $ hClose outputH

      let args = defaultArgs { moduleName    = T.unpack modName
                             , inputSrcFile  = input
                             , outputSrcFile = output
                             }
      maybeErr <- liftIO $ hsimportWithArgs defaultConfig args
      case maybeErr of
        Just err -> do
          liftIO $ removeFile output
          let msg = T.pack $ show err
          return $ IdeResultFail (IdeError PluginError msg Null)
        Nothing -> do
          newText <- liftIO $ T.readFile output
          liftIO $ removeFile output
          J.WorkspaceEdit mChanges mDocChanges <- liftToGhc $ makeDiffResult input newText fileMap

          confFile <- liftIO $ Brittany.getConfFile origInput
          -- Format the import with Brittany
          newChanges <- forM mChanges $ mapM $ mapM (formatTextEdit confFile)
          newDocChanges <- forM mDocChanges $ mapM $ \(J.TextDocumentEdit vDocId tes) -> do
            ftes <- forM tes (formatTextEdit confFile)
            return (J.TextDocumentEdit vDocId ftes)

          return $ IdeResultOk (J.WorkspaceEdit newChanges newDocChanges)

  where formatTextEdit confFile (J.TextEdit r t) = do
          -- TODO: This tab size of 2 spaces should probably be taken from a config
          ft <- fromRight t <$> liftIO (Brittany.runBrittany 2 confFile t)
          return (J.TextEdit r ft)

codeActionProvider :: CodeActionProvider
codeActionProvider plId docId _ _ context = do
  let J.List diags = context ^. J.diagnostics
      terms = mapMaybe getImportables diags

  res <- mapM (bimapM return Hoogle.searchModules) terms
  actions <- catMaybes <$> mapM (uncurry mkImportAction) (concatTerms res)

  if null actions
     then do
       let relaxedTerms = map (bimap id (head . T.words)) terms
       relaxedRes <- mapM (bimapM return Hoogle.searchModules) relaxedTerms
       relaxedActions <- catMaybes <$> mapM (uncurry mkImportAction) (concatTerms relaxedRes)
       return $ IdeResponseOk relaxedActions
     else return $ IdeResponseOk actions

  where
    concatTerms = concatMap (\(d, ts) -> map (d,) ts)

    --TODO: Check if package is already installed
    mkImportAction :: J.Diagnostic -> T.Text -> IdeM (Maybe J.CodeAction)
    mkImportAction diag modName = do
      cmd <- mkLspCommand plId "import" title  (Just cmdParams)
      return (Just (codeAction cmd))
     where
       codeAction cmd = J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [diag])) Nothing (Just cmd)
       title = "Import module " <> modName
       cmdParams = [toJSON (ImportParams (docId ^. J.uri) modName)]

    getImportables :: J.Diagnostic -> Maybe (J.Diagnostic, T.Text)
    getImportables diag@(J.Diagnostic _ _ _ (Just "ghcmod") msg _) = (diag,) <$> extractImportableTerm msg
    getImportables _ = Nothing

extractImportableTerm :: T.Text -> Maybe T.Text
extractImportableTerm dirtyMsg = T.strip <$> asum
  [ T.stripPrefix "Variable not in scope: " msg
  , T.init <$> T.stripPrefix "Not in scope: type constructor or class ‘" msg
  , T.stripPrefix "Data constructor not in scope: " msg]
  where msg = head
              -- Get rid of the rename suggestion parts
              $ T.splitOn "Perhaps you meant "
              $ T.replace "\n" " "
              -- Get rid of trailing/leading whitespace on each individual line
              $ T.unlines $ map T.strip $ T.lines
              $ T.replace "• " "" dirtyMsg
