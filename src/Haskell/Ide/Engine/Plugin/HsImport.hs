{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
module Haskell.Ide.Engine.Plugin.HsImport where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Bitraversable
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid ((<>))
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified GHC.Generics                  as Generics
import qualified GhcMod.Utils                  as GM
import           HsImport
import           Haskell.Ide.Engine.MonadTypes
import qualified Language.Haskell.LSP.Types    as J
import           Haskell.Ide.Engine.PluginUtils
import qualified Haskell.Ide.Engine.Plugin.Hoogle as Hoogle
import           System.Directory
import           System.IO

hsimportDescriptor :: PluginDescriptor
hsimportDescriptor = PluginDescriptor
  { pluginName = "hsimport"
  , pluginDesc = "A tool for extending the import list of a Haskell source file."
  , pluginCommands = [PluginCommand "import" "Import a module" importCmd]
  , pluginCodeActionProvider = codeActionProvider
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
          workspaceEdit <- liftToGhc $ makeDiffResult input newText fileMap
          return $ IdeResultOk workspaceEdit

codeActionProvider :: CodeActionProvider
codeActionProvider docId _ _ context = do
  let J.List diags = context ^. J.diagnostics
      terms = mapMaybe getImportables diags

  res <- mapM (bimapM return Hoogle.searchModules) terms
  let actions = mapMaybe (uncurry mkImportAction) (concatTerms res)

  if null actions
     then do
       let relaxedTerms = map (bimap id (head . T.words)) terms
       relaxedRes <- mapM (bimapM return Hoogle.searchModules) relaxedTerms
       let relaxedActions = mapMaybe (uncurry mkImportAction) (concatTerms relaxedRes)
       return $ IdeResponseOk relaxedActions
     else return $ IdeResponseOk actions

  where
    concatTerms = concatMap (\(d, ts) -> map (d,) ts)

    --TODO: Check if package is already installed
    mkImportAction :: J.Diagnostic -> T.Text -> Maybe J.CodeAction
    mkImportAction diag modName = Just codeAction
     where
       codeAction = J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [diag])) Nothing (Just cmd)
       cmd = J.Command title cmdName (Just cmdParams)
       title = "Import module " <> modName
       cmdName = "hsimport:import"
       cmdParams = toJSON [ImportParams (docId ^. J.uri) modName]

    getImportables :: J.Diagnostic -> Maybe (J.Diagnostic, T.Text)
    getImportables diag@(J.Diagnostic _ _ _ (Just "ghcmod") msg _) = (diag,) <$> extractImportableTerm msg
    getImportables _ = Nothing

extractImportableTerm :: T.Text -> Maybe T.Text
extractImportableTerm dirtyMsg = T.strip <$> asum
  [T.stripPrefix "Variable not in scope: " msg,
  T.init <$> T.stripPrefix "Not in scope: type constructor or class ‘" msg]
  where msg = head
              -- Get rid of the rename suggestion parts
              $ T.splitOn "Perhaps you meant "
              $ T.replace "\n" " "
              -- Get rid of trailing/leading whitespace on each individual line
              $ T.unlines $ map T.strip $ T.lines
              $ T.replace "• " "" dirtyMsg
