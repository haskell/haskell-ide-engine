{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Haskell.Ide.Engine.Plugin.HsImport where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified GHC.Generics                  as Generics
import qualified GhcMod.Utils                  as GM
import           HsImport
import           Haskell.Ide.Engine.MonadTypes
import qualified Language.Haskell.LSP.Types    as J
import           Haskell.Ide.Engine.PluginUtils
import           System.Directory
import           System.IO

hsimportDescriptor :: PluginDescriptor
hsimportDescriptor = PluginDescriptor
  { pluginName = "hsimport"
  , pluginDesc = "A tool for extending the import list of a Haskell source file."
  , pluginCommands = [PluginCommand "import" "Import a module" importCmd]
  , pluginCodeActions = noCodeActions
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
