{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.BasePlugin where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Foldable
import           Data.List
import qualified Data.Text as T
import           Development.GitRev (gitCommitCount)
import           Distribution.System (buildArch)
import           Distribution.Text (display)
import           Haskell.Ide.Engine.PluginDescriptor
import           Options.Applicative.Simple
import qualified Data.Map as Map
import qualified Paths_haskell_ide_engine as Meta
import           Prelude hiding (log)
import           System.Directory

-- ---------------------------------------------------------------------

baseDescriptor :: PluginDescriptor
baseDescriptor = PluginDescriptor
  {
    pdUiCommands =
      [
        UiCommand
          { uiCmdName = "version"
          , uiContexts = [CtxNone]
          , uiAdditionalParams = []
          , uiFunc = versionCmd
          }
      , UiCommand
          { uiCmdName = "plugins"
          , uiContexts = [CtxNone]
          , uiAdditionalParams = []
          , uiFunc = pluginsCmd
          }
      , UiCommand
          { uiCmdName = "commands"
          , uiContexts = [CtxNone]
          , uiAdditionalParams = [RP "plugin"]
          , uiFunc = commandsCmd
          }
      , UiCommand
          { uiCmdName = "pwd"
          , uiContexts = [CtxNone]
          , uiAdditionalParams = []
          , uiFunc = pwdCmd
          }
      , UiCommand
          { uiCmdName = "cwd"
          , uiContexts = [CtxNone]
          , uiAdditionalParams = [RP "dir"]
          , uiFunc = cwdCmd
          }
      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

versionCmd :: Dispatcher
versionCmd _ = return (IdeResponseOk (String $ T.pack version))

pluginsCmd :: Dispatcher
pluginsCmd _ = do
  plugins <- getPlugins
  return (IdeResponseOk (String $ T.pack $ show $ Map.keys plugins))

commandsCmd :: Dispatcher
commandsCmd req = do
  plugins <- getPlugins
  case Map.lookup "plugin" (ideParams req) of
    Nothing -> return (IdeResponseFail (String $ T.pack $ "need 'plugin' parameter"))
    Just p -> case Map.lookup p plugins of
      Nothing -> return (IdeResponseFail (String $ T.pack $ "Can't find plugin:'"++ p ++ "'"))
      Just pl -> return (IdeResponseOk (String $ T.pack $ intercalate "," $ map uiCmdName $ pdUiCommands pl))

pwdCmd :: Dispatcher
pwdCmd _ = do
  dir <- liftIO $ getCurrentDirectory
  return (IdeResponseOk (String $ T.pack dir))

cwdCmd :: Dispatcher
cwdCmd req = do
  case Map.lookup "dir" (ideParams req) of
    Nothing -> return (IdeResponseFail (String $ T.pack $ "need 'dir' parameter"))
    Just dir -> do
      liftIO $ setCurrentDirectory dir
      return (IdeResponseOk Null)

-- ---------------------------------------------------------------------

version :: String
version =
    let commitCount = $gitCommitCount
    in  concat $ concat
            [ [$(simpleVersion Meta.version)]
              -- Leave out number of commits for --depth=1 clone
              -- See https://github.com/commercialhaskell/stack/issues/792
            , [" (" ++ commitCount ++ " commits)" | commitCount /= ("1"::String) &&
                                                    commitCount /= ("UNKNOWN" :: String)]
            , [" ", display buildArch]
            ]

-- ---------------------------------------------------------------------

replPluginInfo :: Plugins -> Map.Map String (String,UiCommand)
replPluginInfo plugins = Map.fromList commands
  where
    commands = concatMap extractCommands $ Map.toList plugins
    extractCommands (pluginName,descriptor) = cmds
      where
        cmds = map (\uic -> (pluginName ++ ":" ++ (uiCmdName uic),(pluginName,uic))) $ pdUiCommands descriptor

-- ---------------------------------------------------------------------
