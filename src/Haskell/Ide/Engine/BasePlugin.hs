{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.BasePlugin where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Foldable
import           Data.List
import           Data.Monoid
import qualified Data.Text as T
import           Development.GitRev (gitCommitCount)
import           Distribution.System (buildArch)
import           Distribution.Text (display)
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Options.Applicative.Simple (simpleVersion)
import qualified Data.Map as Map
import qualified Paths_haskell_ide_engine as Meta
import           Prelude hiding (log)
import           System.Directory

-- ---------------------------------------------------------------------

baseDescriptor :: PluginDescriptor
baseDescriptor = PluginDescriptor
  {
    pdCommands =
      [
        Command
          { cmdDesc = CommandDesc
                        { cmdName = "version"
                        , cmdUiDescription = "return HIE version"
                        , cmdContexts = [CtxNone]
                        , cmdAdditionalParams = []
                        }
          , cmdFunc = versionCmd
          }
      , Command
          { cmdDesc = CommandDesc
                        { cmdName = "plugins"
                        , cmdUiDescription = "list available plugins"
                        , cmdContexts = [CtxNone]
                        , cmdAdditionalParams = []
                        }
          , cmdFunc = pluginsCmd
          }
      , Command
          { cmdDesc = CommandDesc
                        { cmdName = "commands"
                        , cmdUiDescription = "list available commands for a given plugin"
                        , cmdContexts = [CtxNone]
                        , cmdAdditionalParams = [RP "plugin"]
                        }
          , cmdFunc = commandsCmd
          }
      , Command
          { cmdDesc = CommandDesc
                        { cmdName = "commandDetail"
                        , cmdUiDescription = "list parameters required for a given command"
                        , cmdContexts = [CtxNone]
                        , cmdAdditionalParams = [RP "plugin",RP "command"]
                        }
          , cmdFunc = commandDetailCmd
          }
      , Command
          { cmdDesc = CommandDesc
                        { cmdName = "pwd"
                        , cmdUiDescription = "return the current working directory for the HIE process"
                        , cmdContexts = [CtxNone]
                        , cmdAdditionalParams = []
                        }
          , cmdFunc = pwdCmd
          }
      , Command
          { cmdDesc = CommandDesc
                        { cmdName = "cwd"
                        , cmdUiDescription = "change the current working directory for the HIE process"
                        , cmdContexts = [CtxNone]
                        , cmdAdditionalParams = [RP "dir"]
                       }
          , cmdFunc = cwdCmd
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
  -- TODO: Use Maybe Monad. What abut error reporting?
  case Map.lookup "plugin" (ideParams req) of
    Nothing -> return (IdeResponseFail (toJSON $ T.pack "need 'plugin' parameter"))
    Just p -> case Map.lookup p plugins of
      Nothing -> return (IdeResponseFail (toJSON $ "Can't find plugin:'" <> p <> "'"))
      Just pl -> return (IdeResponseOk (toJSON $ map (cmdName . cmdDesc) $ pdCommands pl))

commandDetailCmd :: Dispatcher
commandDetailCmd req = do
  plugins <- getPlugins
  case getParams ["plugin","command"] req of
    Left err -> return err
    Right [p,command] -> do
      case Map.lookup p plugins of
        Nothing -> return (IdeResponseFail (toJSON $ "Can't find plugin:'" <> p <> "'"))
        Just pl -> case find (\cmd -> command == (cmdName $ cmdDesc cmd) ) (pdCommands pl) of
          Nothing -> return (IdeResponseFail (toJSON $ "Can't find command:'" <> command <> "'"))
          Just detail -> return (IdeResponseOk (toJSON (cmdDesc detail)))
    Right _ -> error $ "commandDetailCmd:should not be possible"


pwdCmd :: Dispatcher
pwdCmd _ = do
  dir <- liftIO $ getCurrentDirectory
  return (IdeResponseOk (String $ T.pack dir))

cwdCmd :: Dispatcher
cwdCmd req = do
  case Map.lookup "dir" (ideParams req) of
    Nothing -> return (IdeResponseFail (String "need 'dir' parameter"))
    Just dir -> do
      liftIO $ setCurrentDirectory (T.unpack dir)
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

replPluginInfo :: Plugins -> Map.Map T.Text (T.Text,Command)
replPluginInfo plugins = Map.fromList commands
  where
    commands = concatMap extractCommands $ Map.toList plugins
    extractCommands (pluginName,descriptor) = cmds
      where
        cmds = map (\cmd -> (pluginName <> ":" <> (cmdName $ cmdDesc cmd),(pluginName,cmd))) $ pdCommands descriptor

-- ---------------------------------------------------------------------
