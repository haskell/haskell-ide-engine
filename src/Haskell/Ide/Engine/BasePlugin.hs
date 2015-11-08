{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.Engine.BasePlugin where

import           Control.Monad
import           Data.Aeson
import           Data.Foldable
import           Data.List
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Text as T
import           Data.Vinyl
import           Development.GitRev (gitCommitCount)
import           Distribution.System (buildArch)
import           Distribution.Text (display)
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Options.Applicative.Simple (simpleVersion)
import qualified Paths_haskell_ide_engine as Meta
import           Prelude hiding (log)

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
                        , cmdFileExtensions = []
                        , cmdContexts = [CtxNone]
                        , cmdAdditionalParams = []
                        }
          , cmdFunc = versionCmd
          }
      , Command
          { cmdDesc = CommandDesc
                        { cmdName = "plugins"
                        , cmdUiDescription = "list available plugins"
                        , cmdFileExtensions = []
                        , cmdContexts = [CtxNone]
                        , cmdAdditionalParams = []
                        }
          , cmdFunc = pluginsCmd
          }
      , Command
          { cmdDesc = CommandDesc
                        { cmdName = "commands"
                        , cmdUiDescription = "list available commands for a given plugin"
                        , cmdFileExtensions = []
                        , cmdContexts = [CtxNone]
                        , cmdAdditionalParams = [RP "plugin" "the plugin name" PtText]
                        }
          , cmdFunc = commandsCmd
          }
      , Command
          { cmdDesc = CommandDesc
                        { cmdName = "commandDetail"
                        , cmdUiDescription = "list parameters required for a given command"
                        , cmdFileExtensions = []
                        , cmdContexts = [CtxNone]
                        , cmdAdditionalParams = [RP "plugin"  "the plugin name"  PtText
                                                ,RP "command" "the command name" PtText]
                        }
          , cmdFunc = commandDetailCmd
          }
      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

versionCmd :: CommandFunc
versionCmd _ _ = return (IdeResponseOk (String $ T.pack version))

pluginsCmd :: CommandFunc
pluginsCmd _ _ = do
  plugins <- getPlugins
  return (IdeResponseOk (toJSON $ Map.keys plugins))

commandsCmd :: CommandFunc
commandsCmd _ req = do
  plugins <- getPlugins
  -- TODO: Use Maybe Monad. What abut error reporting?
  case Map.lookup "plugin" (ideParams req) of
    Nothing -> return (missingParameter "plugin")
    Just (ParamTextP p) -> case Map.lookup p plugins of
      Nothing -> return (IdeResponseFail (IdeError
                  UnknownPlugin ("Can't find plugin:" <> p )
                  (Just $ toJSON $ p)))
      Just pl -> return (IdeResponseOk (toJSON $ map (cmdName . cmdDesc) $ pdCommands pl))
    Just x -> return $ incorrectParameter "plugin" ("ParamText"::String) x

commandDetailCmd :: CommandFunc
commandDetailCmd _ req = do
  plugins <- getPlugins
  case getParams (IdText "plugin" :& IdText "command" :& RNil) req of
    Left err -> return err
    Right (ParamText p :& ParamText command :& RNil) -> do
      case Map.lookup p plugins of
        Nothing -> return (IdeResponseError (IdeError
                    UnknownPlugin ("Can't find plugin:" <> p )
                    (Just $ toJSON $ p)))
        Just pl -> case find (\cmd -> command == (cmdName $ cmdDesc cmd) ) (pdCommands pl) of
          Nothing -> return (IdeResponseError (IdeError
                      UnknownCommand ("Can't find command:" <> command )
                      (Just $ toJSON $ command)))
          Just detail -> return (IdeResponseOk (toJSON (cmdDesc detail)))
    Right _ -> return (IdeResponseError (IdeError
                InternalError "commandDetailCmd: ghc’s exhaustiveness checker is broken" Nothing))

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
