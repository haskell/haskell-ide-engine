{-# LANGUAGE DataKinds #-}
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
import           Development.GitRev (gitCommitCount)
import           Distribution.System (buildArch)
import           Distribution.Text (display)
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Options.Applicative.Simple (simpleVersion)
import qualified Paths_haskell_ide_engine as Meta
import           Prelude hiding (log)

-- ---------------------------------------------------------------------

baseDescriptor :: TaggedPluginDescriptor '["version","plugins","commands","commandDetail"]
baseDescriptor = PluginDescriptor
  {
    pdUIShortName = "HIE Base"
  , pdUIOverview = "Commands for HIE itself, "
  , pdCommands =

        buildCommand versionCmd Proxy "return HIE version"
                        [] [CtxNone] []

      :& buildCommand pluginsCmd Proxy "list available plugins"
                          [] [CtxNone] []

      :& buildCommand commandsCmd Proxy "list available commands for a given plugin"
                         [] [CtxNone] [RP "plugin" "the plugin name" PtText]

      :& buildCommand commandDetailCmd Proxy "list parameters required for a given command"
                         [] [CtxNone] [RP "plugin"  "the plugin name"  PtText
                                      ,RP "command" "the command name" PtText]

      :& RNil
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

versionCmd :: CommandFunc T.Text
versionCmd = CmdSync $ \_ _ -> return $ IdeResponseOk (T.pack version)

pluginsCmd :: CommandFunc IdePlugins
pluginsCmd = CmdSync $ \_ _ ->
  IdeResponseOk . IdePlugins . Map.map (map cmdDesc . pdCommands) <$> getPlugins

commandsCmd :: CommandFunc [CommandName]
commandsCmd = CmdSync $ \_ req -> do
  plugins <- getPlugins
  -- TODO: Use Maybe Monad. What abut error reporting?
  case Map.lookup "plugin" (ideParams req) of
    Nothing -> return (missingParameter "plugin")
    Just (ParamTextP p) ->
      case Map.lookup p plugins of
        Nothing -> return $ IdeResponseFail $ IdeError
          { ideCode = UnknownPlugin
          , ideMessage = "Can't find plugin:" <> p
          , ideInfo = Just $ toJSON p
          }
        Just pl -> return $ IdeResponseOk $ map (cmdName . cmdDesc) (pdCommands pl)
    Just x -> return $ incorrectParameter "plugin" ("ParamText"::String) x

commandDetailCmd :: CommandFunc ExtendedCommandDescriptor
commandDetailCmd = CmdSync $ \_ req -> do
  plugins <- getPlugins
  case getParams (IdText "plugin" :& IdText "command" :& RNil) req of
    Left err -> return err
    Right (ParamText p :& ParamText command :& RNil) -> do
      case Map.lookup p plugins of
        Nothing -> return $ IdeResponseError $ IdeError
          { ideCode = UnknownPlugin
          , ideMessage = "Can't find plugin:" <> p
          , ideInfo = Just $ toJSON p
          }
        Just pl -> case find (\cmd -> command == (cmdName $ cmdDesc cmd) ) (pdCommands pl) of
          Nothing -> return $ IdeResponseError $ IdeError
            { ideCode = UnknownCommand
            , ideMessage = "Can't find command:" <> command
            , ideInfo = Just $ toJSON command
            }
          Just detail -> return $ IdeResponseOk (ExtendedCommandDescriptor (cmdDesc detail) p)
    Right _ -> return $ IdeResponseError $ IdeError
      { ideCode = InternalError
      , ideMessage = "commandDetailCmd: ghc’s exhaustiveness checker is broken"
      , ideInfo = Nothing
      }

-- ---------------------------------------------------------------------

version :: String
version =
  let commitCount = $gitCommitCount
  in concat $ concat
    [ [$(simpleVersion Meta.version)]
      -- Leave out number of commits for --depth=1 clone
      -- See https://github.com/commercialhaskell/stack/issues/792
    , [" (" ++ commitCount ++ " commits)" | commitCount /= ("1"::String) &&
                                            commitCount /= ("UNKNOWN" :: String)]
    , [" ", display buildArch] ]

-- ---------------------------------------------------------------------

replPluginInfo :: Plugins -> Map.Map T.Text (T.Text,Command)
replPluginInfo plugins = Map.fromList commands
  where
    commands = concatMap extractCommands $ Map.toList plugins
    extractCommands (pluginName,descriptor) = cmds
      where
        cmds = map (\cmd -> (pluginName <> ":" <> (cmdName $ cmdDesc cmd),(pluginName,cmd))) $ pdCommands descriptor

-- ---------------------------------------------------------------------
