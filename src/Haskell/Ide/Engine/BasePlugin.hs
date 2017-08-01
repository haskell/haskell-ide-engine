{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
module Haskell.Ide.Engine.BasePlugin where

import           Control.Monad
import           Data.Aeson
import           Data.Foldable
import           Data.List
import qualified Data.Map                        as Map
import           Data.Monoid
import qualified Data.Text                       as T
import           Development.GitRev              (gitCommitCount)
import           Distribution.System             (buildArch)
import           Distribution.Text               (display)
import           Haskell.Ide.Engine.IdeFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Options.Applicative.Simple      (simpleVersion)
import qualified Paths_haskell_ide_engine        as Meta
import           Prelude                         hiding (log)

-- ---------------------------------------------------------------------

baseDescriptor :: PluginDescriptor
baseDescriptor = PluginDescriptor
  {
    pluginName = "HIE Base"
  , pluginDesc = "Commands for HIE itself"
  , pluginCommands =
      [ PluginCommand "version" "return HIE version" versionCmd
      , PluginCommand "plugins" "list available plugins" pluginsCmd
      , PluginCommand "commands" "list available commands for a given plugin" commandsCmd
      , PluginCommand "commandDetail" "list parameters required for a given command" commandDetailCmd
      ]
  }

-- ---------------------------------------------------------------------

versionCmd :: CommandFunc () T.Text
versionCmd = CmdSync $ \_ -> return $ IdeResponseOk (T.pack version)

pluginsCmd :: CommandFunc () IdePlugins
pluginsCmd = CmdSync $ \_ ->
  IdeResponseOk <$> getPlugins

commandsCmd :: CommandFunc T.Text [CommandName]
commandsCmd = CmdSync $ \p -> do
  IdePlugins plugins <- getPlugins
  case Map.lookup p plugins of
    Nothing -> return $ IdeResponseFail $ IdeError
      { ideCode = UnknownPlugin
      , ideMessage = "Can't find plugin:" <> p
      , ideInfo = toJSON p
      }
    Just pl -> return $ IdeResponseOk $ map commandName pl

commandDetailCmd :: CommandFunc (T.Text, T.Text) T.Text
commandDetailCmd = CmdSync $ \(p,command) -> do
  IdePlugins plugins <- getPlugins
  case Map.lookup p plugins of
    Nothing -> return $ IdeResponseError $ IdeError
      { ideCode = UnknownPlugin
      , ideMessage = "Can't find plugin:" <> p
      , ideInfo = toJSON p
      }
    Just pl -> case find (\cmd -> command == (commandName cmd) ) pl of
      Nothing -> return $ IdeResponseError $ IdeError
        { ideCode = UnknownCommand
        , ideMessage = "Can't find command:" <> command
        , ideInfo = toJSON command
        }
      Just detail -> return $ IdeResponseOk (commandDesc detail)

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
