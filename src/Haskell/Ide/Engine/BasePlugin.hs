{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.BasePlugin where

import           Control.Concurrent
import           Control.Exception
import           Control.Logging
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Char
import           Data.Foldable
import           Data.IORef
import           Data.List
import           Data.Traversable
import qualified Data.Text as T
import           Data.Version (showVersion)
import           Development.GitRev (gitCommitCount)
import           Distribution.System (buildArch)
import           Distribution.Text (display)
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.Options
import           Haskell.Ide.Engine.Plugin
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Transport.JsonStdio
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.GhcMod.LightGhc as GM
import qualified Language.Haskell.GhcMod.Monad as GM
import qualified Language.Haskell.GhcMod.Types as GM
import           Module (mkModuleName)
import           Options.Applicative.Simple
import qualified Data.Map as Map
import qualified Paths_haskell_ide_engine as Meta
import           Data.Time
import           System.IO
import           Prelude hiding (log)

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
          }
      , UiCommand
          { uiCmdName = "plugins"
          , uiContexts = [CtxNone]
          , uiAdditionalParams = []
          }
      , UiCommand
          { uiCmdName = "commands"
          , uiContexts = [CtxNone]
          , uiAdditionalParams = [RP "plugin"]
          }
      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

baseDispatcher :: Plugins -> Dispatcher
baseDispatcher plugins r@(IdeRequest name ctx params) = do
  debug $ T.pack $ "baseDispatcher:got " ++ show r
  case name of
    "version"   -> return (IdeResponseOk (String $ T.pack version))
    "plugins"   -> return (IdeResponseOk (String $ T.pack $ show $ Map.keys plugins))
    "commands"  -> case Map.lookup "plugin" params of
                     Nothing -> return (IdeResponseFail (String $ T.pack $ "need 'plugin' parameter"))
                     Just p -> case Map.lookup p plugins of
                       Nothing -> return (IdeResponseFail (String $ T.pack $ "Can't find plugin:'"++ p ++ "'"))
                       Just (PluginReg pl _) -> return (IdeResponseOk (String $ T.pack $ intercalate "," $ map uiCmdName $ pdUiCommands pl))
    _           -> return (IdeResponseFail (String $ T.pack $ "command \"" ++ name ++ "\" not recognised"))

-- ---------------------------------------------------------------------

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
    extractCommands (pluginName,PluginReg descriptor _) = cmds
      where
        cmds = map (\uic -> (pluginName ++ ":" ++ (uiCmdName uic),(pluginName,uic))) $ pdUiCommands descriptor

-- ---------------------------------------------------------------------
