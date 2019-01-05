{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
module Haskell.Ide.Engine.Plugin.Base where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Foldable
import qualified Data.Map                        as Map
import           Data.Maybe
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import qualified Data.Text                       as T
import           Development.GitRev              (gitCommitCount)
import           Distribution.System             (buildArch)
import           Distribution.Text               (display)
import           Haskell.Ide.Engine.MonadTypes
import           Options.Applicative.Simple      (simpleVersion)
import qualified Paths_haskell_ide_engine        as Meta

import           System.Directory
import           System.Info
import           System.Process
import qualified System.Log.Logger as L

-- ---------------------------------------------------------------------

baseDescriptor :: PluginId -> PluginDescriptor
baseDescriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginName = "HIE Base"
  , pluginDesc = "Commands for HIE itself"
  , pluginCommands =
      [ PluginCommand "version" "return HIE version" versionCmd
      , PluginCommand "plugins" "list available plugins" pluginsCmd
      , PluginCommand "commands" "list available commands for a given plugin" commandsCmd
      , PluginCommand "commandDetail" "list parameters required for a given command" commandDetailCmd
      ]
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  }

-- ---------------------------------------------------------------------

versionCmd :: CommandFunc () T.Text
versionCmd = CmdSync $ \_ -> return $ IdeResultOk (T.pack version)

pluginsCmd :: CommandFunc () IdePlugins
pluginsCmd = CmdSync $ \_ ->
  IdeResultOk <$> getPlugins

commandsCmd :: CommandFunc T.Text [CommandName]
commandsCmd = CmdSync $ \p -> do
  IdePlugins plugins <- getPlugins
  case Map.lookup p plugins of
    Nothing -> return $ IdeResultFail $ IdeError
      { ideCode = UnknownPlugin
      , ideMessage = "Can't find plugin:" <> p
      , ideInfo = toJSON p
      }
    Just pl -> return $ IdeResultOk $ map commandName $ pluginCommands pl

commandDetailCmd :: CommandFunc (T.Text, T.Text) T.Text
commandDetailCmd = CmdSync $ \(p,command) -> do
  IdePlugins plugins <- getPlugins
  case Map.lookup p plugins of
    Nothing -> return $ IdeResultFail $ IdeError
      { ideCode = UnknownPlugin
      , ideMessage = "Can't find plugin:" <> p
      , ideInfo = toJSON p
      }
    Just pl -> case find (\cmd -> command == commandName cmd) (pluginCommands pl) of
      Nothing -> return $ IdeResultFail $ IdeError
        { ideCode = UnknownCommand
        , ideMessage = "Can't find command:" <> command
        , ideInfo = toJSON command
        }
      Just detail -> return $ IdeResultOk (commandDesc detail)

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
    , [" ", display buildArch]
    , [" ", hieGhcDisplayVersion]
    ]

-- ---------------------------------------------------------------------

hieGhcDisplayVersion :: String
hieGhcDisplayVersion = compilerName ++ "-" ++ VERSION_ghc

getProjectGhcVersion :: IO String
getProjectGhcVersion =
  onException
    (asum [tryStackProject, tryCabalProject, trySystemGhc])
    (error "Couldn't find GHC version through stack, cabal, nor system GHC.")

  where
    tryStackProject = do
      isStackProject   <- doesFileExist "stack.yaml"
      isStackInstalled <- isJust <$> findExecutable "stack"
      guard $ isStackProject && isStackInstalled
      L.infoM "hie" "Trying stack GHC version"
      tryCommand "stack ghc -- --numeric-version"

    tryCabalProject = do
      isCabalProject <- doesFileExist "cabal.project"
      isCabalInstalled <- isJust <$> findExecutable "cabal"
      guard $ isCabalProject && isCabalInstalled
      L.infoM "hie" "Trying cabal GHC version"
      tryCommand "cabal v2-exec ghc -- --numeric-version"

    trySystemGhc = do
      isGhcInstalled   <- isJust <$> findExecutable "ghc"
      guard isGhcInstalled
      L.infoM "hie" "Trying system GHC version"
      tryCommand "ghc --numeric-version"

    tryCommand cmd =
      init <$> readCreateProcess (shell cmd) ""

hieGhcVersion :: String
hieGhcVersion = VERSION_ghc

-- ---------------------------------------------------------------------

checkCabalInstall :: IO Bool
checkCabalInstall = isJust <$> findExecutable "cabal"

-- ---------------------------------------------------------------------
