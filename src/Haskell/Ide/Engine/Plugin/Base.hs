{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
module Haskell.Ide.Engine.Plugin.Base where

import           Control.Exception
import           Data.Aeson
import           Data.Foldable
import qualified Data.Map                        as Map
import           Data.Maybe
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import qualified Data.Text                       as T
import qualified Data.Versions                   as V
import           Development.GitRev              (gitCommitCount)
import           Distribution.System             (buildArch)
import           Distribution.Text               (display)
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Cradle (isStackCradle)
import qualified HIE.Bios.Types as BIOS
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
  , pluginFormattingProvider = Nothing
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

getProjectGhcVersion :: BIOS.Cradle -> IO String
getProjectGhcVersion crdl = do
  isStackProject   <- doesFileExist "stack.yaml"
  isStackInstalled <- isJust <$> findExecutable "stack"
  if isStackCradle crdl && isStackProject && isStackInstalled
    then do
      L.infoM "hie" "Using stack GHC version"
      catch (tryCommand "stack ghc -- --numeric-version") $ \e -> do
        L.errorM "hie" $ show (e :: SomeException)
        L.infoM "hie" "Couldn't find stack version, falling back to plain GHC"
        getGhcVersion
    else do
      L.infoM "hie" "Using plain GHC version"
      getGhcVersion

  where
    getGhcVersion = do
      isGhcInstalled   <- isJust <$> findExecutable "ghc"
      if isGhcInstalled
        then tryCommand "ghc --numeric-version"
        else return "No System GHC found"


tryCommand :: String -> IO String
tryCommand cmd =
  init <$> readCreateProcess (shell cmd) ""

hieGhcVersion :: String
hieGhcVersion = VERSION_ghc

-- ---------------------------------------------------------------------

getStackVersion :: IO (Maybe V.Version)
getStackVersion = do
  isStackInstalled   <- isJust <$> findExecutable "stack"
  if isStackInstalled
    then do
      versionStr <- tryCommand "stack --numeric-version"
      case V.version (T.pack versionStr) of
        Left _err -> return Nothing
        Right v -> return (Just v)
    else return Nothing

stack193Version :: V.Version
stack193Version = case V.version "1.9.3" of
  Left err -> error $ "stack193Version:err=" ++ show err
  Right v -> v

-- ---------------------------------------------------------------------

checkCabalInstall :: IO Bool
checkCabalInstall = isJust <$> findExecutable "cabal"

-- ---------------------------------------------------------------------
