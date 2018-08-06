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
import           Development.GitRev              (gitCommitCount)
import           Distribution.System             (buildArch)
import           Distribution.Text               (display)
import           Control.Lens
import           Haskell.Ide.Engine.MonadTypes
import           Options.Applicative.Simple      (simpleVersion)
import qualified Paths_haskell_ide_engine        as Meta

import           System.Directory
import           System.Info
import           System.Process
import qualified System.Log.Logger as L

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
  , pluginCodeActionProvider = noCodeActions
  }

-- ---------------------------------------------------------------------

versionCmd :: CommandFunc () T.Text
versionCmd = CmdSync $ \_ -> return $ T.pack version

pluginsCmd :: CommandFunc () IdePlugins
pluginsCmd = CmdSync $ \_ -> liftIde $ use idePlugins

commandsCmd :: CommandFunc T.Text [CommandName]
commandsCmd = CmdSync $ \p -> do
  IdePlugins plugins <- liftIde $ use idePlugins
  (cs, _) <- maybe (ideError UnknownPlugin  ("Can't find plugin:"  <> p      ) (toJSON p      )) pure $ Map.lookup p plugins
  return $ map commandName cs

commandDetailCmd :: CommandFunc (T.Text, T.Text) T.Text
commandDetailCmd = CmdSync $ \(p,command) -> do
  IdePlugins plugins <- liftIde $ use idePlugins
  (cs, _) <- maybe (ideError UnknownPlugin  ("Can't find plugin:"  <> p      ) (toJSON p      )) pure $ Map.lookup p plugins
  detail  <- maybe (ideError UnknownCommand ("Can't find command:" <> command) (toJSON command)) pure $ find (\cmd -> command == commandName cmd) cs
  pure $ commandDesc detail

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
getProjectGhcVersion = do
  isStackProject <- doesFileExist "stack.yaml"
  isStackInstalled <- isJust <$> findExecutable "stack"
  if isStackProject && isStackInstalled
    then do
      L.infoM "hie" "Using stack GHC version"
      catch (tryCommand "stack ghc -- --version") $ \e -> do
        L.errorM "hie" $ show (e :: SomeException)
        L.infoM "hie" "Couldn't find stack version, falling back to plain GHC"
        tryCommand "ghc --version"
    else do
      L.infoM "hie" "Using plain GHC version"
      tryCommand "ghc --version"

  where
    tryCommand cmd =
      crackGhcVersion <$> readCreateProcess (shell cmd) ""
    -- "The Glorious Glasgow Haskell Compilation System, version 8.4.3\n"
    -- "The Glorious Glasgow Haskell Compilation System, version 8.4.2\n"
    crackGhcVersion :: String -> String
    crackGhcVersion st = reverse $ takeWhile (/=' ') $ tail $ reverse st

hieGhcVersion :: String
hieGhcVersion = VERSION_ghc

-- ---------------------------------------------------------------------

checkCabalInstall :: IO Bool
checkCabalInstall = isJust <$> findExecutable "cabal"

-- ---------------------------------------------------------------------
