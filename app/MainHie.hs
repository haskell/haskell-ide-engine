{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.STM
import           Data.Semigroup
import qualified Data.Map as Map
import           Data.Version                          (showVersion)
import qualified GhcMod.Types                          as GM
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Options
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Transport.LspStdio
import           Haskell.Ide.Engine.Transport.JsonStdio
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Core             as Core
import           Options.Applicative.Simple
import qualified Paths_haskell_ide_engine              as Meta
import           System.Directory
import qualified System.Log.Logger                     as L
import qualified System.Remote.Monitoring              as EKG

-- ---------------------------------------------------------------------
-- plugins

import           Haskell.Ide.ApplyRefactPlugin
import           Haskell.Ide.BuildPlugin
import           Haskell.Ide.Engine.BasePlugin
import           Haskell.Ide.ExamplePlugin2
import           Haskell.Ide.GhcModPlugin
import           Haskell.Ide.HaRePlugin
import           Haskell.Ide.HooglePlugin

-- ---------------------------------------------------------------------

-- | This will be read from a configuration, eventually
plugins :: IdePlugins
plugins = pluginDescToIdePlugins
  [("applyrefact", applyRefactDescriptor)
  ,("build"      , buildPluginDescriptor)
  ,("eg2"        , example2Descriptor)
  ,("ghcmod"     , ghcmodDescriptor)
  ,("hare"       , hareDescriptor)
  ,("base"       , baseDescriptor)
  ,("hoogle"     , hoogleDescriptor)
  ]

-- ---------------------------------------------------------------------

main :: IO ()
main = do
    let
        numericVersion :: Parser (a -> a)
        numericVersion =
            infoOption
                (showVersion Meta.version)
                (long "numeric-version" <>
                 help "Show only version number")
    -- Parse the options and run
    (global, ()) <-
        simpleOptions
            version
            "haskell-ide-engine - Provide a common engine to power any Haskell IDE"
            ""
            (numericVersion <*> globalOptsParser)
            empty

    run global

-- ---------------------------------------------------------------------

run :: GlobalOpts -> IO ()
run opts = do
  let mLogFileName = case optLogFile opts of
        Just f  -> Just f
        Nothing -> Nothing

      logLevel = if optDebugOn opts
                   then L.DEBUG
                   else L.INFO

  Core.setupLogger mLogFileName ["hie"] logLevel

  when (optEkg opts) $ EKG.forkServer "localhost" 8000 >> return ()

  origDir <- getCurrentDirectory

  case projectRoot opts of
    Nothing -> do
      h <- getHomeDirectory
      setCurrentDirectory h
      logm $ "Setting home directory:" ++ h
    Just root -> setCurrentDirectory root

  logm $  "run entered for HIE " ++ version
  d <- getCurrentDirectory
  logm $ "Current directory:" ++ d

  pin <- atomically newTChan :: IO (TChan PluginRequest)

  let vomitOptions = GM.defaultOptions { GM.optOutput = oo { GM.ooptLogLevel = GM.GmVomit}}
      oo = GM.optOutput GM.defaultOptions
  let ghcModOptions = if optGhcModVomit opts then vomitOptions else GM.defaultOptions

  -- launch the dispatcher.
  let dispatcherProcP dispatcherEnv =
        void $ runIdeGhcM ghcModOptions
            (IdeState emptyModuleCache plugins Map.empty Nothing)
            (dispatcherP dispatcherEnv pin)

  if optLsp opts then
    lspStdioTransport dispatcherProcP pin origDir
  else
    jsonStdioTransport dispatcherProcP pin
