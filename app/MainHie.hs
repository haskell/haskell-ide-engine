{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes   #-}
module Main where

import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.STM
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import           Data.Version                          (showVersion)
import qualified GhcMod.Types                          as GM
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Options
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Transport.LspStdio
import           Haskell.Ide.Engine.Transport.JsonStdio
import qualified Language.Haskell.LSP.Core             as Core
import           Options.Applicative.Simple
import qualified Paths_haskell_ide_engine              as Meta
import           System.Directory
import           System.Environment
import qualified System.Log.Logger                     as L
import qualified System.Remote.Monitoring              as EKG

-- ---------------------------------------------------------------------
-- plugins

import           Haskell.Ide.Engine.Plugin.ApplyRefact
import           Haskell.Ide.Engine.Plugin.Brittany
import           Haskell.Ide.Engine.Plugin.Build
import           Haskell.Ide.Engine.Plugin.Base
import           Haskell.Ide.Engine.Plugin.Example2
import           Haskell.Ide.Engine.Plugin.GhcMod
import           Haskell.Ide.Engine.Plugin.HaRe
import           Haskell.Ide.Engine.Plugin.Hoogle

-- ---------------------------------------------------------------------

-- | This will be read from a configuration, eventually
plugins :: IdePlugins
plugins = pluginDescToIdePlugins
  [("applyrefact", applyRefactDescriptor)
  ,("brittany"   , brittanyDescriptor)
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
        compiler :: Parser (a -> a)
        compiler =
            infoOption
                hieCompilerVersion
                (long "compiler" <>
                 help "Show only compiler and version supported")
    -- Parse the options and run
    (global, ()) <-
        simpleOptions
            version
            "haskell-ide-engine - Provide a common engine to power any Haskell IDE"
            ""
            (numericVersion <*> compiler <*> globalOptsParser)
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

  when (optEkg opts) $ do
    logm $ "Launching EKG server on port " ++ show (optEkgPort opts)
    void $ EKG.forkServer "localhost" (optEkgPort opts) >> return ()

  origDir <- getCurrentDirectory

  maybe (pure ()) setCurrentDirectory $ projectRoot opts

  progName <- getProgName
  logm $  "run entered for HIE(" ++ progName ++ ") " ++ version
  d <- getCurrentDirectory
  logm $ "Current directory:" ++ d

  let vomitOptions = GM.defaultOptions { GM.optOutput = oo { GM.ooptLogLevel = GM.GmVomit}}
      oo = GM.optOutput GM.defaultOptions
  let ghcModOptions = if optGhcModVomit opts then vomitOptions else GM.defaultOptions

  when (optGhcModVomit opts) $ do
    logm "Enabling --vomit for ghc-mod. Output will be on stderr"
  
  -- launch the dispatcher.
  if optLsp opts then do
    pin <- atomically newTChan
    lspStdioTransport (dispatcherP pin plugins ghcModOptions) pin origDir
  else do
    pin <- atomically newTChan
    jsonStdioTransport (dispatcherP pin plugins ghcModOptions) pin
