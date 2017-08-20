{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.STM
import           Data.Semigroup
import           Data.Version                          (showVersion)
import           Development.GitRev                    (gitCommitCount)
import           Distribution.System                   (buildArch)
import           Distribution.Text                     (display)
import qualified GhcMod.ModuleLoader                   as GM
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

-- ---------------------------------------------------------------------
-- plugins

import           Haskell.Ide.ApplyRefactPlugin
import           Haskell.Ide.BuildPlugin
import           Haskell.Ide.Engine.BasePlugin
import           Haskell.Ide.ExamplePlugin2
import           Haskell.Ide.ExamplePluginAsync
import           Haskell.Ide.GhcModPlugin
import           Haskell.Ide.GhcTreePlugin
import           Haskell.Ide.HaRePlugin
import           Haskell.Ide.HooglePlugin

-- ---------------------------------------------------------------------

-- | This will be read from a configuration, eventually
plugins :: IdePlugins
plugins = pluginDescToIdePlugins
  [("applyrefact", applyRefactDescriptor)
  ,("build"      , buildPluginDescriptor)
  ,("eg2"        , example2Descriptor)
  ,("egasync"    , exampleAsyncDescriptor)
  ,("ghcmod"     , ghcmodDescriptor)
  ,("ghctree"    , ghcTreeDescriptor)
  ,("hare"       , hareDescriptor)
  ,("base"       , baseDescriptor)
  ,("hoogle"     , hoogleDescriptor)
  ]

-- ---------------------------------------------------------------------

main :: IO ()
main = do
    -- Version code from stack. Maybe move this stuff into optparse-simple ?
    let commitCount = $gitCommitCount
        versionString' = concat $ concat
            [ [$(simpleVersion Meta.version)]
              -- Leave out number of commits for --depth=1 clone
              -- See https://github.com/commercialhaskell/stack/issues/792
            , [" (" ++ commitCount ++ " commits)" | commitCount /= ("1"::String) &&
                                                    commitCount /= ("UNKNOWN" :: String)]
            , [" ", display buildArch]
            ]
        numericVersion :: Parser (a -> a)
        numericVersion =
            infoOption
                (showVersion Meta.version)
                (long "numeric-version" <>
                 help "Show only version number")
    -- Parse the options and run
    (global, ()) <-
        simpleOptions
            versionString'
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
  let ghcModOptions = (if optGhcModVomit opts then vomitOptions else GM.defaultOptions) { GM.optGhcUserOptions = ["-Wall"]  }

  -- launch the dispatcher.
  let dispatcherProcP dispatcherEnv =
        void $ runIdeM ghcModOptions
            (IdeState plugins GM.emptyModuleCache )
            (dispatcherP dispatcherEnv pin)

  if optLsp opts then
    lspStdioTransport dispatcherProcP pin origDir
  else
    jsonStdioTransport dispatcherProcP pin
