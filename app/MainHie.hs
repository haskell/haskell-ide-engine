{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

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
import           Haskell.Ide.Engine.REPL
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
-- plugins

import Haskell.Ide.ExamplePlugin2

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

plugins :: Plugins
plugins = Map.fromList
  [
    -- Note: statically including known plugins. In future this map could be set
    -- up via a config file of some kind.
    ("eg2", PluginReg example2Descriptor example2Dispatcher)
    -- The base plugin, able to answer questions about the IDE Engine environment.
  , ("base", PluginReg baseDescriptor baseDispatcher)
  ]

-- ---------------------------------------------------------------------

run :: GlobalOpts -> IO ()
run opts = do
  let withLogFun = case optLogFile opts of
        Just f -> withFileLogging f
        Nothing -> withStdoutLogging

  withLogFun $ do
    if optDebugOn opts
      then setLogLevel LevelDebug
      else setLogLevel LevelError

    log $ T.pack $ "run entered for HIE " ++ version
    cin <- newChan :: IO (Chan ChannelRequest)

    -- log $ T.pack $ "replPluginInfo:" ++ show replPluginInfo

    -- launch the dispatcher.
    forkIO (dispatcher cin)

    -- Can have multiple listeners, each using a different transport protocol, so
    -- long as they can pass through a ChannelRequest
    if (optRepl opts)
       then replListener plugins cin
       else jsonStdioTransport cin

    -- At least one needs to be launched, othewise a threadDelay with a large
    -- number should be given. Or some other waiting action.

-- ---------------------------------------------------------------------

dispatcher cin = do
  forever $ do
    debug "run:top of loop"
    req <- readChan cin
    timeNow <- getCurrentTime
    debug $ T.pack $ "main loop:got:" ++ show req
    r <- case Map.lookup (cinPlugin req) plugins of
      Nothing -> return (IdeResponseError (String $ T.pack $ "No plugin found for:" ++ cinPlugin req ))
      Just (PluginReg desc disp) -> disp (cinReq req)
    let cr = CResp (cinPlugin req) (cinReqId req) r
    timeEnd <- getCurrentTime
    writeChan (cinReplyChan req) cr

-- ---------------------------------------------------------------------

-- |Do whatever it takes to get a request from the IDE.
-- pass the request through to the main event dispatcher, and listen on the
-- reply channel for the response, which should go back to the IDE, using
-- whatever it takes.
listener :: Chan ChannelRequest -> IO ()
listener = assert False undefined

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

baseDispatcher :: Dispatcher
baseDispatcher (IdeRequest name ctx params) = do
  case name of
    "version"   -> return (IdeResponseOk (String $ T.pack version))
    "plugins"   -> return (IdeResponseOk (String $ T.pack $ show $ Map.keys plugins))
    -- "command"   -> return (IdeResponseOk (Map.keys plugins))

