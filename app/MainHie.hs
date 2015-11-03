{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent
import           Control.Exception
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
  putStrLn $ "run entered"
  cin <- newChan :: IO (Chan ChannelRequest)

  -- putStrLn $ "run:calling go"
  -- forkIO go2
  -- r <- go
  -- putStrLn $ "run:go returned " ++ show r

  forkIO (runner cin)
  -- Can have multiple listeners, each using a different transport protocol, so
  -- long as they can pass through a ChannelRequest
  -- forkIO (listener cin)
  if (optRepl opts)
     then stdioListener cin
     else jsonStdioTransport cin

runner cin = do
  forever $ do
    -- putStrLn $ "run:top of loop"
    req <- readChan cin
    timeNow <- getCurrentTime
    -- putStrLn $ "main loop:got:" ++ show req
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


stdioListener :: Chan ChannelRequest -> IO ()
stdioListener cin = do
  cout <- newChan :: IO (Chan ChannelResponse)
  hSetBuffering stdout NoBuffering
  putStrLn $ "HIE version : " ++ version
  let
    prompt = "HIE> "
    loop cid = do
      putStr prompt
      cmdArg <- getLine
      -- This command parsing should be built up from the PluginDescriptors
      let
        req = case dropWhileEnd isSpace cmdArg of
          "hello"   -> Right $ ("eg2", IdeRequest "sayHello" NoSession NoContext Map.empty)
          "version" -> Right $ ("base",IdeRequest "version"  NoSession NoContext Map.empty)
          "plugins" -> Right $ ("base",IdeRequest "plugins"  NoSession NoContext Map.empty)
          cmd     -> Left $ "unrecognised command:" ++ cmd
      case req of
        Left err -> putStrLn err
        Right (plugin,req) -> do
          writeChan cin (CReq plugin cid req cout)
          rsp <- readChan cout
          putStrLn $ show (coutResp rsp)
      loop (cid + 1)
  loop 1

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
baseDispatcher (IdeRequest name session ctx params) = do
  case name of
    "version"   -> return (IdeResponseOk (String $ T.pack version))
    "plugins"   -> return (IdeResponseOk (String $ T.pack $ show $ Map.keys plugins))
    -- "command"   -> return (IdeResponseOk (Map.keys plugins))

