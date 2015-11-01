{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Foldable
import           Data.IORef
import           Data.List
import           Data.Traversable
import           Data.Version (showVersion)
import           Development.GitRev (gitCommitCount)
import           Distribution.System (buildArch)
import           Distribution.Text (display)
import           Haskell.Ide.Monad
import           Haskell.Ide.Options
import           Haskell.Ide.Plugin
import           Haskell.Ide.PluginDescriptor
import           Haskell.Ide.Types
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
            "haskell-ide - Provide a common engine to power any Haskell IDE"
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

data PluginReg = PluginReg PluginDescriptor Dispatcher
type PluginId = String

type Plugins = Map.Map PluginId PluginReg


plugins :: Plugins
plugins = Map.fromList
  [
    -- Note: statically including known plugins. In future this map could be set
    -- up via a config file of some kind.
    ("eg2", PluginReg example2Descriptor example2Dispatcher)
    -- The base plugin, able to answer questions about the IDE Engine environment.
  , ("base", PluginReg baseDescriptor baseDispatcher)
  ]

type RequestId = Int

data ChannelRequest = CReq
  { cinPlugin    :: PluginId
  , cinReqId     :: RequestId -- ^An identifier for the request, can tie back to
                            -- e.g. a promise id. It is returned with the
                            -- ChannelResponse.
  , cinReq       :: IdeRequest
  , cinReplyChan :: Chan ChannelResponse
  } deriving Show

instance Show (Chan ChannelResponse) where
  show _ = "(Chan ChannelResponse)"
 
data ChannelResponse = CResp
  { couPlugin :: PluginId
  , coutReqId :: RequestId
  , coutResp  :: IdeResponse
  } deriving Show

-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------

run :: t -> IO ()
run opts = do
  putStrLn $ "run entered"
  cin <- newChan :: IO (Chan ChannelRequest)

  -- Can have multiple listeners, each using a different transport protocol, so
  -- long as they can pass through a ChannelRequest
  -- forkIO (listener cin)
  forkIO (stdioListener cin)

  forever $ do
    -- putStrLn $ "run:top of loop"
    req <- readChan cin
    timeNow <- getCurrentTime
    -- putStrLn $ "main loop:got:" ++ show req
    r <- case Map.lookup (cinPlugin req) plugins of
      Nothing -> return (IdeResponseFail ("No plugin found for:" ++ cinPlugin req ))
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
  putStrLn $ "IE version : " ++ version
  let
    prompt = "IE> "
    loop cid = do
      putStr prompt
      cmdArg <- getLine
      let
        req = case dropWhileEnd isSpace cmdArg of
          "hello"   -> Right $ ("eg2",IdeRequest "sayHello" NoSession NoContext Map.empty)
          "version" -> Right $ ("base",IdeRequest "version" NoSession NoContext Map.empty)
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
      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

baseDispatcher :: Dispatcher
baseDispatcher (IdeRequest name session ctx params) = do
  case name of
    "version"   -> return (IdeResponseOk version)

-- ---------------------------------------------------------------------

startCmdOpts :: Parser [FilePath]
startCmdOpts =
    many (strArgument
        ( metavar "TARGET"
       <> help "Specify Haskell files to load"
        ))

startCmd :: [FilePath] -> GlobalOpts -> IO ()
startCmd targets opts = runIdeM $ do
    let pkgOpts
            | null (optPluginPackages opts) = []
            | otherwise =
                "-hide-all-packages" :
                concatMap (\pkg -> ["-package", pkg])
                          ("haskell-ide" : optPluginPackages opts)
    -- Load and initialize the plugins.
    liftIO $ putStrLn "Initializing plugins"
    _plugins <- GM.withLightHscEnv pkgOpts $ \hsc_env ->
        liftIO $ forM (optPluginModules opts) $ \mn -> do
            plugin <- loadPlugin hsc_env (mkModuleName mn)
            forM_ (initializeHook plugin) id
            return plugin
    liftIO $ putStrLn "Done initializing plugins"
    -- Load target filepaths specified on the CLI.
    setTargets (map Left targets)
