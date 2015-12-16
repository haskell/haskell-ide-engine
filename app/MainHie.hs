{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Exception
import           Control.Monad.Logger
import           Control.Monad
import           Control.Monad.STM
import           Control.Monad.Trans.Maybe
import qualified Data.Map as Map
import           Data.Version (showVersion)
import           Development.GitRev (gitCommitCount)
import           Distribution.System (buildArch)
import           Distribution.Text (display)
import           Haskell.Ide.Engine.Console
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.Options
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Utils
import           Haskell.Ide.Engine.Transport.JsonHttp
import           Haskell.Ide.Engine.Transport.JsonStdio
import           Haskell.Ide.Engine.Types
import           Options.Applicative.Simple
import qualified Paths_haskell_ide_engine as Meta
import           System.Directory
import           System.Environment

-- ---------------------------------------------------------------------
-- plugins

import           Haskell.Ide.ApplyRefactPlugin
import           Haskell.Ide.Engine.BasePlugin
import           Haskell.Ide.ExamplePlugin2
import           Haskell.Ide.GhcModPlugin
import           Haskell.Ide.HaRePlugin

-- ---------------------------------------------------------------------

-- | This will be read from a configuration, eventually
plugins :: Plugins
plugins = Map.fromList
  [
    -- Note: statically including known plugins. In future this map could be set
    -- up via a config file of some kind.
    ("applyrefact", applyRefactDescriptor)
  , ("eg2",         example2Descriptor)
  , ("ghcmod",      ghcmodDescriptor)
  , ("hare",        hareDescriptor)
    -- The base plugin, able to answer questions about the IDE Engine environment.
  , ("base",        baseDescriptor)
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
  let withLogFun = case optLogFile opts of
        Just f -> withFileLogging f
        Nothing -> withStdoutLogging

  withLogFun $ do
    if optDebugOn opts
      then setLogLevel LevelDebug
      else setLogLevel LevelError

    -- We change the current working dirextory of HIE to user home
    -- directory so that plugins do not depend on cwd. All paths to
    -- all project files referenced in commands are expected to be
    -- absolute. Cwd is state and we do not want state in what is
    -- async system.

    getUserHomeDirectory >>= mapM_ setCurrentDirectory

    logm $  "run entered for HIE " ++ version
    cin <- atomically newTChan :: IO (TChan ChannelRequest)

    -- log $ T.pack $ "replPluginInfo:" ++ show replPluginInfo

    case validatePlugins plugins of
      Just err -> error (pdeErrorMsg err)
      Nothing -> return ()

    -- launch the dispatcher.
    _ <- forkIO (runIdeM (IdeState plugins Map.empty) (dispatcher cin))

    -- TODO: pass port in as a param from GlobalOpts
    when (optHttp opts) $
      void $ forkIO (jsonHttpListener cin (optPort opts))

    -- Can have multiple listeners, each using a different transport protocol, so
    -- long as they can pass through a ChannelRequest
    if (optConsole opts)
       -- then replListener plugins cin
       then consoleListener plugins cin
       else jsonStdioTransport cin

    -- At least one needs to be launched, othewise a threadDelay with a large
    -- number should be given. Or some other waiting action.

getUserHomeDirectory :: IO (Maybe String)
getUserHomeDirectory = do
    -- On POSIX-like $HOME points to user home directory.
    -- On Windows %USERPROFILE% points to user home directory.
    runMaybeT (msum [ MaybeT $ lookupEnv "HOME"
                    , MaybeT $ lookupEnv "USERPROFILE"])

-- ---------------------------------------------------------------------

-- |Do whatever it takes to get a request from the IDE.
-- pass the request through to the main event dispatcher, and listen on the
-- reply channel for the response, which should go back to the IDE, using
-- whatever it takes.
listener :: TChan ChannelRequest -> IO ()
listener = assert False undefined
