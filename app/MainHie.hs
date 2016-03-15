{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Exception
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as Map
import           Data.Proxy
import qualified Data.Text as T
import           Data.Version (showVersion)
import           Data.Vinyl
import           Development.GitRev (gitCommitCount)
import           Distribution.System (buildArch)
import           Distribution.Text (display)
import           GHC.TypeLits
import           Haskell.Ide.Engine.Console
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.Options
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Swagger
import           Haskell.Ide.Engine.Transport.JsonHttp
import           Haskell.Ide.Engine.Transport.JsonStdio
import           Haskell.Ide.Engine.Transport.JsonTcp
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.Engine.Utils
import           Network.Simple.TCP
import           Options.Applicative.Simple
import qualified Paths_haskell_ide_engine as Meta
import           System.Directory
import           System.Exit

-- ---------------------------------------------------------------------
-- plugins

import           Haskell.Ide.ApplyRefactPlugin
import           Haskell.Ide.Engine.BasePlugin
import           Haskell.Ide.ExamplePlugin2
import           Haskell.Ide.ExamplePluginAsync
import           Haskell.Ide.GhcModPlugin
import           Haskell.Ide.GhcTreePlugin
import           Haskell.Ide.HaRePlugin

-- ---------------------------------------------------------------------

-- | This will be read from a configuration, eventually
taggedPlugins :: Rec Plugin _
taggedPlugins =
     Plugin (Proxy :: Proxy "applyrefact") applyRefactDescriptor
  :& Plugin (Proxy :: Proxy "eg2")         example2Descriptor
  :& Plugin (Proxy :: Proxy "egasync")     exampleAsyncDescriptor
  :& Plugin (Proxy :: Proxy "ghcmod")      ghcmodDescriptor
  :& Plugin (Proxy :: Proxy "ghctree")     ghcTreeDescriptor
  :& Plugin (Proxy :: Proxy "hare")        hareDescriptor
  :& Plugin (Proxy :: Proxy "base")        baseDescriptor
  :& RNil




recProxy :: Rec f t -> Proxy t
recProxy _ = Proxy

plugins :: Plugins
plugins =
  Map.fromList $
  recordToList'
    (\(Plugin name desc) ->
       (T.pack $ symbolVal name,untagPluginDescriptor desc))
    taggedPlugins

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

    case projectRoot opts of
      Nothing -> pure ()
      Just root -> setCurrentDirectory root

    logm $  "run entered for HIE " ++ version
    cin <- atomically newTChan :: IO (TChan ChannelRequest)

    -- log $ T.pack $ "replPluginInfo:" ++ show replPluginInfo

    case validatePlugins plugins of
      Just err -> error (pdeErrorMsg err)
      Nothing -> return ()

    when (optDumpSwagger opts) $ do
      -- putStrLn "dumping swagger definition"
      -- let swagger = hieSwagger plugins
      let swagger = hieSwagger2 (recProxy taggedPlugins)
      -- putStrLn (show swagger)
      putStrLn (B.unpack $  encode swagger)
      exitSuccess

    -- launch the dispatcher.
    _ <- forkIO (runIdeM (IdeState plugins Map.empty) (dispatcher cin))

    -- TODO: pass port in as a param from GlobalOpts
    when (optHttp opts) $
      void $ forkIO (jsonHttpListener (hieSwagger plugins) (recProxy taggedPlugins) cin (optPort opts))

    when (optTcp opts) $
      void $ forkIO (jsonTcpTransport (optOneShot opts) cin HostAny (show $ optTcpPort opts))
    -- Can have multiple listeners, each using a different transport protocol, so
    -- long as they can pass through a ChannelRequest
    if (optConsole opts)
       then consoleListener plugins cin
       else jsonStdioTransport (optOneShot opts) cin

    -- At least one needs to be launched, othewise a threadDelay with a large
    -- number should be given. Or some other waiting action.

-- |Do whatever it takes to get a request from the IDE.
-- pass the request through to the main event dispatcher, and listen on the
-- reply channel for the response, which should go back to the IDE, using
-- whatever it takes.
listener :: TChan ChannelRequest -> IO ()
listener = assert False undefined
