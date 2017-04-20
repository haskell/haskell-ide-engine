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
{-

Start up an actual instance of the HIE server, and interact with it.

The startup code is based on that in MainHie.hs

TODO: extract the commonality

-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
-- import           Control.Exception
import           Control.Monad
-- import           Control.Monad.Logger
import           Control.Monad.STM
import           Data.Aeson
-- import qualified Data.ByteString.Lazy       as B
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import           Data.Proxy
-- import           Data.Semigroup
import qualified Data.Text as T
-- import           Data.Version (showVersion)
import           Data.Vinyl
-- import           Development.GitRev (gitCommitCount)
-- import           Distribution.System (buildArch)
-- import           Distribution.Text (display)
import           GHC.TypeLits
-- import           Haskell.Ide.Engine.Console
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadFunctions
-- import           Haskell.Ide.Engine.Options
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.SemanticTypes
-- import           Haskell.Ide.Engine.Swagger
import           Haskell.Ide.Engine.Transport.JsonHttp
-- import           Haskell.Ide.Engine.Transport.JsonStdio
-- import           Haskell.Ide.Engine.Transport.JsonTcp
-- import           Haskell.Ide.Engine.Transport.LspStdio
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.Engine.Utils
import           Haskell.Ide.HaRePlugin
import qualified Language.Haskell.GhcMod.Types as GM
-- import           Network.Simple.TCP
-- import           Options.Applicative.Simple
-- import qualified Paths_haskell_ide_engine as Meta
import           System.Directory
-- import           System.Exit
import           System.FilePath
import           TestUtils

import           Test.Hspec

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

startServer :: IO (TChan ChannelRequest, TChan ChannelResponse)
startServer = do
  cin  <- atomically newTChan :: IO (TChan ChannelRequest)
  cout <- atomically newTChan :: IO (TChan ChannelResponse)

  case validatePlugins plugins of
    Just err -> error (pdeErrorMsg err)
    Nothing -> return ()

  let dispatcherProc = void $ forkIO $ runIdeM testOptions (IdeState plugins Map.empty) (dispatcher cin)
  void dispatcherProc
  return (cin,cout)

-- ---------------------------------------------------------------------

main :: IO ()
main = withFileLogging "./test-functional.log" $ cdAndDo "./test/testdata"  $ hspec spec
-- main = withFileLogging "./test-functional.log" $ cdAndDo "/home/alanz/tmp/haskell-hie-test-project"  $ hspec spec

spec :: Spec
spec = do
  describe "functional spec" functionalSpec


-- ---------------------------------------------------------------------

dispatchRequest :: TChan ChannelRequest -> TChan ChannelResponse -> ChannelRequest -> IO (Maybe (IdeResponse Object))
dispatchRequest cin cout req = do
  atomically $ writeTChan cin req
  (CResp _ _ rsp) <- atomically $ readTChan cout
  return (Just rsp)

-- ---------------------------------------------------------------------
{- -}
functionalSpec :: Spec
functionalSpec = do
  describe "consecutive plugin commands" $ do

    it "returns hints as diagnostics" $ do
      (cin,cout) <- startServer
      cwd <- getCurrentDirectory

      -- -------------------------------

      let req1 = IdeRequest "lint" (Map.fromList [("file",ParamValP $ ParamFile "./FuncTest.hs")
                                                ])
      r1 <- dispatchRequest cin cout (CReq "applyrefact" 1 req1 cout)
      r1 `shouldBe`
        Just (IdeResponseOk (jsWrite (FileDiagnostics
                                      { fdFileName = "file://./FuncTest.hs"
                                      , fdDiagnostics =
                                        [ Diagnostic (Range (Position 10 7) (Position 11 19))
                                                     (Just DsWarning)
                                                     Nothing
                                                     (Just "hlint")
                                                     "Redundant do\nFound:\n  do putStrLn \"hello\"\nWhy not:\n  putStrLn \"hello\"\n"
                                        ]
                                      }
                                     )))

      -- -------------------------------

      let req2 = IdeRequest "type" (Map.fromList [("file",ParamValP $ ParamFile "./FuncTest.hs")
                                                  ,("start_pos",ParamValP $ ParamPos (toPos (10,2)))])
      r2 <- dispatchRequest cin cout (CReq "ghcmod" 2 req2 cout)
      r2 `shouldBe`
        Just (IdeResponseOk (H.fromList ["type_info".=toJSON
                        [TypeResult (toPos (10,1)) (toPos (11,19)) "IO ()"
                        ]
                        ]))

      -- -------------------------------

      let req3 = IdeRequest "demote" (Map.fromList [("file",ParamValP $ ParamFile "./FuncTest.hs")
                                                  ,("start_pos",ParamValP $ ParamPos (toPos (8,1)))])
      r3 <- dispatchRequest cin cout (CReq "hare" 3 req3 cout)
      r3 `shouldBe`
        Just (IdeResponseOk $ jsWrite (RefactorResult [HieDiff (cwd </> "FuncTest.hs")
                                                               (cwd </> "FuncTest.refactored.hs")
                                                                "7,8c7,8\n< \n< bb = 5\n---\n>   where\n>     bb = 5\n"]))
{- -}

{-
functionalSpec :: Spec
functionalSpec = do
  describe "consecutive plugin commands" $ do

    it "returns hints as diagnostics" $ do
      (cin,cout) <- startServer
      cwd <- getCurrentDirectory
      let req1 = IdeRequest "lint" (Map.fromList [("file",ParamValP $ ParamFile "./src/Foo.hs")
                                                ])
      r1 <- dispatchRequest cin cout (CReq "applyrefact" 1 req1 cout)
      r1 `shouldBe`
        Just (IdeResponseOk (jsWrite (FileDiagnostics
                                      { fdFileName = "file://./src/Foo.hs"
                                      , fdDiagnostics =
                                        [ Diagnostic (Range (Position 8 7) (Position 9 19))
                                                     (Just DsWarning)
                                                     Nothing
                                                     (Just "hlint")
                                                     "Redundant do\nFound:\n  do putStrLn \"hello\"\nWhy not:\n  putStrLn \"hello\"\n"
                                        ]
                                      }
                                     )))

      let req2 = IdeRequest "demote" (Map.fromList [("file",ParamValP $ ParamFile "./src/Foo.hs")
                                                  ,("start_pos",ParamValP $ ParamPos (toPos (6,1)))])
      r2 <- dispatchRequest cin cout (CReq "hare" 2 req2 cout)
      r2 `shouldBe`
        Just (IdeResponseOk $ jsWrite (RefactorResult [HieDiff (cwd </> "FuncTest.hs")
                                                               (cwd </> "FuncTest.refactored.hs")
                                                                "5,6c5,6\n< \n< bb = 5\n---\n>   where\n>     bb = 5\n"]))
-}
