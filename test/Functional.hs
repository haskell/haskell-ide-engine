{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-

Start up an actual instance of the HIE server, and interact with it.

The startup code is based on that in MainHie.hs

TODO: extract the commonality

-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.HashMap.Strict                   as H
import qualified Data.Map                              as Map
import qualified Data.Set                              as S
import qualified Data.Text                             as T
import qualified GhcMod.ModuleLoader                   as GM
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Types
import           Language.Haskell.LSP.TH.DataTypesJSON hiding (error, name)
import           System.Directory
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

plugins :: IdePlugins
plugins = pluginDescToIdePlugins
  [("applyrefact", applyRefactDescriptor)
  ,("eg2"        , example2Descriptor)
  ,("egasync"    , exampleAsyncDescriptor)
  ,("ghcmod"     , ghcmodDescriptor)
  ,("ghctree"    , ghcTreeDescriptor)
  ,("hare"       , hareDescriptor)
  ,("base"       , baseDescriptor)
  ]

startServer :: IO (TChan PluginRequest)
startServer = do
  cin  <- atomically newTChan

  let dispatcherProc dispatcherEnv = void $ forkIO $ runIdeM testOptions (IdeState plugins GM.emptyModuleCache) (dispatcherP dispatcherEnv cin)
  cancelTVar <- atomically $ newTVar S.empty
  wipTVar <- atomically $ newTVar S.empty
  versionTVar <- atomically $ newTVar Map.empty
  let dispatcherEnv = DispatcherEnv
        { cancelReqsTVar = cancelTVar
        , wipReqsTVar    = wipTVar
        , docVersionTVar = versionTVar
        }

  void $ dispatcherProc dispatcherEnv
  return cin

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  setupStackFiles
  withFileLogging "./test-functional.log" $ cdAndDo "./test/testdata"  $ hspec spec
-- main = withFileLogging "./test-functional.log" $ cdAndDo "/home/alanz/tmp/haskell-hie-test-project"  $ hspec spec

spec :: Spec
spec = do
  describe "functional spec" functionalSpec


-- ---------------------------------------------------------------------

dispatchRequest :: ToJSON a => TChan PluginRequest -> PluginId -> CommandName -> a -> IO (IdeResponse Value)
dispatchRequest cin plugin com arg = do
  mv <- newEmptyMVar
  let req = PReq Nothing Nothing Nothing (const $ return ()) $
        runPluginCommand plugin com (toJSON arg) (putMVar mv)
  atomically $ writeTChan cin req
  takeMVar mv

-- ---------------------------------------------------------------------
{- -}
functionalSpec :: Spec
functionalSpec = do
  describe "consecutive plugin commands" $ do

    it "returns hints as diagnostics" $ do
      cin <- startServer
      cwd <- getCurrentDirectory

      -- -------------------------------

      let req1 = filePathToUri "./FuncTest.hs"
      r1 <- dispatchRequest cin "applyrefact" "lint" req1
      r1 `shouldBe` IdeResponseOk
                           (toJSON PublishDiagnosticsParams
                                      { _uri = filePathToUri "./FuncTest.hs"
                                      , _diagnostics = List
                                        [ Diagnostic (Range (Position 9 6) (Position 10 18))
                                                     (Just DsWarning)
                                                     Nothing
                                                     (Just "hlint")
                                                     "Redundant do\nFound:\n  do putStrLn \"hello\"\nWhy not:\n  putStrLn \"hello\"\n"
                                        ]
                                      }
                                     )

      -- -------------------------------

      let req2 = TP False (filePathToUri "./FuncTest.hs") (toPos (10,2))
      r2 <- dispatchRequest cin "ghcmod" "type" req2
      r2 `shouldBe`
        IdeResponseOk (toJSON [(Range (toPos (10,1)) (toPos (11,19)), ("IO ()" :: T.Text))])

      -- -------------------------------

      let req4 = TP False (filePathToUri "./FuncTest.hs") (toPos (8,1))
      r4 <- dispatchRequest cin "ghcmod" "type" req4
      r4 `shouldBe`
        IdeResponseOk (toJSON [(Range (toPos (8,1)) (toPos (8,7)), ("Int" :: T.Text))])

      -- -------------------------------

      let req3 = HP (filePathToUri "./FuncTest.hs") (toPos (8,1))
      r3 <- dispatchRequest cin "hare" "demote" req3
      r3 `shouldBe`
        (IdeResponseOk
         $ toJSON
         $ WorkspaceEdit
           (Just $ H.singleton (filePathToUri $ cwd </> "FuncTest.hs")
                               $ List [TextEdit (Range (Position 6 0) (Position 7 6))
                                                "  where\n    bb = 5"])
           Nothing)
{- -}

