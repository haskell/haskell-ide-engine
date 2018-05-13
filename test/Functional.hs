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
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Types
import           Language.Haskell.LSP.Types hiding (error, name)
import           System.Directory
import           System.FilePath
import           TestUtils

import           Test.Hspec

-- ---------------------------------------------------------------------
-- plugins

import           Haskell.Ide.Engine.Plugin.ApplyRefact
import           Haskell.Ide.Engine.Plugin.Base
import           Haskell.Ide.Engine.Plugin.Example2
import           Haskell.Ide.Engine.Plugin.GhcMod
import           Haskell.Ide.Engine.Plugin.HaRe

{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
-- ---------------------------------------------------------------------

plugins :: IdePlugins
plugins = pluginDescToIdePlugins
  [("applyrefact", applyRefactDescriptor)
  ,("eg2"        , example2Descriptor)
  ,("ghcmod"     , ghcmodDescriptor)
  ,("hare"       , hareDescriptor)
  ,("base"       , baseDescriptor)
  ]

startServer :: IO (TChan PluginRequest)
startServer = do
  cin  <- atomically newTChan

  let dispatcherProc dispatcherEnv
        = void $ forkIO $ runIdeGhcM testOptions (IdeState emptyModuleCache Map.empty plugins Map.empty Nothing) (dispatcherP dispatcherEnv cin)
  cancelTVar      <- atomically $ newTVar S.empty
  wipTVar         <- atomically $ newTVar S.empty
  versionTVar     <- atomically $ newTVar Map.empty
  let dispatcherEnv = DispatcherEnv
        { cancelReqsTVar     = cancelTVar
        , wipReqsTVar        = wipTVar
        , docVersionTVar     = versionTVar
        }

  void $ dispatcherProc dispatcherEnv
  return cin

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  setupStackFiles
  withFileLogging "./test-functional.log" $ cdAndDo "./test/testdata"  $ hspec spec

spec :: Spec
spec = do
  describe "functional spec" functionalSpec


-- ---------------------------------------------------------------------

dispatchRequest :: ToJSON a => TChan PluginRequest -> PluginId -> CommandName -> a -> IO (IdeResponse DynamicJSON)
dispatchRequest cin plugin com arg = do
  mv <- newEmptyMVar
  let req = GReq Nothing Nothing Nothing (putMVar mv) $
        runPluginCommand plugin com (toJSON arg)
  atomically $ writeTChan cin req
  takeMVar mv

-- ---------------------------------------------------------------------

functionalSpec :: Spec
functionalSpec = do
  describe "consecutive plugin commands" $ do

    it "returns hints as diagnostics" $ do
      cin <- startServer
      cwd <- getCurrentDirectory

      -- -------------------------------

      let req1 = filePathToUri $ cwd </> "FuncTest.hs"
      r1 <- dispatchRequest cin "applyrefact" "lint" req1
      fmap fromDynJSON r1 `shouldBe` IdeResponseOk
                           ( Just
                           $ PublishDiagnosticsParams
                              { _uri = filePathToUri $ cwd </> "FuncTest.hs"
                              , _diagnostics = List
                                [ Diagnostic (Range (Position 9 6) (Position 10 18))
                                             (Just DsInfo)
                                             (Just "Redundant do")
                                             (Just "hlint")
                                             "Redundant do\nFound:\n  do putStrLn \"hello\"\nWhy not:\n  putStrLn \"hello\"\n"
                                             Nothing
                                ]
                              })

      let req3 = HP (filePathToUri $ cwd </> "FuncTest.hs") (toPos (8,1))
      r3 <- dispatchRequest cin "hare" "demote" req3
      fmap fromDynJSON r3 `shouldBe`
        (IdeResponseOk
         $ Just
         $ WorkspaceEdit
           (Just $ H.singleton (filePathToUri $ cwd </> "FuncTest.hs")
                               $ List [TextEdit (Range (Position 6 0) (Position 7 6))
                                                "  where\n    bb = 5"])
           Nothing)

