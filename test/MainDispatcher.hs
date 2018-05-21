{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Test.Hspec.Formatters.Jenkins
import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.Text as T
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Types            as J
import           TestUtils

import           Test.Hspec
-- import           Test.Hspec.Runner

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  setupStackFiles
  withFileLogging "./test-main-dispatcher.log" $ hspec spec

-- main :: IO ()
-- main = do
--   summary <- withFile "results.xml" WriteMode $ \h -> do
--     let c = defaultConfig
--           { configFormatter = xmlFormatter
--           , configHandle = h
--           }
--     hspecWith c Spec.spec
--   unless (summaryFailures summary == 0) $
--     exitFailure

-- ---------------------------------------------------------------------


spec :: Spec
spec = do
  describe "dispatcher" dispatcherSpec

-- ---------------------------------------------------------------------

dispatcherSpec :: Spec
dispatcherSpec = do

  describe "New plugin dispatcher operation" $ do
    it "dispatches response correctly" $ do
      inChan <- atomically newTChan
      outChan <- atomically newTChan
      cancelTVar <- newTVarIO S.empty
      wipTVar <- newTVarIO S.empty
      versionTVar <- newTVarIO $ Map.singleton (filePathToUri "test") 3
      let req1 = GReq Nothing Nothing                          (Just $ J.IdInt 1) (atomically . writeTChan outChan) $ return $ IdeResponseOk $ T.pack "text1"
          req2 = GReq Nothing Nothing                          (Just $ J.IdInt 2) (atomically . writeTChan outChan) $ return $ IdeResponseOk $ T.pack "text2"
          req3 = GReq Nothing (Just (filePathToUri "test", 2)) Nothing            (atomically . writeTChan outChan) $ return $ IdeResponseOk $ T.pack "text3"
          req4 = GReq Nothing Nothing                          (Just $ J.IdInt 3) (atomically . writeTChan outChan) $ return $ IdeResponseOk $ T.pack "text4"
      pid <- forkIO $ runIdeGhcM testOptions (IdeState emptyModuleCache Map.empty (pluginDescToIdePlugins []) Map.empty Nothing)
                              (dispatcherP (DispatcherEnv cancelTVar wipTVar versionTVar) inChan)
      atomically $ writeTChan inChan req1
      atomically $ modifyTVar cancelTVar (S.insert (J.IdInt 2))
      atomically $ writeTChan inChan req2
      atomically $ writeTChan inChan req3
      atomically $ writeTChan inChan req4
      resp1 <- atomically $ readTChan outChan
      resp2 <- atomically $ readTChan outChan
      killThread pid
      resp1 `shouldBe` IdeResponseOk "text1"
      resp2 `shouldBe` IdeResponseOk "text4"

-- ---------------------------------------------------------------------
