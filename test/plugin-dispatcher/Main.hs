{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import qualified Data.Text as T
import           Data.Default
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Scheduler
import           Haskell.Ide.Engine.Types
import           Language.Haskell.LSP.Types
import           TestUtils

import           Test.Hspec
import           Test.Hspec.Runner

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  setupStackFiles
  config <- getHspecFormattedConfig "plugin-dispatcher"
  withFileLogging "plugin-dispatcher.log" $ hspecWith config newPluginSpec

-- ---------------------------------------------------------------------

newPluginSpec :: Spec
newPluginSpec = do
  describe "New plugin dispatcher operation" $
    it "dispatches response correctly" $ do
      outChan <- atomically newTChan
      scheduler <- newScheduler (pluginDescToIdePlugins []) testOptions
      let defCallback = atomically . writeTChan outChan
          delayedCallback = \r -> threadDelay 10000 >> defCallback r

      let req0 = GReq 0 Nothing Nothing                          (Just $ IdInt 0) (\_ -> return () :: IO ())         $ return $ IdeResultOk $ T.pack "text0"
          req1 = GReq 1 Nothing Nothing                          (Just $ IdInt 1) defCallback $ return $ IdeResultOk $ T.pack "text1"
          req2 = GReq 2 Nothing Nothing                          (Just $ IdInt 2) delayedCallback      $ return      $ IdeResultOk $ T.pack "text2"
          req3 = GReq 3 Nothing (Just (filePathToUri "test", 2)) Nothing          defCallback $ return $ IdeResultOk $ T.pack "text3"
          req4 = GReq 4 Nothing Nothing                          (Just $ IdInt 3) defCallback $ return $ IdeResultOk $ T.pack "text4"

      let makeReq = sendRequest scheduler Nothing

      pid <- forkIO $ runScheduler scheduler
                              (\_ _ _ -> return ())
                              (\f x -> f x)
                              def

      sendRequest scheduler (Just (filePathToUri "test", 3)) req0
      makeReq req1
      makeReq req2
      cancelRequest scheduler (IdInt 2)
      makeReq req3
      makeReq req4
      resp1 <- atomically $ readTChan outChan
      resp2 <- atomically $ readTChan outChan
      killThread pid
      resp1 `shouldBe` "text1"
      resp2 `shouldBe` "text4"


