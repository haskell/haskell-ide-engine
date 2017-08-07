{-# LANGUAGE OverloadedStrings #-}
module DispatcherSpec where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.Map                              as Map
import qualified Data.Set                              as S
import qualified Data.Text                             as T
import qualified GhcMod.ModuleLoader                   as GM
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import           TestUtils

import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "dispatcher" dispatcherSpec

-- -- |Used when running from ghci, and it sets the current directory to ./tests
-- tt :: IO ()
-- tt = do
--   cd ".."
--   hspec spec

-- ---------------------------------------------------------------------

dispatcherSpec :: Spec
dispatcherSpec = do
  describe "async dispatcher operation" $ do

    it "receives replies out of order" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let tp = testPlugins chSync
      runIdeM testOptions (IdeState tp GM.emptyModuleCache)
        $ runPluginCommand "test" "cmdasync1" (toJSON ()) (atomically . writeTChan chan)
      runIdeM testOptions (IdeState tp GM.emptyModuleCache)
        $ runPluginCommand "test" "cmdasync2" (toJSON ()) (atomically . writeTChan chan)
      rc1 <- atomically $ readTChan chan
      rc2 <- atomically $ readTChan chan
      rc1 `shouldBe` IdeResponseOk (String "asyncCmd2 sending strobe")
      rc2 `shouldBe` IdeResponseOk (String "asyncCmd1 got strobe")

  describe "New plugin dispatcher operation" $ do
    it "dispatches response correctly" $ do
      inChan <- atomically newTChan
      outChan <- atomically newTChan
      cancelTVar <- newTVarIO S.empty
      wipTVar <- newTVarIO S.empty
      versionTVar <- newTVarIO $ Map.singleton (filePathToUri "test") 3
      let req1 = PReq Nothing Nothing (Just $ J.IdInt 1) (atomically . writeTChan outChan) $ return $ IdeResponseOk $ T.pack "text1"
          req2 = PReq Nothing Nothing (Just $ J.IdInt 2) (atomically . writeTChan outChan) $ return $ IdeResponseOk $ T.pack "text2"
          req3 = PReq Nothing (Just (filePathToUri "test", 2)) Nothing (atomically . writeTChan outChan) $ return $ IdeResponseOk $ T.pack "text3"
          req4 = PReq Nothing Nothing (Just $ J.IdInt 3) (atomically . writeTChan outChan) $ return $ IdeResponseOk $ T.pack "text4"
      pid <- forkIO $ runIdeM testOptions (IdeState (pluginDescToIdePlugins []) GM.emptyModuleCache) (dispatcherP (DispatcherEnv cancelTVar wipTVar versionTVar) inChan)
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

testPlugins :: TChan () -> IdePlugins
testPlugins chSync = pluginDescToIdePlugins [("test",testDescriptor chSync)]

testDescriptor :: TChan () -> PluginDescriptor
testDescriptor chSync = PluginDescriptor
  {
    pluginName = "testDescriptor"
  , pluginDesc = "PluginDescriptor for testing Dispatcher"
  , pluginCommands  =
      [ PluginCommand "cmdasync1" "desc" (asyncCmd1 chSync)
      , PluginCommand "cmdasync2" "desc" (asyncCmd2 chSync)
      ]
  }

-- ---------------------------------------------------------------------

asyncCmd1 :: TChan () -> CommandFunc () T.Text
asyncCmd1 ch = CmdAsync $ \f _ -> do
  _ <- liftIO $ forkIO $ do
    _synced <- atomically $ readTChan ch
    logm $ "asyncCmd1 got val"
    f (IdeResponseOk "asyncCmd1 got strobe")
  return ()

asyncCmd2 :: TChan () -> CommandFunc () T.Text
asyncCmd2 ch  = CmdAsync $ \f _ -> do
  _ <- liftIO $ forkIO $ do
    f (IdeResponseOk "asyncCmd2 sending strobe")
    atomically $ writeTChan ch ()
  return ()

-- ---------------------------------------------------------------------
