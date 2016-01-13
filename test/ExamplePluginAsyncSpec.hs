{-# LANGUAGE OverloadedStrings #-}
module ExamplePluginAsyncSpec where

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.ExamplePluginAsync

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ExamplePluginAsync" examplePluginAsyncSpec

examplePluginAsyncSpec :: Spec
examplePluginAsyncSpec = do
  describe "stores and retrieves in the state" $ do
    it "stores the first one" $ do
      chan <- atomically newTChan
      let req1 = IdeRequest "cmd1" (Map.fromList [])
          cr1 = CReq "test" 1 req1 chan
      let req2 = IdeRequest "cmd2" (Map.fromList [])
          cr2 = CReq "test" 1 req2 chan
      (ra,rb,rc) <- withStdoutLogging $ runIdeM (IdeState Map.empty Map.empty)
        (do
          r1 <- doDispatch testPlugins cr1
          r2 <- doDispatch testPlugins cr2
          r3 <- doDispatch testPlugins cr1
          return (r1,r2,r3))
      ra `shouldBe` Just (IdeResponseOk (H.fromList ["ok" .= ("res=wp cmd1:cnt=1"::String)]))
      rb `shouldBe` Just (IdeResponseOk (H.fromList ["ok" .= ("res=wp cmd2:cnt=2"::String)]))
      rc `shouldBe` Just (IdeResponseOk (H.fromList ["ok" .= ("res=wp cmd1:cnt=3"::String)]))

    -- ---------------------------------

-- ---------------------------------------------------------------------

testPlugins :: Plugins
testPlugins = Map.fromList [("test",untagPluginDescriptor exampleAsyncDescriptor)]
