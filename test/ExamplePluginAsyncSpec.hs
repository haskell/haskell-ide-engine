{-# LANGUAGE OverloadedStrings #-}
module ExamplePluginAsyncSpec where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified GhcMod.ModuleLoader                as GM
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.ExamplePluginAsync
import           TestUtils

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ExamplePluginAsync" examplePluginAsyncSpec

dispatchRequest :: ToJSON a => PluginId -> CommandName -> a -> IdeM (IdeResponse Value)
dispatchRequest plugin com arg = do
  mv <- liftIO newEmptyMVar
  runPluginCommand plugin com (toJSON arg) (putMVar mv)
  liftIO $ takeMVar mv

dispatchRequestP :: IdeM a -> IO a
dispatchRequestP =
   runIdeM testOptions (IdeState testPlugins GM.emptyModuleCache)

examplePluginAsyncSpec :: Spec
examplePluginAsyncSpec = do
  describe "stores and retrieves in the state" $ do
    it "stores the first one" $ do
      (ra,rb,rc) <- dispatchRequestP $ do
          r1 <- dispatchRequest "test" "cmd1" ()
          r2 <- dispatchRequest "test" "cmd2" ()
          r3 <- dispatchRequest "test" "cmd1" ()
          return (r1,r2,r3)
      ra `shouldBe` IdeResponseOk (String "res=wp cmd1:cnt=1")
      rb `shouldBe` IdeResponseOk (String "res=wp cmd2:cnt=2")
      rc `shouldBe` IdeResponseOk (String "res=wp cmd1:cnt=3")

    -- ---------------------------------

-- ---------------------------------------------------------------------

testPlugins :: IdePlugins
testPlugins = pluginDescToIdePlugins [("test",exampleAsyncDescriptor)]
