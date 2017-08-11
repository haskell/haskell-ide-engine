{-# LANGUAGE OverloadedStrings #-}
module ExtensibleStateSpec where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Text                           as T
import           Data.Typeable
import qualified GhcMod.ModuleLoader                 as GM
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
import           TestUtils

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ExtensibleState" extensibleStateSpec

dispatchRequest :: ToJSON a => PluginId -> CommandName -> a -> IdeM (IdeResponse Value)
dispatchRequest plugin com arg = do
  mv <- liftIO newEmptyMVar
  runPluginCommand plugin com (toJSON arg) (putMVar mv)
  liftIO $ takeMVar mv

dispatchRequestP :: IdeM a -> IO a
dispatchRequestP =
    runIdeM testOptions (IdeState testPlugins GM.emptyModuleCache)

extensibleStateSpec :: Spec
extensibleStateSpec =
  describe "stores and retrieves in the state" $
    it "stores the first one" $ do
      r <- dispatchRequestP $ do
          r1 <- dispatchRequest "test" "cmd1" ()
          r2 <- dispatchRequest "test" "cmd2" ()
          return (r1,r2)
      fst r `shouldBe` IdeResponseOk (String "result:put foo")
      snd r `shouldBe` IdeResponseOk (String "result:got:\"foo\"")

    -- ---------------------------------

-- ---------------------------------------------------------------------

testPlugins :: IdePlugins
testPlugins = pluginDescToIdePlugins [("test",testDescriptor)]

testDescriptor :: PluginDescriptor
testDescriptor = PluginDescriptor
  {
    pluginName = "testDescriptor"
  , pluginDesc = "PluginDescriptor for testing Dispatcher"
  , pluginCommands = [
        PluginCommand "cmd1" "description" cmd1
      , PluginCommand "cmd2" "description" cmd2
      ]
  }

-- ---------------------------------------------------------------------

cmd1 :: CommandFunc () T.Text
cmd1 = CmdSync $ \_ -> do
  GM.put (MS1 "foo")
  return (IdeResponseOk (T.pack "result:put foo"))

cmd2 :: CommandFunc () T.Text
cmd2 = CmdSync $ \_ -> do
  (MS1 v) <- GM.get
  return (IdeResponseOk (T.pack $ "result:got:" ++ show v))

newtype MyState1 = MS1 T.Text deriving Typeable

instance GM.ExtensionClass MyState1 where
  initialValue = MS1 "initial"

-- ---------------------------------------------------------------------
