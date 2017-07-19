{-# LANGUAGE OverloadedStrings #-}
module ExtensibleStateSpec where

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Typeable
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.ExtensibleState
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import           TestUtils

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ExtensibleState" extensibleStateSpec

extensibleStateSpec :: Spec
extensibleStateSpec = do
  describe "stores and retrieves in the state" $ do
    it "stores the first one" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req1 = IdeRequest "cmd1" (Map.fromList [])
          cr1 = CReq "test" 1 req1 chan
      let req2 = IdeRequest "cmd2" (Map.fromList [])
          cr2 = CReq "test" 1 req2 chan
      r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty Map.empty)
        (do
          r1 <- doDispatch (testPlugins chSync) cr1
          r2 <- doDispatch (testPlugins chSync) cr2
          return (r1,r2))
      fst r `shouldBe` Just (IdeResponseOk (String "result:put foo"))
      snd r `shouldBe` Just (IdeResponseOk (String "result:got:\"foo\""))

    -- ---------------------------------

-- ---------------------------------------------------------------------

testPlugins :: TChan () -> Plugins
testPlugins chSync = Map.fromList [("test",testDescriptor chSync)]

testDescriptor :: TChan () -> UntaggedPluginDescriptor
testDescriptor _chSync = PluginDescriptor
  {
    pdUIShortName = "testDescriptor"
  , pdUIOverview = "PluginDescriptor for testing Dispatcher"
  , pdCommands =
      [
        mkCmdWithContext cmd1 "cmd1" [CtxNone] []
      , mkCmdWithContext cmd2 "cmd2" [CtxNone] []
      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

cmd1 :: CommandFunc T.Text
cmd1 = CmdSync $ \_ctxs _req -> do
  put (MS1 "foo")
  return (IdeResponseOk (T.pack $ "result:put foo"))

cmd2 :: CommandFunc T.Text
cmd2 = CmdSync $ \_ctxs _req -> do
  (MS1 v) <- get
  return (IdeResponseOk (T.pack $ "result:got:" ++ show v))

data MyState1 = MS1 T.Text deriving Typeable

instance ExtensionClass MyState1 where
  initialValue = MS1 "initial"

-- ---------------------------------------------------------------------

mkCmdWithContext ::(ValidResponse a)
                 => CommandFunc a -> CommandName -> [AcceptedContext] -> [ParamDescription] -> UntaggedCommand
mkCmdWithContext cmd n cts pds =
        Command
          { cmdDesc = CommandDesc
                        { cmdName = n
                        , cmdUiDescription = "description"
                        , cmdFileExtensions = []
                        , cmdContexts = cts
                        , cmdAdditionalParams = pds
                        , cmdReturnType = "Text"
                        , cmdSave = SaveNone
                        }
          , cmdFunc = cmd
          }

-- ---------------------------------------------------------------------
