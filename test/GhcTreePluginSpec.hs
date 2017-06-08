{-# LANGUAGE OverloadedStrings #-}
module GhcTreePluginSpec where

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.Map as Map
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.GhcTreePlugin
import           TestUtils

import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ghc-tree plugin" ghctreeSpec

-- -- |Used when running from ghci, and it sets the current directory to ./tests
-- tt :: IO ()
-- tt = do
--   cd ".."
--   hspec spec

-- ---------------------------------------------------------------------

testPlugins :: Plugins
testPlugins = Map.fromList [("ghctree",untagPluginDescriptor ghcTreeDescriptor)]

-- TODO: break this out into a TestUtils file
dispatchRequest :: IdeRequest -> IO (Maybe (IdeResponse Object))
dispatchRequest req = do
  testChan <- atomically newTChan
  let cr = CReq "ghctree" 1 req testChan
  runIdeM testOptions (IdeState Map.empty Map.empty) (doDispatch testPlugins cr)

-- ---------------------------------------------------------------------

-- | We're not checking the full tree, just checking we have an answer
ghctreeSpec :: Spec
ghctreeSpec = do
  describe "ghc-tree plugin commands" $ do
    it "runs the trees command" $ do
      let req = IdeRequest "trees" (Map.fromList [("file", ParamFileP $ filePathToUri "./ApplyRefact.hs")])
      r <- cdAndDo "./test/testdata" (dispatchRequest req)
      case r of
        Just (IdeResponseFail f) -> fail $ show f
        Just (IdeResponseError e) -> fail $ show e
        Nothing -> fail "Got no response!"
        _ -> return ()
