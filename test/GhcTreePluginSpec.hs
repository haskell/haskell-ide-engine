{-# LANGUAGE OverloadedStrings #-}
module GhcTreePluginSpec where

import           Control.Concurrent
import           Data.Aeson
import qualified GhcMod.ModuleLoader                as GM
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
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

testPlugins :: IdePlugins
testPlugins = pluginDescToIdePlugins [("ghctree",ghcTreeDescriptor)]

dispatchRequest :: ToJSON a => PluginId -> CommandName -> a -> IO (IdeResponse Value)
dispatchRequest plugin com arg = do
  mv <- newEmptyMVar
  dispatchRequestP $ runPluginCommand plugin com (toJSON arg) (putMVar mv)
  takeMVar mv

dispatchRequestP :: IdeM a -> IO a
dispatchRequestP = runIdeM testOptions (IdeState testPlugins GM.emptyModuleCache)

-- ---------------------------------------------------------------------

-- | We're not checking the full tree, just checking we have an answer
ghctreeSpec :: Spec
ghctreeSpec = do
  describe "ghc-tree plugin commands(old plugin api)" $ do
    it "runs the trees command" $ do
      let req = filePathToUri "./ApplyRefact.hs"
      r <- cdAndDo "./test/testdata" (dispatchRequest "ghctree" "trees" req)
      case r of
        (IdeResponseFail f)  -> fail $ show f
        (IdeResponseError e) -> fail $ show e
        _                    -> return ()
  describe "ghc-tree plugin commands(new plugin api)" $ do
    it "runs the trees command" $ do
      let req = treesCmd (filePathToUri "./ApplyRefact.hs")
      r <- cdAndDo "./test/testdata" (dispatchRequestP req)
      case r of
        IdeResponseFail f  -> fail $ show f
        IdeResponseError e -> fail $ show e
        _                  -> return ()
