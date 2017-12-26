{-# LANGUAGE OverloadedStrings #-}
module GhcTreePluginSpec where

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

-- ---------------------------------------------------------------------

testPlugins :: IdePlugins
testPlugins = pluginDescToIdePlugins [("ghctree",ghcTreeDescriptor)]

-- ---------------------------------------------------------------------

-- | We're not checking the full tree, just checking we have an answer
ghctreeSpec :: Spec
ghctreeSpec = do
  describe "ghc-tree plugin commands(old plugin api)" $ do
    it "runs the trees command" $ do
      let req = filePathToUri "./ApplyRefact.hs"
      r <- cdAndDo "./test/testdata" (runSingleReq testPlugins "ghctree" "trees" req)
      case r of
        (IdeResponseFail f)  -> fail $ show f
        (IdeResponseError e) -> fail $ show e
        _                    -> return ()
  describe "ghc-tree plugin commands(new plugin api)" $ do
    it "runs the trees command" $ do
      let req = treesCmd (filePathToUri "./ApplyRefact.hs")
      r <- cdAndDo "./test/testdata" (runIGM testPlugins req)
      case r of
        IdeResponseFail f  -> fail $ show f
        IdeResponseError e -> fail $ show e
        _                  -> return ()
