module PluginUtilsSpec where

import Haskell.Ide.Engine.PluginUtils
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PluginUtils" pluginUtilsSpec

dispatcherSpec :: Spec
dispatcherSpec = do
  describe "" $ do

    it "" $ do
      validatePlugins pluginsWithoutCollisions `shouldBe` []
