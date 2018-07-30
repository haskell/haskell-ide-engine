{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HooglePluginSpec where

import           Control.Monad
import           Data.Maybe
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Plugin.Hoogle
import           Hoogle
import           System.Directory
import           Test.Hspec
import           TestUtils

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hoogle plugin" hoogleSpec

-- ---------------------------------------------------------------------

testPlugins :: IdePlugins
testPlugins = mkIdePlugins [hoogleDescriptor]

dispatchRequestP :: IdeGhcM a -> IO a
dispatchRequestP = runIGM testPlugins

-- ---------------------------------------------------------------------

hoogleSpec :: Spec
hoogleSpec = do
  describe "hoogle environment" $
    it "Checks the default dababase location" $ do
      db <- defaultDatabaseLocation
      exists <- doesFileExist db
      unless exists $ hoogle ["generate"]
  describe "hoogle initialization" $ do
    it "initialization succeeds" $ do
      r <- dispatchRequestP initializeHoogleDb
      isJust r `shouldBe` True

  ---- ---------------------------------

  describe "hoogle plugin commands(new plugin api)" $ do
    it "runs the info command" $ do
      let req = liftToGhc $ infoCmd' "head"
      r <- dispatchRequestP $ initializeHoogleDb >> req
      r `shouldBe` Right "head :: [a] -> a\nbase Prelude\nExtract the first element of a list, which must be non-empty.\n\n"

    -- ---------------------------------

    it "runs the lookup command" $ do
      let req = liftToGhc $ lookupCmd' 1 "[a] -> a"
      r <- dispatchRequestP $ initializeHoogleDb >> req
      r `shouldBe` Right ["Prelude head :: [a] -> a"]
