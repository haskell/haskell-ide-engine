{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HooglePluginSpec where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import           Data.Maybe
import qualified Data.Map                           as Map
import qualified GhcMod.ModuleLoader                as GM
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.HooglePlugin
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
testPlugins = pluginDescToIdePlugins [("hoogle",hoogleDescriptor)]

dispatchRequest :: ToJSON a => PluginId -> CommandName -> a -> IO (IdeResponse Value)
dispatchRequest plugin com arg = do
  mv <- newEmptyMVar
  dispatchRequestP $ runPluginCommand plugin com (toJSON arg) (putMVar mv)
  takeMVar mv

dispatchRequestP :: IdeM a -> IO a
dispatchRequestP = runIdeM testOptions (IdeState GM.emptyModuleCache testPlugins Map.empty)

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
      let req = liftAsync $ infoCmd' "head"
      r <- dispatchRequestP $ initializeHoogleDb >> req
      r `shouldBe` Right "head :: [a] -> a\nbase Prelude\nExtract the first element of a list, which must be non-empty.\n\n"

    -- ---------------------------------

    it "runs the lookup command" $ do
      let req = liftAsync $ lookupCmd' 1 "[a] -> a"
      r <- dispatchRequestP $ initializeHoogleDb >> req
      r `shouldBe` Right ["Prelude head :: [a] -> a"]
