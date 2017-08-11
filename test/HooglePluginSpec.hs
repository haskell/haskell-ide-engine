{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HooglePluginSpec where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.Vector                        as V
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
dispatchRequestP = runIdeM testOptions (IdeState testPlugins GM.emptyModuleCache)

-- ---------------------------------------------------------------------

hoogleSpec :: Spec
hoogleSpec = do
  describe "hoogle environment" $
    it "Checks the default dababase location" $ do
      db <- defaultDatabaseLocation
      exists <- doesFileExist db
      unless exists $ hoogle ["generate"]
  describe "hoogle plugin commands(old plugin api)" $ do
    it "runs the info command" $ do
      r <- dispatchRequest "hoogle" "info" ("head" :: String)
      r `shouldBe` IdeResponseOk (String "head :: [a] -> a\nbase Prelude\nExtract the first element of a list, which must be non-empty.\n\n")

    -- ---------------------------------

    it "runs the lookup command" $ do
      let extractFirst (IdeResponseOk xs) =
              case xs of
                   Array a -> a V.!? 0
                   _       -> Nothing
          extractFirst _ = Nothing
      r <- dispatchRequest "hoogle" "lookup" ("[a] -> a" :: String)
      extractFirst r `shouldBe` Just (String "Prelude head :: [a] -> a")

  ---- ---------------------------------

  describe "hoogle plugin commands(new plugin api)" $ do
    it "runs the info command" $ do
      let req = infoCmd' "head"
      r <- dispatchRequestP $ initializeHoogleDb >> req
      r `shouldBe` Right "head :: [a] -> a\nbase Prelude\nExtract the first element of a list, which must be non-empty.\n\n"

    -- ---------------------------------

    it "runs the lookup command" $ do
      let req = lookupCmd' 1 "[a] -> a"
      r <- dispatchRequestP $ initializeHoogleDb >> req
      r `shouldBe` Right ["Prelude head :: [a] -> a"]
