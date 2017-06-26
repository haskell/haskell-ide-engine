{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HooglePluginSpec where

import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.Vector as V
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.HooglePlugin
import           System.Directory
import qualified Data.Map as Map
import           TestUtils
import           Hoogle
import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hoogle plugin" hoogleSpec

-- ---------------------------------------------------------------------

testPlugins :: Plugins
testPlugins = Map.fromList [("hoogle",untagPluginDescriptor hoogleDescriptor)]

dispatchRequest :: IdeRequest -> IO (Maybe (IdeResponse Value))
dispatchRequest req = do
  testChan <- atomically newTChan
  let cr = CReq "hoogle" 1 req testChan
  r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch testPlugins cr)
  return r

dispatchRequestP :: IdeM a -> IO a
dispatchRequestP = runIdeM testOptions (IdeState Map.empty Map.empty Map.empty)

-- ---------------------------------------------------------------------

hoogleSpec :: Spec
hoogleSpec = do
  describe "hoogle environment" $ do
    it "Checks the default dababase location" $ do
      db <- defaultDatabaseLocation
      exists <- doesFileExist db
      unless exists $ do hoogle ["generate"]
  describe "hoogle plugin commands(old plugin api)" $ do
    it "runs the info command" $ do
      let req = IdeRequest "info" (Map.fromList [("expr", ParamTextP "head")])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk (String "head :: [a] -> a\nbase Prelude\nExtract the first element of a list, which must be non-empty.\n\n"))

    -- ---------------------------------

    it "runs the lookup command" $ do
      let req = IdeRequest "lookup" (Map.fromList [("term", ParamTextP "[a] -> a")])
          extractFirst (IdeResponseOk xs) = do
              case xs of
                   Array a -> a V.!? 0
                   _ -> Nothing
          extractFirst _ = Nothing
      r <- dispatchRequest req
      (extractFirst =<< r) `shouldBe` Just (String "Prelude head :: [a] -> a")

  ---- ---------------------------------

  describe "hoogle plugin commands(new plugin api)" $ do
    it "runs the info command" $ do
      let req = infoCmd' "head"
      r <- dispatchRequestP req
      r `shouldBe` (IdeResponseOk "head :: [a] -> a\nbase Prelude\nExtract the first element of a list, which must be non-empty.\n\n")

    -- ---------------------------------

    it "runs the lookup command" $ do
      let req = lookupCmd' 1 "[a] -> a"
      r <- dispatchRequestP req
      r `shouldBe` IdeResponseOk ["Prelude head :: [a] -> a"]
