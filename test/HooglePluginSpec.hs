{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HooglePluginSpec where

import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Text as T
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

dispatchRequest :: IdeRequest -> IO (Maybe (IdeResponse Object))
dispatchRequest req = do
  testChan <- atomically newTChan
  let cr = CReq "hoogle" 1 req testChan
  r <- runIdeM testOptions (IdeState Map.empty Map.empty) (doDispatch testPlugins cr)
  return r

-- ---------------------------------------------------------------------

hoogleSpec :: Spec
hoogleSpec = do
  describe "hoogle environment" $ do
    it "Checks the default dababase location" $ do
      db <- defaultDatabaseLocation
      exists <- doesFileExist db
      unless exists $ do hoogle ["generate"]
  describe "hoogle plugin commands" $ do
    it "runs the info command" $ do
      let req = IdeRequest "info" (Map.fromList [("expr", ParamTextP "head")])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk (H.fromList ["ok" .= ("head :: [a] -> a\nbase Prelude\nExtract the first element of a list, which must be non-empty.\n\n"::String)]))

    -- ---------------------------------

    it "runs the lookup command" $ do
      let req = IdeRequest "lookup" (Map.fromList [("term", ParamTextP "[a] -> a")])
          extractFirst (IdeResponseOk hmap) = do
              xs <- H.lookup ("ok" :: T.Text) hmap
              case xs of
                   Array a -> a V.!? 0
                   _ -> Nothing
          extractFirst _ = Nothing
      r <- dispatchRequest req
      (extractFirst =<< r) `shouldBe` Just (String "Prelude head :: [a] -> a")


