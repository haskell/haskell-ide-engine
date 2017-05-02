{-# LANGUAGE OverloadedStrings #-}
module HooglePluginSpec where

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Control.Exception
import           Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.SemanticTypes
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.HooglePlugin
import           System.Directory
import qualified Data.Map as Map
import           TestUtils

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
  describe "hoogle plugin commands" $ do
    it "runs the info command" $ do
      let req = IdeRequest "info" (Map.fromList [("expr", ParamTextP "head")])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk (H.fromList ["ok" .= ("head :: [a] -> a\nbase Prelude\nExtract the first element of a list, which must be non-empty.\n\n"::String)]))

    -- ---------------------------------

    it "runs the lookup command" $ do
      let req = IdeRequest "lookup" (Map.fromList [("term", ParamTextP "[a] -> a")])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk (H.fromList ["ok" .= ("Prelude head :: [a] -> a\nPrelude last :: [a] -> a\nData.List head :: [a] -> a\nData.List last :: [a] -> a\nGHC.OldList head :: [a] -> a\nGHC.OldList last :: [a] -> a\nDistribution.Compat.Semigroup mconcat :: [a] -> a\nSystem.Console.CmdArgs.Quote modes# :: [a] -> a\nSystem.Console.CmdArgs.Quote enum# :: [a] -> a\nCorePrelude mconcat :: [a] -> a\n"::String)]))


