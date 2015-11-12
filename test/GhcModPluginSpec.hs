{-# LANGUAGE OverloadedStrings #-}
module GhcModPluginSpec where

-- import           Data.Aeson
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.GhcModPlugin

import qualified Data.Map as Map

import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ghc-mod plugin" ghcmodSpec

-- -- |Used when running from ghci, and it sets the current directory to ./tests
-- tt :: IO ()
-- tt = do
--   cd ".."
--   hspec spec

-- ---------------------------------------------------------------------

ghcmodSpec :: Spec
ghcmodSpec = do
  describe "ghc-mod plugin commands" $ do
    it "runs the check command" $ do
      let req = IdeRequest "check" (Map.fromList [("file", ParamFileP "./test/testdata/FileWithWarning.hs")])
      r <- runIdeM (IdeState Map.empty) (checkCmd [] req)
      r `shouldBe` IdeResponseOk "test/testdata/FileWithWarning.hs:4:7:Not in scope: \8216x\8217\n"

    it "runs the types command, incorrect params" $ do
      let req = IdeRequest "types" (Map.fromList [("file", ParamFileP "./test/testdata/FileWithWarning.hs")])
      r <- runIdeM (IdeState Map.empty) (typesCmd [] req)
      (show r) `shouldBe`
        "IdeResponseFail (IdeError {ideCode = MissingParameter, ideMessage = \"need `start_pos` parameter\", ideInfo = Just (String \"start_pos\")})"

    it "runs the types command, correct params" $ do
      let req = IdeRequest "types" (Map.fromList [("file", ParamFileP "./test/testdata/HaReRename.hs")
                                                 ,("start_pos", ParamPosP (5,9))])
      r <- runIdeM (IdeState Map.empty) (typesCmd [] req)
      r `shouldBe` IdeResponseOk "5 9 5 10 \"Int\"\n5 9 5 14 \"Int\"\n5 1 5 14 \"Int -> Int\"\n"
