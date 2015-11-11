{-# LANGUAGE OverloadedStrings #-}
module JsonStdioSpec where

import           Data.Aeson
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Transport.JsonStdio

import qualified Data.Map as Map

import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "json parsing" jsonSpec

-- -- |Used when running from ghci, and it sets the current directory to ./tests
-- tt :: IO ()
-- tt = do
--   cd ".."
--   hspec spec

-- ---------------------------------------------------------------------

jsonSpec :: Spec
jsonSpec = do
  describe "JSON Generation WireRequest" $ do
    it "generates a WireRequest 1" $ do
      let wr = WireReq "eg1:hello" Map.empty
      (encode wr) `shouldBe` "{\"params\":{},\
                              \\"cmd\":\"eg1:hello\"}"

    it "generates a WireRequest 2" $ do
      let wr = WireReq "eg2:helloTo"  (Map.fromList [("cabal",ParamValP $ ParamText "lib"),("name",ParamValP $ ParamText "foo")])
      (encode wr) `shouldBe` "{\"params\":{\"cabal\":{\"tag\":\"text\",\"contents\":\"lib\"},\
                                          \\"name\":{\"tag\":\"text\",\"contents\":\"foo\"}},\
                              \\"cmd\":\"eg2:helloTo\"}"

  describe "JSON Parsing WireRequest" $ do
    it "parses a WireRequest 1" $ do
      let wr = WireReq "eg1:hello" Map.empty
      (decode "{\"params\":{},\
               \\"cmd\":\"eg1:hello\"}")
         `shouldBe` (Just wr)

    it "parses a WireRequest 2" $ do
      let wr = WireReq "eg2:helloTo" (Map.fromList [("name",ParamValP $ ParamText "foo")])
      (decode "{\"params\":{\"name\":{\"tag\":\"text\",\"contents\":\"foo\"}},\
               \\"cmd\":\"eg2:helloTo\"}")
         `shouldBe` (Just wr)

  describe "JSON Generation WireResponse" $ do
    it "generates a WireResponse Ok" $ do
      let wr = Ok (String "no problem")
      (encode wr) `shouldBe` "{\"tag\":\"Ok\",\"contents\":\"no problem\"}"

    it "generates a WireResponse Fail" $ do
      let wr = Fail (String "Failed")
      (encode wr) `shouldBe` "{\"tag\":\"Fail\",\"contents\":\"Failed\"}"

    it "generates a WireResponse HieError" $ do
      let wr = HieError (String "Funny problem")
      (encode wr) `shouldBe` "{\"tag\":\"HieError\",\"contents\":\"Funny problem\"}"


  describe "JSON Parsing WireResponse" $ do
    it "parses a WireResponse Ok" $ do
      let wr = Ok (String "no problem")
      (decode "{\"tag\":\"Ok\",\"contents\":\"no problem\"}")
          `shouldBe` (Just wr)

    it "parsed a WireResponse Fail" $ do
      let wr = Fail (String "Failed")
      (decode "{\"tag\":\"Fail\",\"contents\":\"Failed\"}")
          `shouldBe` (Just wr)

    it "parsed a WireResponse HieError" $ do
      let wr = HieError (String "Funny problem")
      (decode "{\"tag\":\"HieError\",\"contents\":\"Funny problem\"}")
          `shouldBe` (Just wr)

