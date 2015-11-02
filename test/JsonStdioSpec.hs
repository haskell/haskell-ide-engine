{-# LANGUAGE OverloadedStrings #-}
module JsonStdioSpec where

import           Data.Aeson
import           Haskell.Ide.PluginDescriptor
import           Haskell.Ide.Transport.JsonStdio

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
      let wr = WireReq "eg1:hello" NoSession NoContext Map.empty
      (encode wr) `shouldBe` "{\"context\":{\"tag\":\"NoContext\",\"contents\":[]},\
                              \\"params\":{},\
                              \\"cmd\":\"eg1:hello\",\
                              \\"session\":{\"tag\":\"NoSession\",\"contents\":[]}}"

    it "generates a WireRequest 2" $ do
      let wr = WireReq "eg2:helloTo" (SimpleSession "/home/foo/Bar.hs") (RowCol 3 4) (Map.fromList [("name","foo")])
      (encode wr) `shouldBe` "{\"context\":{\"tag\":\"RowCol\",\"contents\":[3,4]},\
                              \\"params\":{\"name\":\"foo\"},\
                              \\"cmd\":\"eg2:helloTo\",\
                              \\"session\":{\"tag\":\"SimpleSession\",\"contents\":\"/home/foo/Bar.hs\"}}"

    it "generates a WireRequest 3" $ do
      let wr = WireReq "eg2:helloTo" (CabalSession (CabalSection "test") "/home/foo/Bar.hs")
                                                   (Region (5,6) (7,8)) (Map.fromList [("name","foo"),("p3","val")])
      (encode wr) `shouldBe` "{\"context\":{\"tag\":\"Region\",\"contents\":[[5,6],[7,8]]},\
                              \\"params\":{\"name\":\"foo\",\"p3\":\"val\"},\
                              \\"cmd\":\"eg2:helloTo\",\
                              \\"session\":{\"tag\":\"CabalSession\",\"contents\":[\"test\",\"/home/foo/Bar.hs\"]}}"

    it "generates a WireRequest 4" $ do
      let wr = WireReq "eg4:hello4" NoSession WholeFile Map.empty
      (encode wr) `shouldBe` "{\"context\":{\"tag\":\"WholeFile\",\"contents\":[]},\
                              \\"params\":{},\
                              \\"cmd\":\"eg4:hello4\",\
                              \\"session\":{\"tag\":\"NoSession\",\"contents\":[]}}"


  describe "JSON Parsing WireRequest" $ do
    it "parses a WireRequest 1" $ do
      let wr = WireReq "eg1:hello" NoSession NoContext Map.empty
      (decode "{\"context\":{\"tag\":\"NoContext\",\"contents\":[]},\
               \\"params\":{},\
               \\"cmd\":\"eg1:hello\",\
               \\"session\":{\"tag\":\"NoSession\",\"contents\":[]}}")
         `shouldBe` (Just wr)

    it "parses a WireRequest 2" $ do
      let wr = WireReq "eg2:helloTo" (SimpleSession "/home/foo/Bar.hs") (RowCol 3 4) (Map.fromList [("name","foo")])
      (decode "{\"context\":{\"tag\":\"RowCol\",\"contents\":[3,4]},\
               \\"params\":{\"name\":\"foo\"},\
               \\"cmd\":\"eg2:helloTo\",\
               \\"session\":{\"tag\":\"SimpleSession\",\"contents\":\"/home/foo/Bar.hs\"}}")
         `shouldBe` (Just wr)

    it "parses a WireRequest 3" $ do
      let wr = WireReq "eg2:helloTo" (CabalSession (CabalSection "test") "/home/foo/Bar.hs")
                                                   (Region (5,6) (7,8)) (Map.fromList [("name","foo"),("p3","val")])
      (decode "{\"context\":{\"tag\":\"Region\",\"contents\":[[5,6],[7,8]]},\
               \\"params\":{\"name\":\"foo\",\"p3\":\"val\"},\
               \\"cmd\":\"eg2:helloTo\",\
               \\"session\":{\"tag\":\"CabalSession\",\"contents\":[\"test\",\"/home/foo/Bar.hs\"]}}")
         `shouldBe` (Just wr)

    it "parses a WireRequest 4" $ do
      let wr = WireReq "eg4:hello4" NoSession WholeFile Map.empty
      (decode "{\"context\":{\"tag\":\"WholeFile\",\"contents\":[]},\
               \\"params\":{},\
               \\"cmd\":\"eg4:hello4\",\
               \\"session\":{\"tag\":\"NoSession\",\"contents\":[]}}")
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
