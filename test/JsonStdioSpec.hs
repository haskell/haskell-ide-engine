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
      let wr = WireReq "eg1:hello" emptyContext Map.empty
      (encode wr) `shouldBe` "{\"context\":{\"ctxEndPos\":null,\
                                           \\"ctxCabal\":null,\
                                           \\"ctxStartPos\":null,\
                                           \\"ctxFile\":null},\
                              \\"params\":{},\
                              \\"cmd\":\"eg1:hello\"}"

    it "generates a WireRequest 2" $ do
      let wr = WireReq "eg2:helloTo" (Context (Just (CabalSection "lib")) Nothing Nothing Nothing ) (Map.fromList [("name","foo")])
      (encode wr) `shouldBe` "{\"context\":{\"ctxEndPos\":null,\
                                           \\"ctxCabal\":\"lib\",\
                                           \\"ctxStartPos\":null,\
                                           \\"ctxFile\":null},\
                              \\"params\":{\"name\":\"foo\"},\
                              \\"cmd\":\"eg2:helloTo\"}"

    it "generates a WireRequest 3" $ do
      let wr = WireReq "eg2:helloTo" (Context (Just (CabalSection "lib")) (Just "/home/foo/Bar.hs") Nothing Nothing )
                                                    (Map.fromList [("name","foo"),("p3","val")])
      (encode wr) `shouldBe` "{\"context\":{\"ctxEndPos\":null,\
                                           \\"ctxCabal\":\"lib\",\
                                           \\"ctxStartPos\":null,\
                                           \\"ctxFile\":\"/home/foo/Bar.hs\"},\
                              \\"params\":{\"name\":\"foo\",\"p3\":\"val\"},\
                              \\"cmd\":\"eg2:helloTo\"}"

    it "generates a WireRequest 4" $ do
      let wr = WireReq "eg4:hello4" (Context (Just (CabalSection "lib")) (Just "/home/foo/Bar.hs") (Just (1,2)) Nothing ) Map.empty
      (encode wr) `shouldBe` "{\"context\":{\"ctxEndPos\":null,\
                                           \\"ctxCabal\":\"lib\",\
                                           \\"ctxStartPos\":[1,2],\
                                           \\"ctxFile\":\"/home/foo/Bar.hs\"},\
                              \\"params\":{},\
                              \\"cmd\":\"eg4:hello4\"}"

    it "generates a WireRequest 5" $ do
      let wr = WireReq "eg5:hello5" (Context (Just (CabalSection "lib")) (Just "/home/foo/Bar.hs") (Just (1,2)) (Just (3,4)) ) Map.empty
      (encode wr) `shouldBe` "{\"context\":{\"ctxEndPos\":[3,4],\
                                           \\"ctxCabal\":\"lib\",\
                                           \\"ctxStartPos\":[1,2],\
                                           \\"ctxFile\":\"/home/foo/Bar.hs\"},\
                              \\"params\":{},\
                              \\"cmd\":\"eg5:hello5\"}"

  describe "JSON Parsing WireRequest" $ do
    it "parses a WireRequest 1" $ do
      let wr = WireReq "eg1:hello" emptyContext Map.empty
      (decode "{\"context\":{\"ctxEndPos\":null,\
                            \\"ctxCabal\":null,\
                            \\"ctxStartPos\":null,\
                            \\"ctxFile\":null},\
               \\"params\":{},\
               \\"cmd\":\"eg1:hello\"}")
         `shouldBe` (Just wr)

    it "parses a WireRequest 2" $ do
      let wr = WireReq "eg2:helloTo" (Context (Just (CabalSection "lib")) Nothing Nothing Nothing ) (Map.fromList [("name","foo")])
      (decode "{\"context\":{\"ctxEndPos\":null,\
                            \\"ctxCabal\":\"lib\",\
                            \\"ctxStartPos\":null,\
                            \\"ctxFile\":null},\
               \\"params\":{\"name\":\"foo\"},\
               \\"cmd\":\"eg2:helloTo\"}")
         `shouldBe` (Just wr)

    it "parses a WireRequest 3" $ do
      let wr = WireReq "eg2:helloTo" (Context (Just (CabalSection "lib")) (Just "/home/foo/Bar.hs") Nothing Nothing )
                                                    (Map.fromList [("name","foo"),("p3","val")])
      (decode "{\"context\":{\"ctxEndPos\":null,\
                            \\"ctxCabal\":\"lib\",\
                            \\"ctxStartPos\":null,\
                            \\"ctxFile\":\"/home/foo/Bar.hs\"},\
               \\"params\":{\"name\":\"foo\",\"p3\":\"val\"},\
               \\"cmd\":\"eg2:helloTo\"}")
         `shouldBe` (Just wr)

    it "parses a WireRequest 4" $ do
      let wr = WireReq "eg4:hello4" (Context (Just (CabalSection "lib")) (Just "/home/foo/Bar.hs") (Just (1,2)) Nothing ) Map.empty
      (decode "{\"context\":{\"ctxEndPos\":null,\
                            \\"ctxCabal\":\"lib\",\
                            \\"ctxStartPos\":[1,2],\
                            \\"ctxFile\":\"/home/foo/Bar.hs\"},\
               \\"params\":{},\
               \\"cmd\":\"eg4:hello4\"}")
         `shouldBe` (Just wr)

    it "parses a WireRequest 5" $ do
      let wr = WireReq "eg5:hello5" (Context (Just (CabalSection "lib")) (Just "/home/foo/Bar.hs") (Just (1,2)) (Just (3,4)) ) Map.empty
      (decode "{\"context\":{\"ctxEndPos\":[3,4],\
                            \\"ctxCabal\":\"lib\",\
                            \\"ctxStartPos\":[1,2],\
                            \\"ctxFile\":\"/home/foo/Bar.hs\"},\
               \\"params\":{},\
               \\"cmd\":\"eg5:hello5\"}")
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

