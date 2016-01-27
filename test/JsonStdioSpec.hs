{-# LANGUAGE OverloadedStrings #-}
module JsonStdioSpec where

import           Data.Aeson
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Transport.JsonStdio
import           Haskell.Ide.Engine.Transport.Pipes

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
      (encode wr) `shouldBe` "{\"params\":{\"cabal\":{\"text\":\"lib\"},\
                                          \\"name\":{\"text\":\"foo\"}},\
                              \\"cmd\":\"eg2:helloTo\"}"

  describe "JSON Parsing WireRequest" $ do
    it "parses a WireRequest 1" $ do
      let wr = WireReq "eg1:hello" Map.empty
      (decode "{\"params\":{},\
               \\"cmd\":\"eg1:hello\"}")
         `shouldBe` (Just wr)

    it "parses a WireRequest 2" $ do
      let wr = WireReq "eg2:helloTo" (Map.fromList [("name",ParamValP $ ParamText "foo")])
      (decode "{\"params\":{\"name\":{\"text\":\"foo\"}},\
               \\"cmd\":\"eg2:helloTo\"}")
         `shouldBe` (Just wr)

  describe "Processes one command only in --one-shot mode" $ do
    it "exits after recieving one message, and sending one reply" $ do
      pendingWith "write this test"
