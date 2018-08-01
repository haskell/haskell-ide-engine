{-# LANGUAGE OverloadedStrings #-}
module LiquidSpec where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Monoid ((<>))
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Plugin.Liquid
import           System.Directory
import           System.FilePath
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "liquid haskell plugin" $ do
    cwd <- runIO getCurrentDirectory

    -- ---------------------------------

    it "gets annot file paths" $ do
      let
        uri = filePathToUri $ cwd </> "test/testdata/liquid/Evens.hs"
        vimFile  = vimAnnotFile uri
        jsonFile = jsonAnnotFile uri
      vimFile  `shouldBe` (cwd </> "test/testdata/liquid/.liquid/Evens.hs.vim.annot")
      jsonFile `shouldBe` (cwd </> "test/testdata/liquid/.liquid/Evens.hs.json")

    it "reads errors from json file" $ do
      let
        uri = filePathToUri $ cwd </> "test/testdata/liquid/Evens.hs"
        jsonFile = jsonAnnotFile uri
      jf <- BS.readFile jsonFile
      let Just v = decode jf :: Maybe LiquidJson
      (errors v) `shouldBe`
         [LE { start = LP 9 1
             , stop  = LP 9 8
             , message =
                 ("Error: Liquid Type Mismatch\n  Inferred type\n" <>
                  "    VV : {v : Int | v == (7 : int)}\n \n" <>
                  "  not a subtype of Required type\n" <>
                  "    VV : {VV : Int | VV mod 2 == 0}\n \n  In Context")
             }
         ]
