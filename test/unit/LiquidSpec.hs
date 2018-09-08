{-# LANGUAGE OverloadedStrings #-}
module LiquidSpec where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.List
import qualified Data.Text.IO         as T
import           Data.Monoid ((<>))
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Plugin.Liquid
import           System.Directory
import           System.Exit
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

    -- ---------------------------------

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

    -- ---------------------------------

    it "gracefully manages missing json file" $ do
      let
        uri = filePathToUri $ cwd </> "test/testdata/Rename.hs"
      n <- readJsonAnnot uri
      n `shouldBe` Nothing

    -- ---------------------------------

    it "parses a vim annotation" $ do
      parseType "6:1-6:10::Main.weAreEven :: \"[{v : GHC.Types.Int | v mod 2 == 0}]\""
        `shouldBe`
          [LE (LP 6 1) (LP 6 10) "[{v : GHC.Types.Int | v mod 2 == 0}]"]
    -- ---------------------------------

    it "parses multiple vim annotations" $ do
      parseType "1:1-1:1::Main.$trModule :: \"GHC.Types.Module\"\n6:1-6:10::Main.weAreEven :: \"[{v : GHC.Types.Int | v mod 2 == 0}]\""
        `shouldBe`
          [LE (LP 1 1) (LP 1 1) "GHC.Types.Module"
          ,LE (LP 6 1) (LP 6 10) "[{v : GHC.Types.Int | v mod 2 == 0}]"]

    -- ---------------------------------

    it "reads types from vim.annot file" $ do
      let
        uri = filePathToUri $ cwd </> "test/testdata/liquid/Evens.hs"
        vimFile = vimAnnotFile uri
      vf <- T.readFile vimFile
      let ts = parseType vf
      take 2 ts
        `shouldBe`
          [LE (LP 1 1) (LP 1 1) "GHC.Types.Module"
          ,LE (LP 6 1) (LP 6 10) "[{v : GHC.Types.Int | v mod 2 == 0}]"]
      length ts `shouldBe` 38

    -- ---------------------------------

    it "reads types from vim.annot file 2" $ do
      let
        uri = filePathToUri $ cwd </> "test/testdata/liquid/Evens.hs"
      Just ts <- readVimAnnot uri
      take 2 ts
        `shouldBe`
          [LE (LP 1 1) (LP 1 1) "GHC.Types.Module"
          ,LE (LP 6 1) (LP 6 10) "[{v : GHC.Types.Int | v mod 2 == 0}]"]
      length ts `shouldBe` 38

    -- ---------------------------------

    it "gracefully manages missing vim.annot file" $ do
      let
        uri = filePathToUri $ cwd </> "test/testdata/Rename.hs"
      n <- readVimAnnot uri
      n `shouldBe` Nothing

    -- ---------------------------------

    it "runs the liquid haskell exe" $ do
      let
        fp = cwd </> "test/testdata/liquid/Evens.hs"
        -- fp = "/home/alanz/tmp/haskell-proc-play/Evens.hs"
        -- uri = filePathToUri fp
      Just (ef, (msg:_)) <- runLiquidHaskell fp
      msg `shouldSatisfy` isPrefixOf "RESULT\n[{\"start\":{\"line\":9,\"column\":1},\"stop\":{\"line\":9,\"column\":8},\"message\":\"Error: Liquid Type Mismatch\\n  Inferred type\\n    VV : {v : Int | v == (7 : int)}\\n \\n  not a subtype of Required type\\n    VV : {VV : Int | VV mod 2 == 0}\\n"
      ef `shouldBe` ExitFailure 1

    -- ---------------------------------
