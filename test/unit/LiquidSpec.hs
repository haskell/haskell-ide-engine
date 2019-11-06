{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module LiquidSpec where

import           Data.Aeson
import           Data.List
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Data.Monoid ((<>))
import           Data.Maybe (isJust)
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Plugin.Liquid
import           System.Directory
import           System.Exit
import           System.FilePath
import           Test.Hspec
import Control.Monad.IO.Class

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "liquid haskell plugin" $ do
    cwd <- runIO getCurrentDirectory

    -- ---------------------------------

    it "finds liquid haskell exe in $PATH" $ findExecutable "liquid" >>= (`shouldSatisfy` isJust)

    -- ---------------------------------

    -- This produces some products in /test/testdata/liquid/.liquid/
    -- that are used in subsequent test
    it "runs the liquid haskell exe" $ do
      let
        fp = cwd </> "test/testdata/liquid/Evens.hs"
      Just (ef, (msg:_)) <- runLiquidHaskell fp
      liftIO $ putStrLn $ "msg=" ++ msg
      msg `shouldSatisfy` isPrefixOf "RESULT\n[{\"start\":{\"line\""
      ef `shouldBe` ExitFailure 1

    -- ---------------------------------
    it "gets annot file paths" $ do
      let
        uri = filePathToUri $ cwd </> "test/testdata/liquid/Evens.hs"
        vimFile  = vimAnnotFile uri
        jsonFile = jsonAnnotFile uri
      vimFile  `shouldBe` normalise (cwd </> "test/testdata/liquid/.liquid/Evens.hs.vim.annot")
      jsonFile `shouldBe` normalise (cwd </> "test/testdata/liquid/.liquid/Evens.hs.json")

    -- ---------------------------------

    it "reads errors from json file" $ do
      let
        uri = filePathToUri $ cwd </> "test/testdata/liquid/Evens.hs"
        jsonFile = jsonAnnotFile uri
      jf <- BS.readFile jsonFile
      let Just v = decode jf :: Maybe LiquidJson
      let [LE { start, stop, message }] = errors v
      start `shouldBe` LP 9 1
      stop `shouldBe` LP 9 8
      message `shouldSatisfy` T.isPrefixOf
               ("Error: Liquid Type Mismatch\n  Inferred type\n" <>
                "    VV : {v : Int | v == (7 : int)}\n \n" <>
                "  not a subtype of Required type\n" <>
                "    VV : {VV : Int | VV mod 2 == 0}\n")

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
