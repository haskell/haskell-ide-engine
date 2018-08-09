{-# LANGUAGE OverloadedStrings #-}
module SymbolsSpec where

import Control.Monad.IO.Class
import Data.Default
import Language.Haskell.LSP.Test as Test
import Language.Haskell.LSP.Types
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "document symbols" $ do

  -- Some common ranges in Symbols.hs
  let fooR = Range (Position 4 0) (Position 4 3)
      barR = Range (Position 5 8) (Position 5 11)
      dogR = Range (Position 6 17) (Position 6 20)
      catR = Range (Position 6 22) (Position 6 25)
      myDataR = Range (Position 8 5) (Position 8 11)
      aR = Range (Position 8 14) (Position 8 15)
      bR = Range (Position 9 14) (Position 9 15)

  describe "3.10 hierarchical document symbols" $ do
    it "provides nested data types and constructors" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "Symbols.hs" "haskell"
      Left symbs <- getDocumentSymbols doc

      let myData = DocumentSymbol "MyData" (Just "") SkClass Nothing myDataR myDataR (Just (List [a, b]))
          a = DocumentSymbol "A" (Just "") SkConstructor Nothing aR aR (Just mempty)
          b = DocumentSymbol "B" (Just "") SkConstructor Nothing bR bR (Just mempty)

      liftIO $ symbs `shouldContain` [myData]

    it "provides nested where functions" $ runSession hieCommand fullCaps "test/testdata" $ do
      doc <- openDoc "Symbols.hs" "haskell"
      Left symbs <- getDocumentSymbols doc

      let foo = DocumentSymbol "foo" (Just "") SkFunction Nothing fooR fooR (Just (List [bar]))
          bar = DocumentSymbol "bar" (Just "") SkFunction Nothing barR barR (Just (List [dog, cat]))
          dog = DocumentSymbol "dog" (Just "") SkVariable Nothing dogR dogR (Just mempty)
          cat = DocumentSymbol "cat" (Just "") SkVariable Nothing catR catR (Just mempty)

      liftIO $ symbs `shouldContain` [foo]

    -- TODO: Test module, imports

  describe "pre 3.10 symbol information" $ do
    it "provides nested data types and constructors" $ runSession hieCommand oldCaps "test/testdata" $ do
      doc@(TextDocumentIdentifier testUri) <- openDoc "Symbols.hs" "haskell"
      Right symbs <- getDocumentSymbols doc

      let myData = SymbolInformation "MyData" SkClass Nothing (Location testUri myDataR) Nothing
          a = SymbolInformation "A" SkConstructor Nothing (Location testUri aR) (Just "MyData")
          b = SymbolInformation "B" SkConstructor Nothing (Location testUri bR) (Just "MyData")

      liftIO $ symbs `shouldContain` [myData, a, b]

    it "provides nested where functions" $ runSession hieCommand oldCaps "test/testdata" $ do
      doc@(TextDocumentIdentifier testUri) <- openDoc "Symbols.hs" "haskell"
      Right symbs <- getDocumentSymbols doc

      let foo = SymbolInformation "foo" SkFunction Nothing (Location testUri fooR) Nothing
          bar = SymbolInformation "bar" SkFunction Nothing (Location testUri barR) (Just "foo")
          dog = SymbolInformation "dog" SkVariable Nothing (Location testUri dogR) (Just "bar")
          cat = SymbolInformation "cat" SkVariable Nothing (Location testUri catR) (Just "bar")

      -- Order is important!
      liftIO $ symbs `shouldContain` [foo, bar, dog, cat]


-- TODO: Replace with capsForVersion
oldCaps :: ClientCapabilities
oldCaps = def { Test._textDocument = Just tdcs }
  where tdcs = def { _documentSymbol = Just dscs }
        dscs = DocumentSymbolClientCapabilities Nothing Nothing Nothing
