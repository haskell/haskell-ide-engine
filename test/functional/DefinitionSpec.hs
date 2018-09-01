module DefinitionSpec where

import Control.Lens
import Control.Monad.IO.Class
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import System.Directory
import Test.Hspec
import TestUtils
import Utils

spec :: Spec
spec = describe "definitions" $ do
  it "goto's symbols" $ runSessionWithConfig noLogConfig hieCommand fullCaps "test/testdata" $ do
    doc <- openDoc "References.hs" "haskell"
    defs <- getDefinitions doc (Position 7 8)
    let expRange = Range (Position 4 0) (Position 4 3)
    liftIO $ defs `shouldBe` [Location (doc ^. uri) expRange]

  it "goto's imported modules" $ runSessionWithConfig noLogConfig hieCommand fullCaps "test/testdata/definition" $ do
    doc <- openDoc "Foo.hs" "haskell"
    defs <- getDefinitions doc (Position 2 8)
    liftIO $ do
      fp <- canonicalizePath "test/testdata/definition/Bar.hs"
      defs `shouldBe` [Location (filePathToUri fp) zeroRange]

  it "goto's exported modules" $ runSessionWithConfig noLogConfig hieCommand fullCaps "test/testdata/definition" $ do
    doc <- openDoc "Foo.hs" "haskell"
    defs <- getDefinitions doc (Position 0 15)
    liftIO $ do
      fp <- canonicalizePath "test/testdata/definition/Bar.hs"
      defs `shouldBe` [Location (filePathToUri fp) zeroRange]

  it "goto's imported modules that are loaded" $ runSessionWithConfig noLogConfig hieCommand fullCaps "test/testdata/definition" $ do
    doc <- openDoc "Foo.hs" "haskell"
    _ <- openDoc "Bar.hs" "haskell"
    defs <- getDefinitions doc (Position 2 8)
    liftIO $ do
      fp <- canonicalizePath "test/testdata/definition/Bar.hs"
      defs `shouldBe` [Location (filePathToUri fp) zeroRange]

  it "goto's imported modules that are loaded, and then closed" $
    runSessionWithConfig noLogConfig hieCommand fullCaps "test/testdata/definition" $ do
      doc <- openDoc "Foo.hs" "haskell"
      otherDoc <- openDoc "Bar.hs" "haskell"
      closeDoc otherDoc
      defs <- getDefinitions doc (Position 2 8)
      liftIO $ do
        fp <- canonicalizePath "test/testdata/definition/Bar.hs"
        defs `shouldBe` [Location (filePathToUri fp) zeroRange]

zeroRange :: Range
zeroRange = Range (Position 0 0) (Position 0 0)
