module DefinitionSpec where

-- import Control.Applicative.Combinators
import Control.Lens
import Control.Monad.IO.Class
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens
import System.Directory
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "definitions" $ do
  it "goto's symbols" $ runSession hieCommand fullCaps "test/testdata" $ do
    doc <- openDoc "References.hs" "haskell"
    defs <- getDefinitions doc (Position 7 8)
    let expRange = Range (Position 4 0) (Position 4 3)
    liftIO $ defs `shouldBe` [Location (doc ^. uri) expRange]

  -- -----------------------------------

  it "goto's imported modules" $ runSession hieCommand fullCaps "test/testdata/definition" $ do
    doc <- openDoc "Foo.hs" "haskell"
    defs <- getDefinitions doc (Position 2 8)
    liftIO $ do
      fp <- canonicalizePath "test/testdata/definition/Bar.hs"
      defs `shouldBe` [Location (filePathToUri fp) zeroRange]

  -- -----------------------------------

  it "goto's exported modules" $ runSession hieCommand fullCaps "test/testdata/definition" $ do
    doc <- openDoc "Foo.hs" "haskell"
    defs <- getDefinitions doc (Position 0 15)
    liftIO $ do
      fp <- canonicalizePath "test/testdata/definition/Bar.hs"
      defs `shouldBe` [Location (filePathToUri fp) zeroRange]

  -- -----------------------------------

  it "goto's imported modules that are loaded" $ runSession hieCommand fullCaps "test/testdata/definition" $ do
    doc <- openDoc "Foo.hs" "haskell"
    _ <- openDoc "Bar.hs" "haskell"
    defs <- getDefinitions doc (Position 2 8)
    liftIO $ do
      fp <- canonicalizePath "test/testdata/definition/Bar.hs"
      defs `shouldBe` [Location (filePathToUri fp) zeroRange]

  -- -----------------------------------

  it "goto's imported modules that are loaded, and then closed" $
    runSession hieCommand fullCaps "test/testdata/definition" $ do
      doc <- openDoc "Foo.hs" "haskell"
      otherDoc <- openDoc "Bar.hs" "haskell"
      closeDoc otherDoc
      defs <- getDefinitions doc (Position 2 8)
      _ <- waitForDiagnostics
      liftIO $ putStrLn "D"
      liftIO $ do
        fp <- canonicalizePath "test/testdata/definition/Bar.hs"
        defs `shouldBe` [Location (filePathToUri fp) zeroRange]
      liftIO $ putStrLn "E" -- AZ

      noDiagnostics


zeroRange :: Range
zeroRange = Range (Position 0 0) (Position 0 0)
