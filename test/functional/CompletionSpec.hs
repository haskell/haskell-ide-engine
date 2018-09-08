{-# LANGUAGE OverloadedStrings #-}
module CompletionSpec where

import Control.Applicative.Combinators
import Control.Monad.IO.Class
import Control.Lens
import Language.Haskell.LSP.Test
-- import Language.Haskell.LSP.Test.Replay
import Language.Haskell.LSP.Types hiding (applyEdit)
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "completions" $ do
  it "works" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
    doc <- openDoc "Completion.hs" "haskell"
    _ <- skipManyTill loggingNotification (count 2 noDiagnostics)

    let te = TextEdit (Range (Position 4 7) (Position 4 24)) "put"
    _ <- applyEdit doc te

    compls <- getCompletions doc (Position 4 9)
    let item = head $ filter ((== "putStrLn") . (^. label)) compls
    liftIO $ do
      item ^. label `shouldBe` "putStrLn"
      item ^. kind `shouldBe` Just CiFunction
      item ^. detail `shouldBe` Just "String -> IO ()\nPrelude"
      item ^. insertTextFormat `shouldBe` Just Snippet
      item ^. insertText `shouldBe` Just "putStrLn ${1:String}"
  --TODO: Replay session

  it "completes imports" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
    doc <- openDoc "Completion.hs" "haskell"
    _ <- skipManyTill loggingNotification (count 2 noDiagnostics)

    let te = TextEdit (Range (Position 0 17) (Position 0 26)) "Data.M"
    _ <- applyEdit doc te

    compls <- getCompletions doc (Position 0 22)
    let item = head $ filter ((== "Maybe") . (^. label)) compls
    liftIO $ do
      item ^. label `shouldBe` "Maybe"
      item ^. detail `shouldBe` Just "Data.Maybe"
      item ^. kind `shouldBe` Just CiModule

  it "completes qualified imports" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
    doc <- openDoc "Completion.hs" "haskell"
    _ <- skipManyTill loggingNotification (count 2 noDiagnostics)

    let te = TextEdit (Range (Position 1 17) (Position 0 25)) "Dat"
    _ <- applyEdit doc te

    compls <- getCompletions doc (Position 0 19)
    let item = head $ filter ((== "Data.List") . (^. label)) compls
    liftIO $ do
      item ^. label `shouldBe` "Data.List"
      item ^. detail `shouldBe` Just "Data.List"
      item ^. kind `shouldBe` Just CiModule

  it "provides snippets for polymorphic types" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
    doc <- openDoc "Completion.hs" "haskell"
    _ <- skipManyTill loggingNotification (count 2 noDiagnostics)

    let te = TextEdit (Range (Position 4 7) (Position 4 24)) "fold"
    _ <- applyEdit doc te
    
    compls <- getCompletions doc (Position 4 11)
    let item = head $ filter ((== "foldl") . (^. label)) compls
    liftIO $ do
      item ^. label `shouldBe` "foldl"
      item ^. kind `shouldBe` Just CiFunction
      item ^. insertTextFormat `shouldBe` Just Snippet
      item ^. insertText `shouldBe` Just "foldl ${1:b -> a -> b} ${2:b} ${3:t a}"