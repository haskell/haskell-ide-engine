{-# LANGUAGE OverloadedStrings #-}
module CompletionSpec where

import Control.Applicative.Combinators
import Control.Monad.IO.Class
import Control.Lens
import Language.Haskell.LSP.Test
-- import Language.Haskell.LSP.Test.Replay
import Language.Haskell.LSP.Types
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "completions" $
  it "works" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
    doc <- openDoc "Completion.hs" "haskell"
    _ <- skipManyTill loggingNotification (count 2 noDiagnostics)

    let te = TextEdit (Range (Position 1 7) (Position 1 24)) "putSt"
    _ <- applyEdit doc te

    compls <- getCompletions doc (Position 1 9)
    let item = head $ filter ((== "putStrLn") . (^. label)) compls
    liftIO $ do
      item ^. label `shouldBe` "putStrLn"
      item ^. kind `shouldBe` Just CiFunction
      item ^. detail `shouldBe` Just "String -> IO ()\nPrelude"
  --TODO: Replay session
