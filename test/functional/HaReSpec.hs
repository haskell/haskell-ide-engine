{-# LANGUAGE OverloadedStrings #-}
module HaReSpec where

import Control.Applicative.Combinators
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text as T
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types hiding (error, context)
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "HaRe" $
  context "code actions" $ do
    context "lift one level" $
      it "works" $
        let r = Range (Position 2 8) (Position 2 17)
            expected =
              "module HaReLift where\n\
              \foo = bar\n\n\
              \bar = \"hello\""
          in execCodeAction "HaReLift.hs" r "Lift bar one level" expected
    context "lift to top level" $
      it "works" $
          let r = Range (Position 2 8) (Position 2 17)
              expected =
                "module HaReLift where\n\
                \foo = bar\n\n\
                \bar = \"hello\""
            in execCodeAction "HaReLift.hs" r "Lift bar to top level" expected
    context "delete definition" $
      it "works" $
        let r = Range (Position 1 0) (Position 1 4)
            expected = "module HaReLift where\n"
          in execCodeAction "HaReLift.hs" r "Delete definition of foo" expected
    context "duplicate definition" $
      it "works" $
        let r = Range (Position 1 0) (Position 1 4)
            expected =
              "module HaReLift where\n\
              \foo = bar\n\
              \  where bar = \"hello\"\n\
              \foo' = bar\n\
              \  where bar = \"hello\"\n"
          in execCodeAction "HaReLift.hs" r "Duplicate definition of foo" expected
    context "demote definition" $ it "works" $
      let r = Range (Position 5 0) (Position 5 1)
          expected = "\nmain = putStrLn \"hello\"\n\n\
                     \foo x = y + 3\n  where\n    y = 7\n"
        in execCodeAction "HaReDemote.hs" r "Demote y one level" expected


getCANamed :: T.Text -> [CAResult] -> CodeAction
getCANamed named = head . mapMaybe test
  where test (CACodeAction ca@(CodeAction t _ _ _ _))
          | named `T.isInfixOf` t = Just ca
          | otherwise = Nothing
        test _ = Nothing

execCodeAction :: String -> Range -> T.Text -> T.Text -> IO ()
execCodeAction fp r n expected = runSession hieCommand fullCaps "test/testdata" $ do
  doc <- openDoc fp "haskell"

  -- Code actions aren't deferred - need to wait for compilation
  _ <- count 2 waitForDiagnostics

  ca <- getCANamed n <$> getCodeActions doc r
  executeCodeAction ca

  content <- getDocumentEdit doc

  liftIO $ content `shouldBe` expected
