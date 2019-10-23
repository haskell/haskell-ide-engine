{-# LANGUAGE OverloadedStrings #-}
module HoverSpec where

import Control.Applicative.Combinators
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Text as T
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "hover" $
  it "works" $ runSession hieCommand fullCaps "test/testdata" $ do
    doc <- openDoc "Hover.hs" "haskell"
    _ <- count 2 $ skipManyTill loggingNotification noDiagnostics
    Just h <- getHover doc (Position 1 19)
    liftIO $ do
      h ^. range `shouldBe` Just (Range (Position 1 16) (Position 1 19))
      let
          hasType (HoverContents (MarkupContent MkMarkdown s))
            = "\n```haskell\nsum :: [Int] -> Int\n```" `T.isPrefixOf`s
          hasType _ = False

          sumDoc = "The `sum` function computes the sum of the numbers of a structure."

          hasDoc (HoverContents (MarkupContent MkMarkdown s))
            = sumDoc `T.isInfixOf` s
          hasDoc _               = False

      h ^. contents `shouldSatisfy` hasType
      h ^. contents `shouldSatisfy` hasDoc
