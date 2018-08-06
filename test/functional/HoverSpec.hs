{-# LANGUAGE OverloadedStrings #-}
module HoverSpec where

import Control.Applicative.Combinators
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Text as T
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "hover" $
  it "works" $ runSession hieCommand fullCaps "test/testdata" $ do
    doc <- openDoc "Hover.hs" "haskell"
    _ <- skipManyTill loggingNotification $ count 2 noDiagnostics
    Just hover <- getHover doc (Position 1 19)
    liftIO $ do
      hover ^. range `shouldBe` Just (Range (Position 1 16) (Position 1 19))
      let hasType (CodeString (LanguageString "haskell" "sum :: [Int] -> Int")) = True
          hasType _ = False

          sumDoc = "The `sum` function computes the sum of the numbers of a structure."

          hasDoc (PlainString s) = sumDoc `T.isInfixOf` s
          hasDoc _               = False
      hover ^. contents `shouldSatisfy` any hasType
      hover ^. contents `shouldSatisfy` any hasDoc
