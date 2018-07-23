{-# LANGUAGE OverloadedStrings #-}
module FormatSpec where

import Control.Monad.IO.Class
import qualified Data.Text as T
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Test.Hspec
import TestUtils

spec :: Spec
spec = do
  describe "format document" $ do
    it "works" $ runSession hieCommand "test/testdata" $ do
      doc <- openDoc "Format.hs" "haskell"
      formatDoc doc (FormattingOptions 2 True)
      documentContents doc >>= liftIO . (`shouldBe` formattedDocTabSize2)
    it "works with custom tab size" $ runSession hieCommand "test/testdata" $ do
      doc <- openDoc "Format.hs" "haskell"
      formatDoc doc (FormattingOptions 5 True)
      documentContents doc >>= liftIO . (`shouldBe` formattedDocTabSize5)

  describe "format range" $ do
    it "works" $ runSession hieCommand "test/testdata" $ do
      doc <- openDoc "Format.hs" "haskell"
      formatRange doc (FormattingOptions 2 True) (Range (Position 1 0) (Position 3 10))
      documentContents doc >>= liftIO . (`shouldBe` formattedRangeTabSize2)
    it "works with custom tab size" $ runSession hieCommand "test/testdata" $ do
      doc <- openDoc "Format.hs" "haskell"
      formatRange doc (FormattingOptions 5 True) (Range (Position 4 0) (Position 7 19))
      documentContents doc >>= liftIO . (`shouldBe` formattedRangeTabSize5)

formattedDocTabSize2 :: T.Text
formattedDocTabSize2 =
  "module Format where\n\
  \foo :: Int -> Int\n\
  \foo 3 = 2\n\
  \foo x = x\n\
  \bar :: String -> IO String\n\
  \bar s = do\n\
  \  x <- return \"hello\"\n\
  \  return \"asdf\"\n\n"

formattedDocTabSize5 :: T.Text
formattedDocTabSize5 =
  "module Format where\n\
  \foo :: Int -> Int\n\
  \foo 3 = 2\n\
  \foo x = x\n\
  \bar :: String -> IO String\n\
  \bar s = do\n\
  \     x <- return \"hello\"\n\
  \     return \"asdf\"\n\n"

formattedRangeTabSize2 :: T.Text
formattedRangeTabSize2 =
  "module    Format where\n\
  \foo :: Int -> Int\n\
  \foo 3 = 2\n\
  \foo x = x\n\
  \bar   :: String ->   IO String\n\
  \bar s =  do\n\
  \      x <- return \"hello\"\n\
  \      return \"asdf\"\n\
  \      \n"

formattedRangeTabSize5 :: T.Text
formattedRangeTabSize5 =
  "module    Format where\n\
  \foo   :: Int ->  Int\n\
  \foo  3 = 2\n\
  \foo    x  = x\n\
  \bar :: String -> IO String\n\
  \bar s = do\n\
  \     x <- return \"hello\"\n\
  \     return \"asdf\"\n\
  \      \n"
