module Main where

import Test.Hspec
import HIE.Bios
import HIE.Bios.Check
import HIE.Bios.Types

import System.FilePath

import Control.Exception as E
import Data.List (isPrefixOf)
import System.Directory
import System.FilePath (addTrailingPathSeparator)

withDirectory_ :: FilePath -> IO a -> IO a
withDirectory_ dir action = bracket getCurrentDirectory
                                    setCurrentDirectory
                                    (\_ -> setCurrentDirectory dir >> action)

withDirectory :: FilePath -> (FilePath -> IO a) -> IO a
withDirectory dir action = bracket getCurrentDirectory
                                   setCurrentDirectory
                                   (\d -> setCurrentDirectory dir >> action d)

main :: IO ()
main = hspec $ do
  describe "cradle" $ do
    it "cabal" $ do
      withDirectory_ "test/data/cabal-test" $ do
        cradle <- findCradle "src/"
        res <- checkSyntax defaultOptions cradle ["src/A.hs"]
        res `shouldBe` "Dummy:0:0:Error:[1 of 1] Compiling A                ( src/A.hs, nothing )\nsrc/A.hs:3:1:Warning: Top-level binding with no type signature: a :: ()\n"
