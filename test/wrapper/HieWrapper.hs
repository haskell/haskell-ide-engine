module Main where

import Haskell.Ide.Engine.Plugin.Base
import Test.Hspec
import System.Directory
import System.Process

main :: IO ()
main = hspec $
  describe "version checking" $ do
    it "picks up a stack.yaml with 8.2.1" $
      withCurrentDirectory "test/testdata/wrapper/8.2.1" $
        getProjectGhcVersion `shouldReturn` "8.2.1"
    it "picks up a stack.yaml with 8.2.2" $
      withCurrentDirectory "test/testdata/wrapper/lts-11.14" $
        getProjectGhcVersion `shouldReturn` "8.2.2"
    it "picks up whatever version of ghc is on this machine" $
      withCurrentDirectory "test/testdata/wrapper/ghc" $ do
        ghcDisplayVer <- readCreateProcess (shell "ghc --version") ""
        ghcVer <- getProjectGhcVersion
        init ghcDisplayVer `shouldEndWith` ghcVer
