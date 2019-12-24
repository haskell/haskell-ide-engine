module Main where

import Control.Monad.IO.Class (liftIO)
import Haskell.Ide.Engine.Cradle (findLocalCradle)
import Haskell.Ide.Engine.Version
import Test.Hspec
import System.Directory
import System.FilePath
import System.Process

main :: IO ()
main = hspec $
  describe "version checking" $ do
    it "picks up a stack.yaml with 8.8.1" $
      withCurrentDirectory "test/testdata/wrapper/8.8.1" $ do
        d <- getCurrentDirectory
        cradle <- liftIO (findLocalCradle (d </> "File.hs"))
        getProjectGhcVersion cradle `shouldReturn` "8.8.1"
    it "picks up a stack.yaml with 8.6.5" $
      withCurrentDirectory "test/testdata/wrapper/lts-14.18" $ do
        d <- getCurrentDirectory
        cradle <- liftIO (findLocalCradle (d </> "File.hs"))
        getProjectGhcVersion cradle `shouldReturn` "8.6.5"
    it "picks up whatever version of ghc is on this machine" $
      withCurrentDirectory "test/testdata/wrapper/ghc" $ do
        d <- getCurrentDirectory
        cradle <- liftIO (findLocalCradle (d </> "File.hs"))
        ghcDisplayVer <- readCreateProcess (shell "ghc --version") ""
        ghcVer <- getProjectGhcVersion cradle
        init ghcDisplayVer `shouldEndWith` ghcVer


