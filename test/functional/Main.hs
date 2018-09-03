module Main where

import Test.Hspec
import qualified FunctionalSpec
import TestUtils

main :: IO ()
main = do
  setupStackFiles
  -- withFileLogging "functional.log" $ hspec FunctionalSpec.spec
  withFileLogging logFilePath $ hspec FunctionalSpec.spec
