module Main where

import Test.Hspec
import qualified Spec
import TestUtils

main :: IO ()
main = do
  setupStackFiles
  withFileLogging "functional.log" $ hspec Spec.spec
