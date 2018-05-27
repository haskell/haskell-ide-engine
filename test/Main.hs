module Main where

import Control.Monad
import System.Directory
-- import Test.Hspec.Formatters.Jenkins
import Test.Hspec.Runner
import TestUtils
import qualified Spec

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  setupStackFiles
  let logfile = "./test-main.log"
  exists <- doesFileExist logfile
  when exists $ removeFile logfile
  withFileLogging logfile $ hspec Spec.spec

-- main :: IO ()
-- main = do
--   summary <- withFile "results.xml" WriteMode $ \h -> do
--     let c = defaultConfig
--           { configFormatter = xmlFormatter
--           , configHandle = h
--           }
--     hspecWith c Spec.spec
--   unless (summaryFailures summary == 0) $
--     exitFailure

-- ---------------------------------------------------------------------

