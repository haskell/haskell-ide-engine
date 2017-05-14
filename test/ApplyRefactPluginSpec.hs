{-# LANGUAGE OverloadedStrings #-}
module ApplyRefactPluginSpec where

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.Map as Map
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.SemanticTypes
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.ApplyRefactPlugin
import           TestUtils

import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "apply-refact plugin" applyRefactSpec

-- -- |Used when running from ghci, and it sets the current directory to ./tests
-- tt :: IO ()
-- tt = do
--   cd ".."
--   hspec spec

-- ---------------------------------------------------------------------

testPlugins :: Plugins
testPlugins = Map.fromList [("applyrefact",untagPluginDescriptor applyRefactDescriptor)]

-- TODO: break this out into a TestUtils file
dispatchRequest :: IdeRequest -> IO (Maybe (IdeResponse Object))
dispatchRequest req = do
  testChan <- atomically newTChan
  let cr = CReq "applyrefact" 1 req testChan
  r <- runIdeM testOptions (IdeState Map.empty Map.empty) (doDispatch testPlugins cr)
  return r

-- ---------------------------------------------------------------------

applyRefactSpec :: Spec
applyRefactSpec = do
  describe "apply-refact plugin commands" $ do

    -- ---------------------------------

    it "applies one hint only" $ do

      let req = IdeRequest "applyOne" (Map.fromList [("file",ParamValP $ ParamFile "./test/testdata/ApplyRefact.hs")
                                                    ,("start_pos",ParamValP $ ParamPos (toPos (2,8)))
                                                    ])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk (jsWrite (HieDiff
                                      { dFirst = "./test/testdata/ApplyRefact.hs"
                                      , dSecond = "changed"
                                      , dDiff =
                                        ("2c2\n"++
                                         "< main = (putStrLn \"hello\")\n"++
                                         "---\n"++
                                         "> main = putStrLn \"hello\"\n")
                                      }
                                     )))

    -- ---------------------------------

    it "applies all hints" $ do

      let req = IdeRequest "applyAll" (Map.fromList [("file",ParamValP $ ParamFile "./test/testdata/ApplyRefact.hs")
                                                    ])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk (jsWrite (HieDiff
                                      { dFirst = "./test/testdata/ApplyRefact.hs"
                                      , dSecond = "changed"
                                      , dDiff =
                                        ("2c2\n"++
                                         "< main = (putStrLn \"hello\")\n"++
                                         "---\n"++
                                         "> main = putStrLn \"hello\"\n"++
                                         "4c4\n"++
                                         "< foo x = (x + 1)\n"++
                                         "---\n"++
                                         "> foo x = x + 1\n")
                                      }
                                     )))

    -- ---------------------------------

    it "returns hints as diagnostics" $ do

      let req = IdeRequest "lint" (Map.fromList [("file",ParamValP $ ParamFile "./test/testdata/ApplyRefact.hs")
                                                ])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk (jsWrite (FileDiagnostics
                                      { fdFileName = "file://./test/testdata/ApplyRefact.hs"
                                      , fdDiagnostics =
                                        [ Diagnostic (Range (Position 1 7) (Position 1 25))
                                                     (Just DsHint)
                                                     Nothing
                                                     (Just "hlint")
                                                     "Redundant bracket\nFound:\n  (putStrLn \"hello\")\nWhy not:\n  putStrLn \"hello\"\n"
                                        , Diagnostic (Range (Position 3 8) (Position 3 15))
                                                     (Just DsHint)
                                                     Nothing
                                                     (Just "hlint")
                                                     "Redundant bracket\nFound:\n  (x + 1)\nWhy not:\n  x + 1\n"
                                        ]
                                      }
                                     )))

    -- ---------------------------------
