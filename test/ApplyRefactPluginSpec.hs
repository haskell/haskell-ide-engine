{-# LANGUAGE OverloadedStrings #-}
module ApplyRefactPluginSpec where

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Data.Aeson
import           Data.Algorithm.Diff
import qualified Data.Map as Map
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.SemanticTypes
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.ApplyRefactPlugin
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
testPlugins = Map.fromList [("applyrefact",applyRefactDescriptor)]

-- TODO: break this out into a TestUtils file
dispatchRequest :: IdeRequest -> IO (Maybe (IdeResponse Object))
dispatchRequest req = do
  testChan <- atomically newTChan
  let cr = CReq "applyrefact" 1 req testChan
  r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
  return r

-- ---------------------------------------------------------------------

applyRefactSpec :: Spec
applyRefactSpec = do
  describe "apply-refact plugin commands" $ do

    -- ---------------------------------

    it "renames" $ do

      let req = IdeRequest "applyOne" (Map.fromList [("file",ParamValP $ ParamFile "./test/testdata/ApplyRefact.hs")
                                                    ,("start_pos",ParamValP $ ParamPos (2,8))
                                                    ])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk (jsWrite (RefactorResult [HieDiff
                                                           "test/testdata/HaReRename.hs"
                                                           "test/testdata/HaReRename.refactored.hs"
                                                           [ (First (4,"foo :: Int -> Int"))
                                                           , (First (5,"foo x = x + 3"))
                                                           , (Second (4,"foolong :: Int -> Int"))
                                                           , (Second (5, "foolong x = x + 3"))
                                                           ]
                                                           ])))

    -- ---------------------------------
