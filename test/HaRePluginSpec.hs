{-# LANGUAGE OverloadedStrings #-}
module HaRePluginSpec where

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
import           Haskell.Ide.HaRePlugin
import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hare plugin" hareSpec

-- -- |Used when running from ghci, and it sets the current directory to ./tests
-- tt :: IO ()
-- tt = do
--   cd ".."
--   hspec spec

-- ---------------------------------------------------------------------

testPlugins :: Plugins
testPlugins = Map.fromList [("hare",hareDescriptor)]

-- TODO: break this out into a TestUtils file
dispatchRequest :: IdeRequest -> IO (Maybe (IdeResponse Object))
dispatchRequest req = do
  testChan <- atomically newTChan
  let cr = CReq "hare" 1 req testChan
  r <- withStdoutLogging $ runIdeM (IdeState Map.empty Map.empty) (doDispatch testPlugins cr)
  return r

-- ---------------------------------------------------------------------

hareSpec :: Spec
hareSpec = do
  describe "hare plugin commands" $ do

    -- ---------------------------------

    it "renames" $ do

      let req = IdeRequest "rename" (Map.fromList [("file",ParamValP $ ParamFile "./test/testdata/HaReRename.hs")
                                                  ,("start_pos",ParamValP $ ParamPos (5,1))
                                                  ,("name",ParamValP $ ParamText "foolong")])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk (jsWrite (RefactorResult [HieDiff
                                                           "test/testdata/HaReRename.hs"
                                                           "test/testdata/HaReRename.refactored.hs"
                                                           ("4,5c4,5\n"++
                                                            "< foo :: Int -> Int\n"++
                                                            "< foo x = x + 3\n"++
                                                            "---\n"++
                                                            "> foolong :: Int -> Int\n"++
                                                            "> foolong x = x + 3\n")
                                                           ])))

    -- ---------------------------------

    it "returns an error for invalid rename" $ do
      let req = IdeRequest "rename" (Map.fromList [("file",ParamValP $ ParamFile "./test/testdata/HaReRename.hs")
                                                  ,("start_pos",ParamValP $ ParamPos (15,1))
                                                  ,("name",ParamValP $ ParamText "foolong")])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseFail
                      (IdeError { ideCode = PluginError
                                , ideMessage = "rename: \"Invalid cursor position!\"", ideInfo = Nothing}))

    -- ---------------------------------

    it "demotes" $ do
      let req = IdeRequest "demote" (Map.fromList [("file",ParamValP $ ParamFile "./test/testdata/HaReDemote.hs")
                                                  ,("start_pos",ParamValP $ ParamPos (6,1))])
      r <- dispatchRequest req
      -- r `shouldBe` Just (IdeResponseOk (H.fromList ["refactor" .= ["test/testdata/HaReDemote.hs"::FilePath]]))
      r `shouldBe` Just (IdeResponseOk $ jsWrite (RefactorResult [HieDiff
                                                           "test/testdata/HaReDemote.hs"
                                                           "test/testdata/HaReDemote.refactored.hs"
                                                           ("5,6c5,6\n"++
                                                            "< \n"++
                                                            "< y = 7\n"++
                                                            "---\n"++
                                                            ">   where\n"++
                                                            ">     y = 7\n")
                                                           -- [ (First (5,""))
                                                           -- , (First (6,"y = 7"))
                                                           -- , (Second (5,"  where"))
                                                           -- , (Second (6,"    y = 7"))
                                                           -- ]
                                                           ]))

    -- ---------------------------------

    it "duplicates a definition" $ do

      let req = IdeRequest "dupdef" (Map.fromList [("file",ParamValP $ ParamFile "./test/testdata/HaReRename.hs")
                                                  ,("start_pos",ParamValP $ ParamPos (5,1))
                                                  ,("name",ParamValP $ ParamText "foonew")])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite (RefactorResult [HieDiff
                                                           "test/testdata/HaReRename.hs"
                                                           "test/testdata/HaReRename.refactored.hs"
                                                           ("6a7,9\n"++
                                                            "> foonew :: Int -> Int\n"++
                                                            "> foonew x = x + 3\n"++
                                                            "> \n")
                                                           ]))

    -- ---------------------------------

    it "converts if to case" $ do

      let req = IdeRequest "iftocase" (Map.fromList [("file",ParamValP $ ParamFile "./test/testdata/HaReCase.hs")
                                                    ,("start_pos",ParamValP $ ParamPos (5,9))
                                                    ,("end_pos",  ParamValP $ ParamPos (9,12))])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite (RefactorResult [HieDiff
                                                           "test/testdata/HaReCase.hs"
                                                           "test/testdata/HaReCase.refactored.hs"
                                                           ("5,9c5,9\n"++
                                                            "< foo x = if odd x\n"++
                                                            "<         then\n"++
                                                            "<           x + 3\n"++
                                                            "<         else\n"++
                                                            "<           x\n"++
                                                            "---\n"++
                                                            "> foo x = case odd x of\n"++
                                                            ">   True  ->\n"++
                                                            ">             x + 3\n"++
                                                            ">   False ->\n>             x\n")
                                                           ]))

    -- ---------------------------------

    it "lifts one level" $ do

      let req = IdeRequest "liftonelevel" (Map.fromList [("file",ParamValP $ ParamFile "./test/testdata/HaReMoveDef.hs")
                                                    ,("start_pos",ParamValP $ ParamPos (6,5))])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite (RefactorResult [HieDiff
                                                           "test/testdata/HaReMoveDef.hs"
                                                           "test/testdata/HaReMoveDef.refactored.hs"
                                                           ("5,6d4\n"++
                                                            "<   where\n"++
                                                            "<     y = 4\n"++
                                                            "7a6,7\n"++
                                                            "> y = 4\n"++
                                                            "> \n")
                                                           ]))

    -- ---------------------------------

    it "lifts to top level" $ do

      let req = IdeRequest "lifttotoplevel" (Map.fromList [("file",ParamValP $ ParamFile "./test/testdata/HaReMoveDef.hs")
                                                          ,("start_pos",ParamValP $ ParamPos (12,9))])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite (RefactorResult [HieDiff
                                                           "test/testdata/HaReMoveDef.hs"
                                                           "test/testdata/HaReMoveDef.refactored.hs"
                                                           ("11,12d10\n"++
                                                            "<       where\n"++
                                                            "<         z = 7\n"++
                                                            "13a12\n"++
                                                            "> z = 7\n"++
                                                            "14a14\n"++
                                                            "> \n")
                                                           ]))

    -- ---------------------------------
