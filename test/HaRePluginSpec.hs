{-# LANGUAGE OverloadedStrings #-}
module HaRePluginSpec where

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.Map as Map
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.SemanticTypes
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.HaRePlugin
import           System.Directory
import           System.FilePath
import           TestUtils

import           Test.Hspec

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}
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
testPlugins = Map.fromList [("hare",untagPluginDescriptor hareDescriptor)]

-- TODO: break this out into a TestUtils file
dispatchRequest :: IdeRequest -> IO (Maybe (IdeResponse Object))
dispatchRequest req = do
  testChan <- atomically newTChan
  let cr = CReq "hare" 1 req testChan
  r <- cdAndDo "./test/testdata" $ withStdoutLogging
    $ runIdeM testOptions (IdeState Map.empty Map.empty) (doDispatch testPlugins cr)
  return r

-- ---------------------------------------------------------------------

hareSpec :: Spec
hareSpec = do
  describe "hare plugin commands" $ do
    cwd <- runIO $ getCurrentDirectory
    -- ---------------------------------

    it "renames" $ do

      let req = IdeRequest "rename" (Map.fromList [("file",ParamValP $ ParamFile "./HaReRename.hs")
                                                  ,("start_pos",ParamValP $ ParamPos (toPos (5,1)))
                                                  ,("name",ParamValP $ ParamText "foolong")])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk (jsWrite (RefactorResult [HieDiff
                                                           (cwd </> "test/testdata/HaReRename.hs")
                                                           (cwd </> "test/testdata/HaReRename.refactored.hs")
                                                           ("4,5c4,5\n"++
                                                            "< foo :: Int -> Int\n"++
                                                            "< foo x = x + 3\n"++
                                                            "---\n"++
                                                            "> foolong :: Int -> Int\n"++
                                                            "> foolong x = x + 3\n")
                                                           ])))

    -- ---------------------------------

    it "returns an error for invalid rename" $ do
      let req = IdeRequest "rename" (Map.fromList [("file",ParamValP $ ParamFile "./HaReRename.hs")
                                                  ,("start_pos",ParamValP $ ParamPos (toPos (15,1)))
                                                  ,("name",ParamValP $ ParamText "foolong")])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseFail
                      (IdeError { ideCode = PluginError
                                , ideMessage = "rename: \"Invalid cursor position!\"", ideInfo = Null}))

    -- ---------------------------------

    it "demotes" $ do
      let req = IdeRequest "demote" (Map.fromList [("file",ParamValP $ ParamFile "./HaReDemote.hs")
                                                  ,("start_pos",ParamValP $ ParamPos (toPos (6,1)))])
      r <- dispatchRequest req
      -- r `shouldBe` Just (IdeResponseOk (H.fromList ["refactor" .= ["test/testdata/HaReDemote.hs"::FilePath]]))
      r `shouldBe` Just (IdeResponseOk $ jsWrite (RefactorResult [HieDiff
                                                           (cwd </> "test/testdata/HaReDemote.hs")
                                                           (cwd </> "test/testdata/HaReDemote.refactored.hs")
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

      let req = IdeRequest "dupdef" (Map.fromList [("file",ParamValP $ ParamFile "./HaReRename.hs")
                                                  ,("start_pos",ParamValP $ ParamPos (toPos (5,1)))
                                                  ,("name",ParamValP $ ParamText "foonew")])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite (RefactorResult [HieDiff
                                                           (cwd </> "test/testdata/HaReRename.hs")
                                                           (cwd </> "test/testdata/HaReRename.refactored.hs")
                                                           ("6a7,9\n"++
                                                            "> foonew :: Int -> Int\n"++
                                                            "> foonew x = x + 3\n"++
                                                            "> \n")
                                                           ]))

    -- ---------------------------------

    it "converts if to case" $ do

      let req = IdeRequest "iftocase" (Map.fromList [("file",ParamValP $ ParamFile "./HaReCase.hs")
                                                    ,("start_pos",ParamValP $ ParamPos (toPos (5,9)))
                                                    ,("end_pos",  ParamValP $ ParamPos (toPos (9,12))) ])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite (RefactorResult [HieDiff
                                                           (cwd </> "test/testdata/HaReCase.hs")
                                                           (cwd </> "test/testdata/HaReCase.refactored.hs")
                                                           ("5,9c5,9\n"++
                                                            "< foo x = if odd x\n"++
                                                            "<         then\n"++
                                                            "<           x + 3\n"++
                                                            "<         else\n"++
                                                            "<           x\n"++
                                                            "---\n"++
                                                            "> foo x = case odd x of\n"++
                                                            ">   True  ->\n"++
                                                            ">     x + 3\n"++
                                                            ">   False ->\n"++
                                                            ">     x\n")
                                                           ]))

    -- ---------------------------------

    it "lifts one level" $ do

      let req = IdeRequest "liftonelevel" (Map.fromList [("file",ParamValP $ ParamFile "./HaReMoveDef.hs")
                                                    ,("start_pos",ParamValP $ ParamPos (toPos (6,5)))])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite (RefactorResult [HieDiff
                                                           (cwd </> "test/testdata/HaReMoveDef.hs")
                                                           (cwd </> "test/testdata/HaReMoveDef.refactored.hs")
                                                           ("5,6d4\n"++
                                                            "<   where\n"++
                                                            "<     y = 4\n"++
                                                            "7a6,7\n"++
                                                            "> y = 4\n"++
                                                            "> \n")
                                                           ]))

    -- ---------------------------------

    it "lifts to top level" $ do

      let req = IdeRequest "lifttotoplevel" (Map.fromList [("file",ParamValP $ ParamFile "./HaReMoveDef.hs")
                                                          ,("start_pos",ParamValP $ ParamPos (toPos (12,9)))])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite (RefactorResult [HieDiff
                                                           (cwd </> "test/testdata/HaReMoveDef.hs")
                                                           (cwd </> "test/testdata/HaReMoveDef.refactored.hs")
                                                           ("11,12d10\n"++
                                                            "<       where\n"++
                                                            "<         z = 7\n"++
                                                            "13a12\n"++
                                                            "> z = 7\n"++
                                                            "14a14\n"++
                                                            "> \n")
                                                           ]))

    -- ---------------------------------

    it "deletes a definition" $ do
      let req = IdeRequest "deletedef" (Map.fromList [("file",ParamValP $ ParamFile "./FuncTest.hs")
                                                  ,("start_pos",ParamValP $ ParamPos (toPos (6,1)))])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite (RefactorResult [HieDiff
                                                           (cwd </> "test/testdata/FuncTest.hs")
                                                           (cwd </> "test/testdata/FuncTest.refactored.hs")
                                                           ("5,7d4\n"++
                                                            "< foo :: Int\n"++
                                                            "< foo = bb\n"++
                                                            "< \n")
                                                           ]))

    -- ---------------------------------

    it "generalises an applicative" $ do
      let req = IdeRequest "genapplicative" (Map.fromList [("file",ParamValP $ ParamFile "./HaReGA1.hs")
                                                  ,("start_pos",ParamValP $ ParamPos (toPos (4,1)))])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite (RefactorResult [HieDiff
                                                           (cwd </> "test/testdata/HaReGA1.hs")
                                                           (cwd </> "test/testdata/HaReGA1.refactored.hs")
                                                           ("5,9c5\n"++
                                                            "< parseStr = do\n"++
                                                            "<   char '\"'\n"++
                                                            "<   str <- many1 (noneOf \"\\\"\")\n"++
                                                            "<   char '\"'\n"++
                                                            "<   return str\n"++
                                                            "---\n"++
                                                            "> parseStr = char '\"' *> (many1 (noneOf \"\\\"\")) <* char '\"'\n"
                                                           )
                                                           ]))

    -- ---------------------------------
