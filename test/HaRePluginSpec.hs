{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HaRePluginSpec where

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.Map                            as Map
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.SemanticTypes
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.HaRePlugin
import           Language.Haskell.LSP.TH.DataTypesJSON
import qualified Data.HashMap.Strict as H
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
  cdAndDo "./test/testdata"
    $ runIdeM testOptions (IdeState Map.empty Map.empty) (doDispatch testPlugins cr)

-- ---------------------------------------------------------------------

hareSpec :: Spec
hareSpec = do
  describe "hare plugin commands" $ do
    cwd <- runIO getCurrentDirectory
    -- ---------------------------------

    it "renames" $ do

      let req = IdeRequest "rename" (Map.fromList [("file",ParamFileP $ filePathToUri "./HaReRename.hs")
                                                  ,("start_pos",ParamPosP (toPos (5,1)))
                                                  ,("name",ParamTextP "foolong")])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk $
              jsWrite
              $ RefactorResult
              [WorkspaceEdit
              {_changes = Just (H.fromList [(filePathToUri $ cwd </> "test/testdata/HaReRename.hs",List [TextEdit {_range = Range {_start = Position {_line = 3, _character = 0}, _end = Position {_line = 4, _character = 13}}, _newText = "foolong :: Int -> Int\nfoolong x = x + 3"}])]), _documentChanges = Nothing}] )

    -- ---------------------------------

    it "returns an error for invalid rename" $ do
      let req = IdeRequest "rename" (Map.fromList [("file",ParamFileP $ filePathToUri "./HaReRename.hs")
                                                  ,("start_pos",ParamPosP (toPos (15,1)))
                                                  ,("name",ParamTextP "foolong")])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseFail
                      IdeError { ideCode = PluginError
                                , ideMessage = "rename: \"Invalid cursor position!\"", ideInfo = Null})

    -- ---------------------------------

    it "demotes" $ do
      let req = IdeRequest "demote" (Map.fromList [("file",ParamFileP $ filePathToUri "./HaReDemote.hs")
                                                  ,("start_pos",ParamPosP (toPos (6,1)))])
      r <- dispatchRequest req
      -- r `shouldBe` Just (IdeResponseOk (H.fromList ["refactor" .= ["test/testdata/HaReDemote.hs"::FilePath]]))
      r `shouldBe` Just (IdeResponseOk $ jsWrite $ RefactorResult {rrDiffs = [WorkspaceEdit {_changes = Just (H.fromList [(filePathToUri $ cwd </> "test/testdata/HaReDemote.hs",List [TextEdit {_range = Range {_start = Position {_line = 4, _character = 0}, _end = Position {_line = 5, _character = 5}}, _newText = "  where\n    y = 7"}])]), _documentChanges = Nothing}]} )

    -- ---------------------------------

    it "duplicates a definition" $ do

      let req = IdeRequest "dupdef" (Map.fromList [("file",ParamFileP $ filePathToUri "./HaReRename.hs")
                                                  ,("start_pos",ParamPosP (toPos (5,1)))
                                                  ,("name",ParamTextP "foonew")])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite $ RefactorResult {rrDiffs = [WorkspaceEdit {_changes = Just (H.fromList [(filePathToUri $ cwd </> "test/testdata/HaReRename.hs",List [TextEdit {_range = Range {_start = Position {_line = 6, _character = 0}, _end = Position {_line = 8, _character = 0}}, _newText = "foonew :: Int -> Int\nfoonew x = x + 3\n\n"}])]), _documentChanges = Nothing}]})

    -- ---------------------------------

    it "converts if to case" $ do

      let req = IdeRequest "iftocase" (Map.fromList [("file",ParamFileP $ filePathToUri "./HaReCase.hs")
                                                    ,("start_pos",ParamPosP (toPos (5,9)))
                                                    ,("end_pos",  ParamPosP (toPos (9,12))) ])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite $ RefactorResult {rrDiffs = [WorkspaceEdit {_changes = Just (H.fromList [(filePathToUri $ cwd </> "test/testdata/HaReCase.hs",List [TextEdit {_range = Range {_start = Position {_line = 4, _character = 0}, _end = Position {_line = 8, _character = 11}}, _newText = "foo x = case odd x of\n  True  ->\n    x + 3\n  False ->\n    x"}])]), _documentChanges = Nothing}]})

    -- ---------------------------------

    it "lifts one level" $ do

      let req = IdeRequest "liftonelevel" (Map.fromList [("file",ParamFileP $ filePathToUri "./HaReMoveDef.hs")
                                                        ,("start_pos",ParamPosP (toPos (6,5)))])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite $ RefactorResult {rrDiffs = [WorkspaceEdit {_changes = Just (H.fromList [(filePathToUri $ cwd </> "test/testdata/HaReMoveDef.hs",List [TextEdit {_range = Range {_start = Position {_line = 4, _character = 0}, _end = Position {_line = 5, _character = 9}}, _newText = ""},TextEdit {_range = Range {_start = Position {_line = 5, _character = 0}, _end = Position {_line = 6, _character = 0}}, _newText = "y = 4\n\n"}])]), _documentChanges = Nothing}]})

    -- ---------------------------------

    it "lifts to top level" $ do

      let req = IdeRequest "lifttotoplevel" (Map.fromList [("file",ParamFileP $ filePathToUri "./HaReMoveDef.hs")
                                                          ,("start_pos",ParamPosP (toPos (12,9)))])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite $ RefactorResult {rrDiffs = [WorkspaceEdit {_changes = Just (H.fromList [(filePathToUri $ cwd </> "test/testdata/HaReMoveDef.hs",List [TextEdit {_range = Range {_start = Position {_line = 10, _character = 0}, _end = Position {_line = 11, _character = 13}}, _newText = ""},TextEdit {_range = Range {_start = Position {_line = 11, _character = 0}, _end = Position {_line = 11, _character = 5}}, _newText = "z = 7\n"},TextEdit {_range = Range {_start = Position {_line = 13, _character = 0}, _end = Position {_line = 13, _character = 0}}, _newText = "\n"}])]), _documentChanges = Nothing}]})

    -- ---------------------------------

    it "deletes a definition" $ do
      let req = IdeRequest "deletedef" (Map.fromList [("file",ParamFileP $ filePathToUri "./FuncTest.hs")
                                                  ,("start_pos",ParamPosP (toPos (6,1)))])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite $ RefactorResult {rrDiffs = [WorkspaceEdit {_changes = Just (H.fromList [(filePathToUri $ cwd </> "test/testdata/FuncTest.hs",List [TextEdit {_range = Range {_start = Position {_line = 4, _character = 0}, _end = Position {_line = 6, _character = 0}}, _newText = ""}])]), _documentChanges = Nothing}]})

    -- ---------------------------------

    it "generalises an applicative" $ do
      let req = IdeRequest "genapplicative" (Map.fromList [("file",ParamFileP $ filePathToUri "./HaReGA1.hs")
                                                          ,("start_pos",ParamPosP (toPos (4,1)))])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk $ jsWrite $ RefactorResult {rrDiffs = [WorkspaceEdit {_changes = Just (H.fromList [(filePathToUri $ cwd </> "test/testdata/HaReGA1.hs",List [TextEdit {_range = Range {_start = Position {_line = 4, _character = 0}, _end = Position {_line = 8, _character = 12}}, _newText = "parseStr = char '\"' *> (many1 (noneOf \"\\\"\")) <* char '\"'"}])]), _documentChanges = Nothing}]})

    -- ---------------------------------
