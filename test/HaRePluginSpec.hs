{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DuplicateRecordFields   #-}
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
dispatchRequest :: IdeRequest -> IO (Maybe (IdeResponse Value))
dispatchRequest req = do
  testChan <- atomically newTChan
  let cr = CReq "hare" 1 req testChan
  cdAndDo "./test/testdata"
    $ runIdeM testOptions (IdeState Map.empty Map.empty) (doDispatch testPlugins cr)

dispatchRequestP :: IdeM a -> IO a
dispatchRequestP =
  cdAndDo "./test/testdata"
    . runIdeM testOptions (IdeState Map.empty Map.empty)

dispatchRequestPGoto :: IdeM a -> IO a
dispatchRequestPGoto =
  cdAndDo "./test/testdata/gototest"
    . runIdeM testOptions (IdeState Map.empty Map.empty)

-- ---------------------------------------------------------------------

hareSpec :: Spec
hareSpec = do
  describe "hare plugin commands(old plugin api)" $ do
    cwd <- runIO getCurrentDirectory
    -- ---------------------------------

    it "renames" $ do

      let req = IdeRequest "rename" (Map.fromList [("file",ParamFileP $ filePathToUri "./HaReRename.hs")
                                                  ,("start_pos",ParamPosP (toPos (5,1)))
                                                  ,("name",ParamTextP "foolong")])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk
              $ toJSON
              $ WorkspaceEdit
                (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReRename.hs")
                                    $ List [TextEdit (Range (Position 3 0) (Position 4 13))
                                              "foolong :: Int -> Int\nfoolong x = x + 3"])
                Nothing )

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
      r `shouldBe`
        Just (IdeResponseOk
              $ toJSON
              $ WorkspaceEdit
                (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReDemote.hs")
                                    $ List [TextEdit (Range (Position 4 0) (Position 5 5))
                                              "  where\n    y = 7"])
                Nothing)

    -- ---------------------------------

    it "duplicates a definition" $ do

      let req = IdeRequest "dupdef" (Map.fromList [("file",ParamFileP $ filePathToUri "./HaReRename.hs")
                                                  ,("start_pos",ParamPosP (toPos (5,1)))
                                                  ,("name",ParamTextP "foonew")])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk
              $ toJSON
              $ WorkspaceEdit
                (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReRename.hs")
                                    $ List [TextEdit (Range (Position 6 0) (Position 8 0))
                                              "foonew :: Int -> Int\nfoonew x = x + 3\n\n"])
                Nothing)

    -- ---------------------------------

    it "converts if to case" $ do

      let req = IdeRequest "iftocase" (Map.fromList [("file",ParamFileP $ filePathToUri "./HaReCase.hs")
                                                    ,("start_pos",ParamPosP (toPos (5,9)))
                                                    ,("end_pos",  ParamPosP (toPos (9,12))) ])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk
              $ toJSON
              $ WorkspaceEdit
                (Just
                 $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReCase.hs")
                               $ List [TextEdit (Range (Position 4 0) (Position 8 11))
                                       "foo x = case odd x of\n  True  ->\n    x + 3\n  False ->\n    x"])
                Nothing)

    -- ---------------------------------

    it "lifts one level" $ do

      let req = IdeRequest "liftonelevel" (Map.fromList [("file",ParamFileP $ filePathToUri "./HaReMoveDef.hs")
                                                        ,("start_pos",ParamPosP (toPos (6,5)))])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk
              $ toJSON
              $ WorkspaceEdit
                (Just $ H.singleton
                  ( filePathToUri $ cwd </> "test/testdata/HaReMoveDef.hs" )
                  $ List [TextEdit (Range (Position 4 0) (Position 5 9)) ""
                         ,TextEdit (Range (Position 5 0) (Position 6 0)) "y = 4\n\n"])
                Nothing)

    -- ---------------------------------

    it "lifts to top level" $ do

      let req = IdeRequest "lifttotoplevel" (Map.fromList [("file",ParamFileP $ filePathToUri "./HaReMoveDef.hs")
                                                          ,("start_pos",ParamPosP (toPos (12,9)))])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk
              $ toJSON
              $ WorkspaceEdit
               (Just $ H.singleton
                  ( filePathToUri $ cwd </> "test/testdata/HaReMoveDef.hs")
                  $ List [TextEdit (Range (Position 10 0) (Position 11 13)) ""
                         ,TextEdit (Range (Position 11 0) (Position 11 5)) "z = 7\n"
                         ,TextEdit (Range (Position 13 0) (Position 13 0)) "\n"])
               Nothing)

    -- ---------------------------------

    it "deletes a definition" $ do
      let req = IdeRequest "deletedef" (Map.fromList [("file",ParamFileP $ filePathToUri "./FuncTest.hs")
                                                  ,("start_pos",ParamPosP (toPos (6,1)))])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk
              $ toJSON
              $ WorkspaceEdit
               (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/FuncTest.hs")
                                   $ List [TextEdit (Range (Position 4 0) (Position 6 0)) ""])
               Nothing)

    -- ---------------------------------

    it "generalises an applicative" $ do
      let req = IdeRequest "genapplicative" (Map.fromList [("file",ParamFileP $ filePathToUri "./HaReGA1.hs")
                                                          ,("start_pos",ParamPosP (toPos (4,1)))])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk
             $ toJSON
             $ WorkspaceEdit
               (Just $ H.singleton ( filePathToUri $ cwd </> "test/testdata/HaReGA1.hs" )
                                   $ List [TextEdit (Range (Position 4 0) (Position 8 12))
                                            "parseStr = char '\"' *> (many1 (noneOf \"\\\"\")) <* char '\"'"])
               Nothing)

    -- ---------------------------------

  describe "hare plugin commands(new plugin api)" $ do
    cwd <- runIO getCurrentDirectory
    -- ---------------------------------

    it "renames" $ do

      let req = renameCmd' (TextDocumentPositionParams (TextDocumentIdentifier $ filePathToUri "./HaReRename.hs") (toPos (5,1))) "foolong"
      r <- dispatchRequestP req
      r `shouldBe`
        (IdeResponseOk
         $ WorkspaceEdit
           (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReRename.hs")
                               $ List [TextEdit (Range (Position 3 0) (Position 4 13))
                                         "foolong :: Int -> Int\nfoolong x = x + 3"])
           Nothing )

    -- ---------------------------------

    it "returns an error for invalid rename" $ do
      let req = renameCmd' (TextDocumentPositionParams (TextDocumentIdentifier $ filePathToUri "./HaReRename.hs") (toPos (15,1))) "foolong"
      r <- dispatchRequestP req
      r `shouldBe` (IdeResponseFail
                      IdeError { ideCode = PluginError
                               , ideMessage = "rename: \"Invalid cursor position!\"", ideInfo = Null})

    -- ---------------------------------

    it "demotes" $ do
      let req = demoteCmd' (TextDocumentPositionParams (TextDocumentIdentifier $ filePathToUri "./HaReDemote.hs") (toPos (6,1)))
      r <- dispatchRequestP req
      -- r `shouldBe` Just (IdeResponseOk (H.fromList ["refactor" .= ["test/testdata/HaReDemote.hs"::FilePath]]))
      r `shouldBe`
        (IdeResponseOk
         $ WorkspaceEdit
           (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReDemote.hs")
                               $ List [TextEdit (Range (Position 4 0) (Position 5 5))
                                         "  where\n    y = 7"])
           Nothing)

    -- ---------------------------------

    it "duplicates a definition" $ do

      let req = dupdefCmd' (TextDocumentPositionParams (TextDocumentIdentifier $ filePathToUri "./HaReRename.hs") (toPos (5,1))) "foonew"
      r <- dispatchRequestP req
      r `shouldBe`
        (IdeResponseOk
         $ WorkspaceEdit
           (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReRename.hs")
                               $ List [TextEdit (Range (Position 6 0) (Position 8 0))
                                         "foonew :: Int -> Int\nfoonew x = x + 3\n\n"])
           Nothing)

    -- ---------------------------------

    it "converts if to case" $ do

      let req = iftocaseCmd' (Location (filePathToUri "./HaReCase.hs")
                                       (Range (toPos (5,9))
                                              (toPos (9,12))))
      r <- dispatchRequestP req
      r `shouldBe`
        (IdeResponseOk
         $ WorkspaceEdit
           (Just
            $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReCase.hs")
                          $ List [TextEdit (Range (Position 4 0) (Position 8 11))
                                  "foo x = case odd x of\n  True  ->\n    x + 3\n  False ->\n    x"])
           Nothing)

    -- ---------------------------------

    it "lifts one level" $ do

      let req = liftonelevelCmd' (TextDocumentPositionParams (TextDocumentIdentifier $ filePathToUri "./HaReMoveDef.hs") (toPos (6,5)))
      r <- dispatchRequestP req
      r `shouldBe`
        (IdeResponseOk
         $ WorkspaceEdit
           (Just $ H.singleton
             ( filePathToUri $ cwd </> "test/testdata/HaReMoveDef.hs" )
             $ List [TextEdit (Range (Position 4 0) (Position 5 9)) ""
                    ,TextEdit (Range (Position 5 0) (Position 6 0)) "y = 4\n\n"])
           Nothing)

    -- ---------------------------------

    it "lifts to top level" $ do

      let req = lifttotoplevelCmd' (TextDocumentPositionParams (TextDocumentIdentifier $ filePathToUri "./HaReMoveDef.hs") (toPos (12,9)))
      r <- dispatchRequestP req
      r `shouldBe`
        (IdeResponseOk
         $ WorkspaceEdit
          (Just $ H.singleton
             ( filePathToUri $ cwd </> "test/testdata/HaReMoveDef.hs")
             $ List [TextEdit (Range (Position 10 0) (Position 11 13)) ""
                    ,TextEdit (Range (Position 11 0) (Position 11 5)) "z = 7\n"
                    ,TextEdit (Range (Position 13 0) (Position 13 0)) "\n"])
          Nothing)

    -- ---------------------------------

    it "deletes a definition" $ do
      let req = deleteDefCmd' (TextDocumentPositionParams (TextDocumentIdentifier $ filePathToUri "./FuncTest.hs") (toPos (6,1)))
      r <- dispatchRequestP req
      r `shouldBe`
        (IdeResponseOk
         $ WorkspaceEdit
          (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/FuncTest.hs")
                              $ List [TextEdit (Range (Position 4 0) (Position 6 0)) ""])
          Nothing)

    -- ---------------------------------

    it "generalises an applicative" $ do
      let req = genApplicativeCommand' (TextDocumentPositionParams (TextDocumentIdentifier $ filePathToUri "./HaReGA1.hs") (toPos (4,1)))
      r <- dispatchRequestP req
      r `shouldBe`
        (IdeResponseOk
        $ WorkspaceEdit
          (Just $ H.singleton ( filePathToUri $ cwd </> "test/testdata/HaReGA1.hs" )
                              $ List [TextEdit (Range (Position 4 0) (Position 8 12))
                                       "parseStr = char '\"' *> (many1 (noneOf \"\\\"\")) <* char '\"'"])
          Nothing)
    it "finds definition across components" $ do
      let req = findDefCmd (TextDocumentPositionParams (TextDocumentIdentifier $ filePathToUri "./app/Main.hs") (toPos (7,8)))
      r <- dispatchRequestPGoto req
      r `shouldBe` IdeResponseOk (Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs")
                                           (Range (toPos (6,1)) (toPos (6,9))))
      let req2 = findDefCmd (TextDocumentPositionParams (TextDocumentIdentifier $ filePathToUri "./app/Main.hs") (toPos (7,20)))
      r2 <- dispatchRequestPGoto req2
      r2 `shouldBe` IdeResponseOk (Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib2.hs")
                                            (Range (toPos (5,1)) (toPos (5,2))))
    it "finds definition in the same component" $ do
      let req = findDefCmd (TextDocumentPositionParams (TextDocumentIdentifier $ filePathToUri "./src/Lib2.hs") (toPos (6,5)))
      r <- dispatchRequestPGoto req
      r `shouldBe` IdeResponseOk (Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs")
                                           (Range (toPos (6,1)) (toPos (6,9))))
    it "finds local definitions" $ do
      let req = findDefCmd (TextDocumentPositionParams (TextDocumentIdentifier $ filePathToUri "./src/Lib2.hs") (toPos (7,11)))
      r <- dispatchRequestPGoto req
      r `shouldBe` IdeResponseOk (Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib2.hs")
                                           (Range (toPos (10,9)) (toPos (10,10))))
      let req2 = findDefCmd (TextDocumentPositionParams (TextDocumentIdentifier $ filePathToUri "./src/Lib2.hs") (toPos (10,13)))
      r2 <- dispatchRequestPGoto req2
      r2 `shouldBe` IdeResponseOk (Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib2.hs")
                                            (Range (toPos (9,9)) (toPos (9,10))))


    -- ---------------------------------
