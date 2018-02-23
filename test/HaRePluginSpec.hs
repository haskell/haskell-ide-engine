{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module HaRePluginSpec where

import           Data.Aeson
import qualified Data.HashMap.Strict                   as H
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Plugin.GhcMod
import           Haskell.Ide.Engine.Plugin.HaRe
import           Haskell.Ide.Engine.Plugin.HieExtras
import           Language.Haskell.LSP.TH.DataTypesJSON (Location(..), TextEdit(..))
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

-- ---------------------------------------------------------------------

testPlugins :: IdePlugins
testPlugins = pluginDescToIdePlugins [("hare",hareDescriptor)]

dispatchRequestPGoto :: IdeGhcM a -> IO a
dispatchRequestPGoto =
  cdAndDo "./test/testdata/gototest"
    . runIGM testPlugins

-- ---------------------------------------------------------------------

hareSpec :: Spec
hareSpec = do
  describe "hare plugin commands(old plugin api)" $ do
    cwd <- runIO getCurrentDirectory
    -- ---------------------------------

    it "renames" $ cdAndDo "test/testdata" $ do

      let uri = filePathToUri "./HaReRename.hs"
          act = renameCmd' uri (toPos (5,1)) "foolong"
          arg = HPT uri (toPos (5,1)) "foolong"
          res = IdeResponseOk $ WorkspaceEdit
            (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReRename.hs")
                                $ List [TextEdit (Range (Position 3 0) (Position 4 13))
                                          "foolong :: Int -> Int\nfoolong x = x + 3"])
            Nothing
      testCommand testPlugins act "hare" "rename" arg res

    -- ---------------------------------

    it "returns an error for invalid rename" $ cdAndDo "test/testdata" $ do
      let uri = filePathToUri "./HaReRename.hs"
          act = renameCmd' uri (toPos (15,1)) "foolong"
          arg = HPT uri (toPos (15,1)) "foolong"
          res = IdeResponseFail
                  IdeError { ideCode = PluginError
                           , ideMessage = "rename: \"Invalid cursor position!\"", ideInfo = Null}
      testCommand testPlugins act "hare" "rename" arg res

    -- ---------------------------------

    it "demotes" $ cdAndDo "test/testdata" $ do
      let uri = filePathToUri "./HaReDemote.hs"
          act = demoteCmd' uri (toPos (6,1))
          arg = HP uri (toPos (6,1))
          res = IdeResponseOk $ WorkspaceEdit
            (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReDemote.hs")
                                $ List [TextEdit (Range (Position 4 0) (Position 5 5))
                                          "  where\n    y = 7"])
            Nothing
      testCommand testPlugins act "hare" "demote" arg res

    -- ---------------------------------

    it "duplicates a definition" $ cdAndDo "test/testdata" $ do
      let uri = filePathToUri "./HaReRename.hs"
          act = dupdefCmd' uri (toPos (5,1)) "foonew"
          arg = HPT uri (toPos (5,1)) "foonew"
          res = IdeResponseOk $ WorkspaceEdit
            (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReRename.hs")
                                $ List [TextEdit (Range (Position 6 0) (Position 6 0))
                                          "foonew :: Int -> Int\nfoonew x = x + 3\n\n"])
            Nothing
      testCommand testPlugins act "hare" "dupdef" arg res

    -- ---------------------------------

    it "converts if to case" $ cdAndDo "test/testdata" $ do

      let uri = filePathToUri "./HaReCase.hs"
          act = iftocaseCmd' uri (Range (toPos (5,9))
                                        (toPos (9,12)))
          arg = HR uri (toPos (5,9)) (toPos (9,12))
          res = IdeResponseOk $ WorkspaceEdit
            (Just
             $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReCase.hs")
                           $ List [TextEdit (Range (Position 4 0) (Position 8 11))
                                   "foo x = case odd x of\n  True  ->\n    x + 3\n  False ->\n    x"])
            Nothing
      testCommand testPlugins act "hare" "iftocase" arg res

    -- ---------------------------------

    it "lifts one level" $ cdAndDo "test/testdata" $ do

      let uri = filePathToUri "./HaReMoveDef.hs"
          act = liftonelevelCmd' uri (toPos (6,5))
          arg = HP uri (toPos (6,5))
          res = IdeResponseOk $ WorkspaceEdit
            (Just $ H.singleton
              ( filePathToUri $ cwd </> "test/testdata/HaReMoveDef.hs" )
              $ List [ TextEdit (Range (Position 6 0) (Position 6 0)) "y = 4\n\n"
                     , TextEdit (Range (Position 4 0) (Position 5 9)) ""
                     ])
            Nothing
      testCommand testPlugins act "hare" "liftonelevel" arg res

    -- ---------------------------------

    it "lifts to top level" $ cdAndDo "test/testdata" $ do

      let uri = filePathToUri "./HaReMoveDef.hs"
          act = lifttotoplevelCmd' uri (toPos (12,9))
          arg = HP uri (toPos (12,9))
          res = IdeResponseOk $ WorkspaceEdit
           (Just $ H.singleton
              ( filePathToUri $ cwd </> "test/testdata/HaReMoveDef.hs")
              $ List [ TextEdit (Range (Position 13 0) (Position 13 0)) "\n"
                     , TextEdit (Range (Position 12 0) (Position 12 0)) "z = 7\n"
                     , TextEdit (Range (Position 10 0) (Position 11 13)) ""
                     ])
           Nothing
      testCommand testPlugins act "hare" "lifttotoplevel" arg res

    -- ---------------------------------

    it "deletes a definition" $ cdAndDo "test/testdata" $ do
      let uri = filePathToUri "./FuncTest.hs"
          act = deleteDefCmd' uri (toPos (6,1))
          arg = HP uri (toPos (6,1))
          res = IdeResponseOk $ WorkspaceEdit
            (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/FuncTest.hs")
                                $ List [TextEdit (Range (Position 4 0) (Position 6 0)) ""])
            Nothing
      testCommand testPlugins act "hare" "deletedef" arg res

    -- ---------------------------------

    it "generalises an applicative" $ cdAndDo "test/testdata" $ do
      let uri = filePathToUri "./HaReGA1.hs"
          act = genApplicativeCommand' uri (toPos (4,1))
          arg = HP uri (toPos (4,1))
          res = IdeResponseOk $ WorkspaceEdit
            (Just $ H.singleton ( filePathToUri $ cwd </> "test/testdata/HaReGA1.hs" )
                                $ List [TextEdit (Range (Position 4 0) (Position 8 12))
                                         "parseStr = char '\"' *> (many1 (noneOf \"\\\"\")) <* char '\"'"])
            Nothing
      testCommand testPlugins act "hare" "genapplicative" arg res

    -- ---------------------------------

  describe "Additional GHC API commands" $ do
    cwd <- runIO getCurrentDirectory

    it "finds definition across components" $ do
      let u = filePathToUri "./app/Main.hs"
          lreq = setTypecheckedModule u
          req = findDef u (toPos (7,8))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResponseOk [Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs")
                                           (Range (toPos (6,1)) (toPos (6,9)))]
      let req2 = findDef u (toPos (7,20))
      r2 <- dispatchRequestPGoto $ lreq >> req2
      r2 `shouldBe` IdeResponseOk [Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib2.hs")
                                            (Range (toPos (5,1)) (toPos (5,2)))]
    it "finds definition in the same component" $ do
      let u = filePathToUri "./src/Lib2.hs"
          lreq = setTypecheckedModule u
          req = findDef u (toPos (6,5))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResponseOk [Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs")
                                           (Range (toPos (6,1)) (toPos (6,9)))]
    it "finds local definitions" $ do
      let u = filePathToUri "./src/Lib2.hs"
          lreq = setTypecheckedModule u
          req = findDef u (toPos (7,11))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResponseOk [Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib2.hs")
                                           (Range (toPos (10,9)) (toPos (10,10)))]
      let req2 = findDef u (toPos (10,13))
      r2 <- dispatchRequestPGoto $ lreq >> req2
      r2 `shouldBe` IdeResponseOk [Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib2.hs")
                                            (Range (toPos (9,9)) (toPos (9,10)))]


    -- ---------------------------------
