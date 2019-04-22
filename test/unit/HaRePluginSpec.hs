{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HaRePluginSpec where

import           Control.Monad.Trans.Free
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Map                      as M
import qualified Data.HashMap.Strict           as H
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Plugin.GhcMod
import           Haskell.Ide.Engine.Plugin.HaRe
import           Haskell.Ide.Engine.Support.HieExtras
import           Language.Haskell.LSP.Types     ( Location(..)
                                                , TextEdit(..)
                                                )
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
testPlugins = pluginDescToIdePlugins [hareDescriptor "hare"]

dispatchRequestPGoto :: IdeGhcM a -> IO a
dispatchRequestPGoto =
  withCurrentDirectory "./test/testdata/gototest"
    . runIGM testPlugins

-- ---------------------------------------------------------------------

hareSpec :: Spec
hareSpec = do
  describe "hare plugin commands(old plugin api)" $ do
    cwd <- runIO getCurrentDirectory
    -- ---------------------------------

    it "renames" $ withCurrentDirectory "test/testdata" $ do

      let uri = filePathToUri $ cwd </> "test/testdata/HaReRename.hs"
          act = renameCmd' uri (toPos (5,1)) "foolong"
          arg = HPT uri (toPos (5,1)) "foolong"
          textEdits = List [TextEdit (Range (Position 3 0) (Position 4 13)) "foolong :: Int -> Int\nfoolong x = x + 3"]
          res = IdeResultOk $ WorkspaceEdit
            (Just $ H.singleton uri textEdits)
            Nothing
      testCommand testPlugins act "hare" "rename" arg res

    -- ---------------------------------

    it "returns an error for invalid rename" $ withCurrentDirectory "test/testdata" $ do
      let uri = filePathToUri $ cwd </> "test/testdata/HaReRename.hs"
          act = renameCmd' uri (toPos (15,1)) "foolong"
          arg = HPT uri (toPos (15,1)) "foolong"
          res = IdeResultFail
                  IdeError { ideCode = PluginError
                           , ideMessage = "rename: \"Invalid cursor position!\"", ideInfo = Null}
      testCommand testPlugins act "hare" "rename" arg res

    -- ---------------------------------

    it "demotes" $ withCurrentDirectory "test/testdata" $ do
      let uri = filePathToUri $ cwd </> "test/testdata/HaReDemote.hs"
          act = demoteCmd' uri (toPos (6,1))
          arg = HP uri (toPos (6,1))
          textEdits = List [TextEdit (Range (Position 4 0) (Position 5 5)) "  where\n    y = 7"]
          res = IdeResultOk $ WorkspaceEdit
            (Just $ H.singleton uri textEdits)
            Nothing
      testCommand testPlugins act "hare" "demote" arg res

    -- ---------------------------------

    it "duplicates a definition" $ withCurrentDirectory "test/testdata" $ do
      let uri = filePathToUri $ cwd </> "test/testdata/HaReRename.hs"
          act = dupdefCmd' uri (toPos (5,1)) "foonew"
          arg = HPT uri (toPos (5,1)) "foonew"
          textEdits = List [TextEdit (Range (Position 6 0) (Position 6 0)) "foonew :: Int -> Int\nfoonew x = x + 3\n\n"]
          res = IdeResultOk $ WorkspaceEdit
            (Just $ H.singleton uri textEdits)
            Nothing
      testCommand testPlugins act "hare" "dupdef" arg res

    -- ---------------------------------

    it "converts if to case" $ withCurrentDirectory "test/testdata" $ do

      let uri = filePathToUri $ cwd </> "test/testdata/HaReCase.hs"
          act = iftocaseCmd' uri (Range (toPos (5,9))
                                        (toPos (9,12)))
          arg = HR uri (toPos (5,9)) (toPos (9,12))
          textEdits = List [TextEdit (Range (Position 4 0) (Position 8 11))
                      "foo x = case odd x of\n  True  ->\n    x + 3\n  False ->\n    x"]
          res = IdeResultOk $ WorkspaceEdit
            (Just $ H.singleton uri textEdits)
            Nothing
      testCommand testPlugins act "hare" "iftocase" arg res

    -- ---------------------------------

    it "lifts one level" $ withCurrentDirectory "test/testdata" $ do

      let uri = filePathToUri $ cwd </> "test/testdata/HaReMoveDef.hs"
          act = liftonelevelCmd' uri (toPos (6,5))
          arg = HP uri (toPos (6,5))
          textEdits = List [ TextEdit (Range (Position 6 0) (Position 6 0)) "y = 4\n\n"
                          , TextEdit (Range (Position 4 0) (Position 6 0)) ""]
          res = IdeResultOk $ WorkspaceEdit
            (Just $ H.singleton uri textEdits)
            Nothing
      testCommand testPlugins act "hare" "liftonelevel" arg res

    -- ---------------------------------

    it "lifts to top level" $ withCurrentDirectory "test/testdata" $ do

      let uri = filePathToUri $ cwd </> "test/testdata/HaReMoveDef.hs"
          act = lifttotoplevelCmd' uri (toPos (12,9))
          arg = HP uri (toPos (12,9))
          textEdits = List [ TextEdit (Range (Position 13 0) (Position 13 0)) "\n"
                           , TextEdit (Range (Position 12 0) (Position 12 0)) "z = 7\n"
                           , TextEdit (Range (Position 10 0) (Position 12 0)) ""
                           ]
          res = IdeResultOk $ WorkspaceEdit
            (Just $ H.singleton uri textEdits)
            Nothing
      testCommand testPlugins act "hare" "lifttotoplevel" arg res

    -- ---------------------------------

    it "deletes a definition" $ withCurrentDirectory "test/testdata" $ do
      let uri = filePathToUri $ cwd </> "test/testdata/FuncTest.hs"
          act = deleteDefCmd' uri (toPos (6,1))
          arg = HP uri (toPos (6,1))
          textEdits = List [TextEdit (Range (Position 4 0) (Position 7 0)) ""]
          res = IdeResultOk $ WorkspaceEdit
            (Just $ H.singleton uri textEdits)
            Nothing
      testCommand testPlugins act "hare" "deletedef" arg res

    -- ---------------------------------

    it "generalises an applicative" $ withCurrentDirectory "test/testdata" $ do
      let uri = filePathToUri $ cwd </> "test/testdata/HaReGA1.hs"
          act = genApplicativeCommand' uri (toPos (4,1))
          arg = HP uri (toPos (4,1))
          textEdits = List [TextEdit (Range (Position 4 0) (Position 8 12))
                      "parseStr = char '\"' *> (many1 (noneOf \"\\\"\")) <* char '\"'"]
          res = IdeResultOk $ WorkspaceEdit
            (Just $ H.singleton uri textEdits)
            Nothing
      testCommand testPlugins act "hare" "genapplicative" arg res

    -- ---------------------------------

  describe "Additional GHC API commands" $ do
    cwd <- runIO getCurrentDirectory

    it "finds definition across components" $ do
      let u = filePathToUri $ cwd </> "test/testdata/gototest/app/Main.hs"
          lreq = setTypecheckedModule u
          req = liftToGhc $ TestDeferM $ findDef u (toPos (7,8))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResultOk [Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs")
                                           (Range (toPos (6,1)) (toPos (6,9)))]
      let req2 = liftToGhc $ TestDeferM $ findDef u (toPos (7,20))
      r2 <- dispatchRequestPGoto $ lreq >> req2
      r2 `shouldBe` IdeResultOk [Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib2.hs")
                                            (Range (toPos (5,1)) (toPos (5,2)))]
    it "finds definition in the same component" $ do
      let u = filePathToUri $ cwd </> "test/testdata/gototest/src/Lib2.hs"
          lreq = setTypecheckedModule u
          req = liftToGhc $ TestDeferM $ findDef u (toPos (6,5))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResultOk [Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs")
                                           (Range (toPos (6,1)) (toPos (6,9)))]
    it "finds local definitions" $ do
      let u = filePathToUri $ cwd </> "test/testdata/gototest/src/Lib2.hs"
          lreq = setTypecheckedModule u
          req = liftToGhc $ TestDeferM $ findDef u (toPos (7,11))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResultOk [Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib2.hs")
                                            (Range (toPos (10,9)) (toPos (10,10)))]
      let req2 = liftToGhc $ TestDeferM $ findDef u (toPos (10,13))
      r2 <- dispatchRequestPGoto $ lreq >> req2
      r2 `shouldBe` IdeResultOk [Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib2.hs")
                                            (Range (toPos (9,9)) (toPos (9,10)))]
    it "finds local definition of record variable" $ do
      let u    = filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs"
          lreq = setTypecheckedModule u
          req  = liftToGhc $ TestDeferM $ findTypeDef u (toPos (11, 23))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResultOk
        [ Location
            (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs")
            (Range (toPos (8, 1)) (toPos (8, 29)))
        ]
    it "finds local definition of newtype variable" $ do
      let u    = filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs"
          lreq = setTypecheckedModule u
          req  = liftToGhc $ TestDeferM $ findTypeDef u (toPos (16, 21))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResultOk
        [ Location
            (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs")
            (Range (toPos (13, 1)) (toPos (13, 30)))
        ]
    it "finds local definition of sum type variable" $ do
      let u    = filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs"
          lreq = setTypecheckedModule u
          req  = liftToGhc $ TestDeferM $ findTypeDef u (toPos (21, 13))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResultOk
        [ Location
            (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs")
            (Range (toPos (18, 1)) (toPos (18, 26)))
        ]
    it "finds local definition of sum type contructor" $ do
      let u    = filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs"
          lreq = setTypecheckedModule u
          req  = liftToGhc $ TestDeferM $ findTypeDef u (toPos (24, 7))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResultOk 
        [ Location
            (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs") 
            (Range (toPos (18, 1)) (toPos (18, 26)))
        ]
    it "can not find non-local definition of type def" $ do
      let u    = filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs"
          lreq = setTypecheckedModule u
          req  = liftToGhc $ TestDeferM $ findTypeDef u (toPos (30, 17))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResultOk []
    it "find local definition of type def" $ do
      let u    = filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs"
          lreq = setTypecheckedModule u
          req  = liftToGhc $ TestDeferM $ findTypeDef u (toPos (35, 16))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResultOk
        [ Location
            (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs")
            (Range (toPos (18, 1)) (toPos (18, 26)))
        ]
    it "find type-definition of type def in component" $ do
      let u    = filePathToUri $ cwd </> "test/testdata/gototest/src/Lib2.hs"
          lreq = setTypecheckedModule u
          req  = liftToGhc $ TestDeferM $ findTypeDef u (toPos (13, 20))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResultOk
        [ Location
            (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs")
            (Range (toPos (8, 1)) (toPos (8, 29)))
        ]
    it "find definition of parameterized data type" $ do
      let u    = filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs"
          lreq = setTypecheckedModule u
          req  = liftToGhc $ TestDeferM $ findTypeDef u (toPos (40, 19))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResultOk
        [ Location
            (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs")
            (Range (toPos (37, 1)) (toPos (37, 31)))
        ]

    -- ---------------------------------

newtype TestDeferM a = TestDeferM (IdeDeferM a) deriving (Functor, Applicative, Monad)
instance LiftsToGhc TestDeferM where
  liftToGhc (TestDeferM (FreeT f)) = do
    x <- liftToGhc f
    case x of
      Pure a -> return a
      Free (Defer fp cb) -> do
        fp' <- liftIO $ canonicalizePath fp
        muc <- fmap (M.lookup fp' . uriCaches) getModuleCache
        case muc of
          Just uc -> liftToGhc $ TestDeferM $ cb uc
          Nothing -> error "No cache to lift IdeDeferM to IdeGhcM"
