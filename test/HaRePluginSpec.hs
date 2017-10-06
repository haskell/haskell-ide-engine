{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module HaRePluginSpec where

import           Control.Concurrent
import           Data.Aeson
import qualified Data.HashMap.Strict                   as H
import qualified GhcMod.ModuleLoader                   as GM
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.GhcModPlugin
import           Haskell.Ide.HaRePlugin
import           Language.Haskell.LSP.TH.DataTypesJSON
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

testPlugins :: IdePlugins
testPlugins = pluginDescToIdePlugins [("hare",hareDescriptor)]

-- TODO: break this out into a TestUtils file
dispatchRequest :: ToJSON a => PluginId -> CommandName -> a -> IO (IdeResponse Value)
dispatchRequest plugin com arg = do
  mv <- newEmptyMVar
  dispatchRequestP $ runPluginCommand plugin com (toJSON arg) (putMVar mv)
  takeMVar mv

dispatchRequestP :: IdeM a -> IO a
dispatchRequestP =
  cdAndDo "./test/testdata"
    . runIdeM testOptions (IdeState testPlugins GM.emptyModuleCache)

dispatchRequestPGoto :: IdeM a -> IO a
dispatchRequestPGoto =
  cdAndDo "./test/testdata/gototest"
    . runIdeM testOptions (IdeState testPlugins GM.emptyModuleCache)

-- ---------------------------------------------------------------------

hareSpec :: Spec
hareSpec = do
  describe "hare plugin commands(old plugin api)" $ do
    cwd <- runIO getCurrentDirectory
    -- ---------------------------------

    it "renames" $ do

      let req = HPT (filePathToUri "./HaReRename.hs") (toPos (5,1)) "foolong"
      r <- dispatchRequest "hare" "rename" req
      r `shouldBe`
        (IdeResponseOk
         $ toJSON
         $ WorkspaceEdit
           (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReRename.hs")
                               $ List [TextEdit (Range (Position 3 0) (Position 4 13))
                                         "foolong :: Int -> Int\nfoolong x = x + 3"])
           Nothing )

    -- ---------------------------------

    it "returns an error for invalid rename" $ do
      let req = HPT (filePathToUri "./HaReRename.hs") (toPos (15,1)) "foolong"
      r <- dispatchRequest "hare" "rename" req
      r `shouldBe` (IdeResponseFail
                      IdeError { ideCode = PluginError
                                , ideMessage = "rename: \"Invalid cursor position!\"", ideInfo = Null})

    -- ---------------------------------

    it "demotes" $ do
      let req = HP (filePathToUri "./HaReDemote.hs") (toPos (6,1))
      r <- dispatchRequest "hare" "demote" req
      r `shouldBe`
        (IdeResponseOk
         $ toJSON
         $ WorkspaceEdit
           (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReDemote.hs")
                               $ List [TextEdit (Range (Position 4 0) (Position 5 5))
                                         "  where\n    y = 7"])
           Nothing)

    -- ---------------------------------

    it "duplicates a definition" $ do

      let req = HPT (filePathToUri "./HaReRename.hs") (toPos (5,1)) "foonew"
      r <- dispatchRequest "hare" "dupdef" req
      r `shouldBe`
        (IdeResponseOk
         $ toJSON
         $ WorkspaceEdit
           (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReRename.hs")
                               $ List [TextEdit (Range (Position 6 0) (Position 6 0))
                                         "foonew :: Int -> Int\nfoonew x = x + 3\n\n"])
           Nothing)

    -- ---------------------------------

    it "converts if to case" $ do

      let req = HR (filePathToUri "./HaReCase.hs") (toPos (5,9)) (toPos (9,12))
      r <- dispatchRequest "hare" "iftocase" req
      r `shouldBe`
        (IdeResponseOk
         $ toJSON
         $ WorkspaceEdit
           (Just
            $ H.singleton (filePathToUri $ cwd </> "test/testdata/HaReCase.hs")
                          $ List [TextEdit (Range (Position 4 0) (Position 8 11))
                                  "foo x = case odd x of\n  True  ->\n    x + 3\n  False ->\n    x"])
           Nothing)

    -- ---------------------------------

    it "lifts one level" $ do

      let req = HP (filePathToUri "./HaReMoveDef.hs") (toPos (6,5))
      r <- dispatchRequest "hare" "liftonelevel" req
      r `shouldBe`
        (IdeResponseOk
         $ toJSON
         $ WorkspaceEdit
           (Just $ H.singleton
             ( filePathToUri $ cwd </> "test/testdata/HaReMoveDef.hs" )
             $ List [ TextEdit (Range (Position 6 0) (Position 6 0)) "y = 4\n\n"
                    , TextEdit (Range (Position 4 0) (Position 5 9)) ""
                    ])
           Nothing)

    -- ---------------------------------

    it "lifts to top level" $ do

      let req = HP (filePathToUri "./HaReMoveDef.hs") (toPos (12,9))
      r <- dispatchRequest "hare" "lifttotoplevel" req
      r `shouldBe`
        (IdeResponseOk
         $ toJSON
         $ WorkspaceEdit
          (Just $ H.singleton
             ( filePathToUri $ cwd </> "test/testdata/HaReMoveDef.hs")
             $ List [ TextEdit (Range (Position 13 0) (Position 13 0)) "\n"
                    , TextEdit (Range (Position 12 0) (Position 12 0)) "z = 7\n"
                    , TextEdit (Range (Position 10 0) (Position 11 13)) ""
                    ])
          Nothing)

    -- ---------------------------------

    it "deletes a definition" $ do
      let req = HP (filePathToUri "./FuncTest.hs") (toPos (6,1))
      r <- dispatchRequest "hare" "deletedef" req
      r `shouldBe`
        (IdeResponseOk
         $ toJSON
         $ WorkspaceEdit
          (Just $ H.singleton (filePathToUri $ cwd </> "test/testdata/FuncTest.hs")
                              $ List [TextEdit (Range (Position 4 0) (Position 6 0)) ""])
          Nothing)

    -- ---------------------------------

    it "generalises an applicative" $ do
      let req = HP (filePathToUri "./HaReGA1.hs") (toPos (4,1))
      r <- dispatchRequest "hare" "genapplicative" req
      r `shouldBe`
        (IdeResponseOk
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
                               $ List [TextEdit (Range (Position 6 0) (Position 6 0))
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
             $ List [ TextEdit (Range (Position 6 0) (Position 6 0)) "y = 4\n\n"
                    , TextEdit (Range (Position 4 0) (Position 5 9)) ""
                    ])
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
             $ List [ TextEdit (Range (Position 13 0) (Position 13 0)) "\n"
                    , TextEdit (Range (Position 12 0) (Position 12 0)) "z = 7\n"
                    , TextEdit (Range (Position 10 0) (Position 11 13)) ""
                    ])
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
      let u = filePathToUri "./app/Main.hs"
      let lreq = setTypecheckedModule u
      let req = findDef u (toPos (7,8))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResponseOk [Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs")
                                           (Range (toPos (6,1)) (toPos (6,9)))]
      let req2 = findDef u (toPos (7,20))
      r2 <- dispatchRequestPGoto $ lreq >> req2
      r2 `shouldBe` IdeResponseOk [Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib2.hs")
                                            (Range (toPos (5,1)) (toPos (5,2)))]
    it "finds definition in the same component" $ do
      let u = filePathToUri "./src/Lib2.hs"
      let lreq = setTypecheckedModule u
      let req = findDef u (toPos (6,5))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResponseOk [Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib.hs")
                                           (Range (toPos (6,1)) (toPos (6,9)))]
    it "finds local definitions" $ do
      let u = filePathToUri "./src/Lib2.hs"
      let lreq = setTypecheckedModule u
      let req = findDef u (toPos (7,11))
      r <- dispatchRequestPGoto $ lreq >> req
      r `shouldBe` IdeResponseOk [Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib2.hs")
                                           (Range (toPos (10,9)) (toPos (10,10)))]
      let req2 = findDef u (toPos (10,13))
      r2 <- dispatchRequestPGoto $ lreq >> req2
      r2 `shouldBe` IdeResponseOk [Location (filePathToUri $ cwd </> "test/testdata/gototest/src/Lib2.hs")
                                            (Range (toPos (9,9)) (toPos (9,10)))]


    -- ---------------------------------
