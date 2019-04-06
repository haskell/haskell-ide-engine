{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module GhcModPluginSpec where

import           Control.Exception
import qualified Data.HashMap.Strict                 as H
import qualified Data.Map                            as Map
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Set                            as S
import qualified Data.Text                           as T
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Plugin.GhcMod
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Support.HieExtras
import           Language.Haskell.LSP.Types          (TextEdit (..))
import           System.Directory
import           TestUtils

import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ghc-mod plugin" ghcmodSpec

-- ---------------------------------------------------------------------

testPlugins :: IdePlugins
testPlugins = pluginDescToIdePlugins [ghcmodDescriptor "ghcmod"]

-- ---------------------------------------------------------------------

ghcmodSpec :: Spec
ghcmodSpec =
  describe "ghc-mod plugin commands(old plugin api)" $ do
    it "runs the check command" $ withCurrentDirectory "./test/testdata" $ do
      fp <- makeAbsolute "./FileWithWarning.hs"
      let act = setTypecheckedModule arg
          arg = filePathToUri fp
          res = IdeResultOk $
            (Map.singleton arg (S.singleton diag), [])
          diag = Diagnostic (Range (toPos (4,7))
                                   (toPos (4,8)))
                            (Just DsError)
                            Nothing
                            (Just "ghcmod")
                            "Variable not in scope: x"
                            Nothing

      testCommand testPlugins act "ghcmod" "check" arg res

    -- ---------------------------------

    it "runs the lint command" $ withCurrentDirectory "./test/testdata" $ do
      fp <- makeAbsolute "FileWithWarning.hs"
      let uri = filePathToUri fp
          act = lintCmd' uri
          arg = uri
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,2,2,0)))
          res = IdeResultOk (T.pack fp <> ":6:9: Warning: Redundant do\NULFound:\NUL  do return (3 + x)\NULPerhaps:\NUL  return (3 + x)\n")
#else
          res = IdeResultOk (T.pack fp <> ":6:9: Warning: Redundant do\NULFound:\NUL  do return (3 + x)\NULWhy not:\NUL  return (3 + x)\n")
#endif
      testCommand testPlugins act "ghcmod" "lint" arg res

    -- ---------------------------------

    it "runs the info command" $ withCurrentDirectory "./test/testdata" $ do
      fp <- makeAbsolute "HaReRename.hs"
      let uri = filePathToUri fp
          act = infoCmd' uri "main"
          arg = IP uri "main"
          res = IdeResultOk "main :: IO () \t-- Defined at HaReRename.hs:2:1\n"
      -- ghc-mod tries to load the test file in the context of the hie project if we do not cd first.
      testCommand testPlugins act "ghcmod" "info" arg res

    -- ---------------------------------

    it "runs the type command" $ withCurrentDirectory "./test/testdata" $ do
      fp <- makeAbsolute "HaReRename.hs"
      let uri = filePathToUri fp
          act = do
            _ <- setTypecheckedModule uri
            liftToGhc $ newTypeCmd (toPos (5,9)) uri
          arg = TP False uri (toPos (5,9))
          res = IdeResultOk
            [(Range (toPos (5,9)) (toPos (5,10)), "Int")
            ,(Range (toPos (5,9)) (toPos (5,14)), "Int")
            ,(Range (toPos (5,1)) (toPos (5,14)), "Int -> Int")
            ]
      testCommand testPlugins act "ghcmod" "type" arg res

    it "runs the type command with an absolute path from another folder, correct params" $ do
      fp <- makeAbsolute "./test/testdata/HaReRename.hs"
      cd <- getCurrentDirectory
      cd2 <- getHomeDirectory
      bracket (setCurrentDirectory cd2)
              (\_->setCurrentDirectory cd)
              $ \_-> do
        let uri = filePathToUri fp
        let act = do
              _ <- setTypecheckedModule uri
              liftToGhc $ newTypeCmd (toPos (5,9)) uri
        let arg = TP False uri (toPos (5,9))
        let res = IdeResultOk
              [(Range (toPos (5,9)) (toPos (5,10)), "Int")
              ,(Range (toPos (5,9)) (toPos (5,14)), "Int")
              ,(Range (toPos (5,1)) (toPos (5,14)), "Int -> Int")
              ]
        testCommand testPlugins act "ghcmod" "type" arg res

    -- ---------------------------------

    it "runs the casesplit command" $ withCurrentDirectory "./test/testdata" $ do
      fp <- makeAbsolute "GhcModCaseSplit.hs"
      let uri = filePathToUri fp
          act = do
            _ <- setTypecheckedModule uri
            splitCaseCmd' uri (toPos (5,5))
          arg = HP uri (toPos (5,5))
          res = IdeResultOk $ WorkspaceEdit
            (Just $ H.singleton uri
                                $ List [TextEdit (Range (Position 4 0) (Position 4 10))
                                          "foo Nothing = ()\nfoo (Just x) = ()"])
            Nothing
      testCommand testPlugins act "ghcmod" "casesplit" arg res

    it "runs the casesplit command with an absolute path from another folder, correct params" $ do
      fp <- makeAbsolute "./test/testdata/GhcModCaseSplit.hs"
      cd <- getCurrentDirectory
      cd2 <- getHomeDirectory
      bracket (setCurrentDirectory cd2)
              (\_-> setCurrentDirectory cd)
              $ \_-> do
        let uri = filePathToUri fp
            act = do
              _ <- setTypecheckedModule uri
              splitCaseCmd' uri (toPos (5,5))
            arg = HP uri (toPos (5,5))
            res = IdeResultOk $ WorkspaceEdit
              (Just $ H.singleton uri
                                  $ List [TextEdit (Range (Position 4 0) (Position 4 10))
                                            "foo Nothing = ()\nfoo (Just x) = ()"])
              Nothing
        testCommand testPlugins act "ghcmod" "casesplit" arg res
