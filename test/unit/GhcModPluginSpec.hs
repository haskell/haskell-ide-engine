{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module GhcModPluginSpec where

import           Control.Exception
import qualified Data.Map                            as Map
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Set                            as S
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Plugin.GhcMod
import           Haskell.Ide.Engine.PluginUtils
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
testPlugins = mkIdePlugins [ghcmodDescriptor]

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

      runIGM testPlugins act `shouldReturn` res

    -- ---------------------------------

    it "runs the type command" $ withCurrentDirectory "./test/testdata" $ do
      fp <- makeAbsolute "HaReRename.hs"
      let uri = filePathToUri fp
          act = do
            _ <- setTypecheckedModule uri
            liftToGhc $ newTypeCmd (toPos (5,9)) uri
          res = IdeResultOk
            [(Range (toPos (5,9)) (toPos (5,10)), "Int")
            ,(Range (toPos (5,9)) (toPos (5,14)), "Int")
            ,(Range (toPos (5,1)) (toPos (5,14)), "Int -> Int")
            ]
      runIGM testPlugins act `shouldReturn` res

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
        let res = IdeResultOk
              [(Range (toPos (5,9)) (toPos (5,10)), "Int")
              ,(Range (toPos (5,9)) (toPos (5,14)), "Int")
              ,(Range (toPos (5,1)) (toPos (5,14)), "Int -> Int")
              ]
        runIGM testPlugins act `shouldReturn` res
