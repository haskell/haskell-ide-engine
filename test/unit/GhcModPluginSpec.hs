{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module GhcModPluginSpec where

import qualified Data.Map                            as Map
import qualified Data.Set                            as S
import qualified Data.Text                           as T
import           Haskell.Ide.Engine.Ghc
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Plugin.Generic
import           Haskell.Ide.Engine.Plugin.GhcMod
import           Haskell.Ide.Engine.PluginUtils
import           Language.Haskell.LSP.Types          ( toNormalizedUri )
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
      Right (_,env) <- runSingle testPlugins fp act
      case env of
        [] -> return ()
        [s] -> T.unpack s `shouldStartWith` "Loaded package environment from"
        ss -> fail $ "got:" ++ show ss
      let
          res = Right
            (Diagnostics (Map.singleton (toNormalizedUri arg) (S.singleton diag)), env)
          diag = Diagnostic (Range (toPos (4,7))
                                   (toPos (4,8)))
                            (Just DsError)
                            Nothing
                            (Just "bios")
                            "Variable not in scope: x"
                            Nothing

      testCommand testPlugins fp act "ghcmod" "check" arg res


-- ----------------------------------------------------------------------------

    it "runs the type command, find type" $ withCurrentDirectory "./test/testdata" $ do
      fp <- makeAbsolute "HaReRename.hs"
      let uri = filePathToUri fp
          act = do
            _ <- setTypecheckedModule uri
            liftToGhc $ newTypeCmd (toPos (5,9)) uri
          arg = TP False uri (toPos (5,9))
          res = Right
            [ (Range (toPos (5,9)) (toPos (5,10)), "Int")
            , (Range (toPos (5,1)) (toPos (5,14)), "Int -> Int")
            ]

      testCommand testPlugins fp act "ghcmod" "type" arg res


-- ----------------------------------------------------------------------------

    -- it "runs the casesplit command" $ withCurrentDirectory "./test/testdata" $ do
    --   fp <- makeAbsolute "GhcModCaseSplit.hs"
    --   let uri = filePathToUri fp
    --       act = do
    --         _ <- setTypecheckedModule uri
    --         -- splitCaseCmd' uri (toPos (5,5))
    --         splitCaseCmd uri (toPos (5,5))
    --       arg = HP uri (toPos (5,5))
    --       res = Right $ WorkspaceEdit
    --         (Just $ H.singleton uri
    --                             $ List [TextEdit (Range (Position 4 0) (Position 4 10))
    --                                       "foo Nothing = ()\nfoo (Just x) = ()"])
    --         Nothing
    --   testCommand testPlugins act "ghcmod" "casesplit" arg res
