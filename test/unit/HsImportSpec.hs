{-# LANGUAGE OverloadedStrings #-}
module HsImportSpec where

import           Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Plugin.HsImport
import qualified Haskell.Ide.Engine.Config as Config
import qualified Haskell.Ide.Engine.Plugin.Brittany as Brittany
import qualified Haskell.Ide.Engine.Plugin.Ormolu   as Ormolu
import qualified Haskell.Ide.Engine.Plugin.Floskell as Floskell
import           System.Directory
import           System.FilePath
import           Test.Hspec
import           TestUtils


main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "hsimport plugin" hsImportSpec

-- ---------------------------------------------------------------------

testPlugins :: IdePlugins
testPlugins = pluginDescToIdePlugins
    [ Brittany.brittanyDescriptor "brittany"
    , Floskell.floskellDescriptor "floskell"
    , Ormolu.ormoluDescriptor "ormolu"
    ]

brittanyFilePath :: FilePath
brittanyFilePath = "test" </> "testdata" </> "CodeActionImportList.hs"

dispatchRequestP :: IdeGhcM a -> IO a
dispatchRequestP act = do
  cwd <- liftIO $ getCurrentDirectory
  runIGM testPlugins (cwd </> "test" </> "testdata" </> "File.hs") act

-- ---------------------------------------------------------------------

hsImportSpec :: Spec
hsImportSpec = do
  describe "formats with brittany" $ hsImportSpecRunner "brittany"
    [ -- Expected output for simple format.
      [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Control.Monad\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Control.Monad                  ( when )\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Data.Maybe                     ( Maybe )\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Data.Maybe                     ( Maybe(..) )\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Data.Maybe                     ( Maybe(Nothing) )\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Data.Function                  ( ($) )\n"
      ]
    ]
  describe "formats with floskell" $ hsImportSpecRunner "floskell"
    [ -- Expected output for simple format.
      [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Control.Monad\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Control.Monad (when)\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Data.Maybe (Maybe)\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Data.Maybe (Maybe(..))\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Data.Maybe (Maybe(Nothing))\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Data.Function (($))\n"
      ]
    ]
  describe "formats with ormolu" $ case ghcVersion of
      GHC86 -> hsImportSpecRunner "ormolu"
        [ -- Expected output for simple format.
          [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import Control.Monad\n"
          ]
        , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import Control.Monad (when)\n"
          ]
        , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import Data.Maybe (Maybe)\n"
          ]
        , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import Data.Maybe (Maybe (..))\n"
          ]
        , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import Data.Maybe (Maybe (Nothing))\n"
          ]
        , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import Data.Function (($))\n"
          ]
        ]
      _ -> it "is NOP formatter" $
            pendingWith "Ormolu only supported by GHC >= 8.6. Need to restore this."

-- ---------------------------------------------------------------------
-- Parameterized HsImport Spec.
-- ---------------------------------------------------------------------
hsImportSpecRunner :: T.Text -> [[TextEdit]] -> Spec
hsImportSpecRunner formatterName [e1, e2, e3, e4, e5, e6] = do
    it "formats" $ do
      fp <- makeAbsolute brittanyFilePath
      let uri = filePathToUri fp
      let act = importModule (ImportParams uri Simple "Control.Monad")

      IdeResultOk (WorkspaceEdit (Just changes) _) <- runSingle' (setFormatter formatterName) testPlugins fp act
      case Map.lookup uri changes of
          Just (List val) -> val `shouldBe` e1
          Nothing -> fail "No Change found"

    it "import-list formats" $ do
      fp <- makeAbsolute brittanyFilePath
      let uri = filePathToUri fp
      let act = importModule (ImportParams uri (Complex (Import $ Only "when")) "Control.Monad")

      IdeResultOk (WorkspaceEdit (Just changes) _) <- runSingle' (setFormatter formatterName) testPlugins fp act
      case Map.lookup uri changes of
          Just (List val) -> val `shouldBe` e2
          Nothing -> fail "No Change found"

    it "import-list type formats" $ do
      fp <- makeAbsolute brittanyFilePath
      let uri = filePathToUri fp
      let act = importModule (ImportParams uri (Complex (Import $ Only "Maybe")) "Data.Maybe")

      IdeResultOk (WorkspaceEdit (Just changes) _) <- runSingle' (setFormatter formatterName) testPlugins fp act
      case Map.lookup uri changes of
          Just (List val) -> val `shouldBe` e3
          Nothing -> fail "No Change found"

    it "import-list constructor formats" $ do
      fp <- makeAbsolute brittanyFilePath
      let uri = filePathToUri fp
      let act = importModule (ImportParams uri (Complex (Import $ AllOf "Maybe")) "Data.Maybe")

      IdeResultOk (WorkspaceEdit (Just changes) _) <- runSingle' (setFormatter formatterName) testPlugins fp act
      case Map.lookup uri changes of
          Just (List val) -> val `shouldBe` e4
          Nothing -> fail "No Change found"

    it "import-list constructor formats" $ do
      fp <- makeAbsolute brittanyFilePath
      let uri = filePathToUri fp
      let act = importModule (ImportParams uri (Complex (Import $ OneOf "Maybe" "Nothing")) "Data.Maybe")

      IdeResultOk (WorkspaceEdit (Just changes) _) <- runSingle' (setFormatter formatterName) testPlugins fp act
      case Map.lookup uri changes of
          Just (List val) -> val `shouldBe` e5
          Nothing -> fail "No Change found"

    it "import-list infix function formats" $ do
      fp <- makeAbsolute brittanyFilePath
      let uri = filePathToUri fp
      let act = importModule (ImportParams uri (Complex (Import $ Only "$")) "Data.Function")

      IdeResultOk (WorkspaceEdit (Just changes) _) <- runSingle' (setFormatter formatterName) testPlugins fp act
      case Map.lookup uri changes of
          Just (List val) -> val `shouldBe` e6
          Nothing -> fail "No Change found"

-- Silence warnings
hsImportSpecRunner formatter args =
  error $ "Not the right amount of arguments for \"hsImportSpec ("
    ++ T.unpack formatter
    ++ ")\", got "
    ++ show (length args)

setFormatter :: T.Text -> Config.Config -> Config.Config
setFormatter formatterName cfg = cfg { Config.formattingProvider = formatterName }