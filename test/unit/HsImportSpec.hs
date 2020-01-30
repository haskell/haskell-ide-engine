{-# LANGUAGE OverloadedStrings #-}
module HsImportSpec where

import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict                as Map
import qualified Data.Text                          as T
import qualified Haskell.Ide.Engine.Config          as Config
import           Haskell.Ide.Engine.MonadTypes
import qualified Haskell.Ide.Engine.Plugin.Brittany as Brittany
import qualified Haskell.Ide.Engine.Plugin.Floskell as Floskell
import           Haskell.Ide.Engine.Plugin.HsImport
import qualified Haskell.Ide.Engine.Plugin.Ormolu   as Ormolu
import qualified Haskell.Ide.Engine.Plugin.Stylish  as Stylish
import           Haskell.Ide.Engine.PluginUtils
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
    , Stylish.stylishDescriptor "stylish"
    ]

codeActionImportList :: FilePath
codeActionImportList = "test" </> "testdata" </> "CodeActionImportList.hs"

codeActionBigImportList :: FilePath
codeActionBigImportList = "test" </> "testdata" </> "CodeActionImportListElaborate.hs"

dispatchRequestP :: IdeGhcM a -> IO a
dispatchRequestP act = do
  cwd <- liftIO getCurrentDirectory
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
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 32))) $
        "import           System.IO                      ( IO\n" <>
        "                                                , hPutStrLn\n" <>
        "                                                )"
      ]
    , [ TextEdit (Range (toPos (3, 1)) (toPos (3, 99))) $
        "import           Data.List                      ( find\n" <>
        "                                                , head\n" <>
        "                                                , last\n" <>
        "                                                , tail\n" <>
        "                                                , init\n" <>
        "                                                , union\n" <>
        "                                                , (\\\\)\n" <>
        "                                                , null\n" <>
        "                                                , length\n" <>
        "                                                , cons\n" <>
        "                                                , uncons\n" <>
        "                                                , reverse\n" <>
        "                                                )"
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
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 32))) "import           System.IO (IO, hPutStrLn)"
      ]
    , [ TextEdit (Range (toPos (3, 1)) (toPos (3, 99))) $
        "import           Data.List (find, head, last, tail, init, union, (\\\\), null\n" <>
        "                          , length, cons, uncons, reverse)"
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
        , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 32))) "import System.IO (IO, hPutStrLn)"
          ]
        , [ TextEdit (Range (toPos (3, 1)) (toPos (3, 99))) "import Data.List ((\\\\), cons, find, head, init, last, length, null, reverse, tail, uncons, union)"
          ]
        ]
      _ -> it "is NOP formatter" $
            pendingWith "Ormolu only supported by GHC >= 8.6. Need to restore this."
  describe "formats with stylish" $ hsImportSpecRunner "stylish"
    [ -- Expected output for simple format.
      [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Control.Monad\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Control.Monad (when)\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Data.Maybe (Maybe)\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Data.Maybe (Maybe (..))\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Data.Maybe (Maybe (Nothing))\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 1))) "import           Data.Function (($))\n"
      ]
    , [ TextEdit (Range (toPos (2, 1)) (toPos (2, 32))) "import           System.IO (IO, hPutStrLn)"
      ]
    , [ TextEdit (Range (toPos (3, 1)) (toPos (3, 99))) $
        "import           Data.List (cons, find, head, init, last, length, null, reverse,\n" <>
        "                            tail, uncons, union, (\\\\))"
      ]
    ]
-- ---------------------------------------------------------------------
-- Parameterized HsImport Spec.
-- ---------------------------------------------------------------------
hsImportSpecRunner :: T.Text -> [[TextEdit]] -> Spec
hsImportSpecRunner formatterName [e1, e2, e3, e4, e5, e6, e7, e8] = do
    it "formats" $ do
      fp <- makeAbsolute codeActionImportList
      let uri = filePathToUri fp
      let act = importModule (ImportParams uri Simple "Control.Monad")
      expectHsImportResult formatterName fp uri e1 act

    it "import-list formats" $ do
      fp <- makeAbsolute codeActionImportList
      let uri = filePathToUri fp
      let act = importModule (ImportParams uri (Complex (Import $ Only "when")) "Control.Monad")
      expectHsImportResult formatterName fp uri e2 act

    it "import-list type formats" $ do
      fp <- makeAbsolute codeActionImportList
      let uri = filePathToUri fp
      let act = importModule (ImportParams uri (Complex (Import $ Only "Maybe")) "Data.Maybe")
      expectHsImportResult formatterName fp uri e3 act

    it "import-list constructor formats" $ do
      fp <- makeAbsolute codeActionImportList
      let uri = filePathToUri fp
      let act = importModule (ImportParams uri (Complex (Import $ AllOf "Maybe")) "Data.Maybe")
      expectHsImportResult formatterName fp uri e4 act

    it "import-list constructor formats" $ do
      fp <- makeAbsolute codeActionImportList
      let uri = filePathToUri fp
      let act = importModule (ImportParams uri (Complex (Import $ OneOf "Maybe" "Nothing")) "Data.Maybe")
      expectHsImportResult formatterName fp uri e5 act

    it "import-list infix function formats" $ do
      fp <- makeAbsolute codeActionImportList
      let uri = filePathToUri fp
      let act = importModule (ImportParams uri (Complex (Import $ Only "$")) "Data.Function")
      expectHsImportResult formatterName fp uri e6 act

    it "import-list with existing entry formats" $ do
      fp <- makeAbsolute codeActionBigImportList
      let uri = filePathToUri fp
      let act = importModule (ImportParams uri (Complex (Import $ Only "hPutStrLn")) "System.IO")
      expectHsImportResult formatterName fp uri e7 act

    it "import-list with forced overflow formats" $ do
      fp <- makeAbsolute codeActionBigImportList
      let uri = filePathToUri fp
      let act = importModule (ImportParams uri (Complex (Import $ Only "reverse")) "Data.List")
      expectHsImportResult formatterName fp uri e8 act


-- Silence warnings
hsImportSpecRunner formatter args =
  error $ "Not the right amount of arguments for \"hsImportSpec ("
    ++ T.unpack formatter
    ++ ")\", got "
    ++ show (length args)

setFormatter :: T.Text -> Config.Config -> Config.Config
setFormatter formatterName cfg = cfg { Config.formattingProvider = formatterName }

expectHsImportResult :: T.Text -> FilePath -> Uri -> [TextEdit] -> IdeGhcM (IdeResult WorkspaceEdit) -> IO ()
expectHsImportResult formatterName fp uri expectedChanges act = do
  IdeResultOk (WorkspaceEdit (Just changes) _) <- runSingle' (setFormatter formatterName) testPlugins fp act
  case Map.lookup uri changes of
      Just (List val) -> val `shouldBe` expectedChanges
      Nothing         -> fail "No Change found"
