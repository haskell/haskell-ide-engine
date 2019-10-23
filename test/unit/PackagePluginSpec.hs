{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module PackagePluginSpec where

import           Control.Monad                  ( forM_ )
import qualified Data.Aeson                    as Json
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as H
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Plugin.Package
import           System.FilePath
import           System.Directory
import           Test.Hspec
import           TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Package plugin" packageSpec

testdata :: FilePath
testdata = "test" </> "testdata" </> "addPackageTest"

testPlugins :: IdePlugins
testPlugins = pluginDescToIdePlugins [packageDescriptor "package"]

cabalProject :: [FilePath]
cabalProject = ["cabal-lib", "cabal-exe"]

hpackProject :: [FilePath]
hpackProject = ["hpack-lib", "hpack-exe"]

packageSpec :: Spec
packageSpec = do
  cwd <- runIO getCurrentDirectory
  describe "Find correct package type" $ do
    forM_ hpackProject $ \hpack ->
      it ("hpack project find package.yaml (\"" ++ hpack ++ "\")") $ do
        let fp = testdata </> hpack
        packageType <- findPackageType fp
        packageType `shouldBe` HpackPackage (fp </> "package.yaml")
    forM_ cabalProject $ \cabal ->
      it ("hpack project find cabal file (\"" ++ cabal ++ "\")") $ do
        let fp = testdata </> cabal
        packageType <- findPackageType fp
        packageType `shouldBe` CabalPackage "add-package-test.cabal"
    it "Find no project description if none is present" $ do
      let fp = cwd </> testdata </> "invalid"
      packageType <- findPackageType fp
      packageType `shouldBe` NoPackage
    it "Throws exception if path is invalid" $ do
      let fp = testdata </> "unknownPath"
      findPackageType fp `shouldThrow` anyIOException
  describe "Add the package to the correct file" $ do
    it "Adds package to .cabal to executable component"
      $ withCurrentDirectory (testdata </> "cabal-exe")
      $ do
          let
            fp        = cwd </> testdata </> "cabal-exe"
            uri       = filePathToUri $ fp </> "add-package-test.cabal"
            args      = AddParams fp (fp </> "AddPackage.hs") "text"
            act       = addCmd' args
            textEdits =
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,0,0)))
              List
                [ TextEdit (Range (Position 0 0) (Position 7 27)) $ T.concat
                  [ "cabal-version: >=1.10\n"
                  , "name: add-package-test\n"
                  , "version: 0.1.0.0\n"
                  , "license: BSD3\n"
                  , "maintainer: luke_lau@icloud.com\n"
                  , "author: Luke Lau\n"
                  , "build-type: Simple\n"
                  , "extra-source-files:\n"
                  , "    ChangeLog.md"
                  ]
                , TextEdit (Range (Position 10 0) (Position 13 34)) $ T.concat
                  [ "    main-is: AddPackage.hs\n"
                  , "    default-language: Haskell2010\n"
                  , "    build-depends:\n"
                  , "        base >=4.7 && <5,\n"
                  , "        text -any"
                  ]
                ]
#else
              List -- TODO: this seems to indicate that the command does nothing
                [ TextEdit (Range (Position 0 0) (Position 7 27)) $ T.concat
                  [ "name: add-package-test\n"
                  , "version: 0.1.0.0\n"
                  , "cabal-version: >=1.10\n"
                  , "build-type: Simple\n"
                  , "license: BSD3\n"
                  , "maintainer: luke_lau@icloud.com\n"
                  , "author: Luke Lau\n"
                  , "extra-source-files:\n"
                  , "    ChangeLog.md"
                  ]
                , TextEdit (Range (Position 9 0) (Position 13 34)) $ T.concat
                  [ "executable  AddPackage\n"
                  , "    main-is: AddPackage.hs\n"
                  ]
                ]
#endif
            res = IdeResultOk
              $ WorkspaceEdit (Just $ H.singleton uri textEdits) Nothing
          testCommand testPlugins act "package" "add" args res

    it "Add package to .cabal to library component"
      $ withCurrentDirectory (testdata </> "cabal-lib")
      $ do
          let
            fp        = cwd </> testdata </> "cabal-lib"
            uri       = filePathToUri $ fp </> "add-package-test.cabal"
            args      = AddParams fp (fp </> "AddPackage.hs") "text"
            act       = addCmd' args
            textEdits =
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,0,0)))
              List
                [ TextEdit (Range (Position 0 0) (Position 7 27)) $ T.concat
                  [ "cabal-version: >=1.10\n"
                  , "name: add-package-test\n"
                  , "version: 0.1.0.0\n"
                  , "license: BSD3\n"
                  , "maintainer: luke_lau@icloud.com\n"
                  , "author: Luke Lau\n"
                  , "build-type: Simple\n"
                  , "extra-source-files:\n"
                  , "    ChangeLog.md"
                  ]
                , TextEdit (Range (Position 10 0) (Position 13 34)) $ T.concat
                  [ "    exposed-modules:\n"
                  , "        AddPackage\n"
                  , "    default-language: Haskell2010\n"
                  , "    build-depends:\n"
                  , "        base >=4.7 && <5,\n"
                  , "        text -any"
                  ]
                ]
#else
              List
                [ TextEdit (Range (Position 0 0) (Position 7 27)) $ T.concat
                  [ "name: add-package-test\n"
                  , "version: 0.1.0.0\n"
                  , "cabal-version: >=1.10\n"
                  , "build-type: Simple\n"
                  , "license: BSD3\n"
                  , "maintainer: luke_lau@icloud.com\n"
                  , "author: Luke Lau\n"
                  , "extra-source-files:\n"
                  , "    ChangeLog.md"
                  ]
                , TextEdit (Range (Position 10 0) (Position 13 34)) $ T.concat
                  [ "    exposed-modules:\n"
                  , "        AddPackage\n"
                  , "    build-depends:\n"
                  , "        base >=4.7 && <5,\n"
                  , "        text -any\n"
                  , "    default-language: Haskell2010\n"
                  ]
                ]
#endif
            res = IdeResultOk
              $ WorkspaceEdit (Just $ H.singleton uri textEdits) Nothing
          testCommand testPlugins act "package" "add" args res


    it "Adds package to package.yaml to executable component"
      $ withCurrentDirectory (testdata </> "hpack-exe")
      $ do
          let
            fp   = cwd </> testdata </> "hpack-exe"
            uri  = filePathToUri $ fp </> "package.yaml"
            args = AddParams fp (fp </> "app" </> "Asdf.hs") "zlib"
            act  = addCmd' args
            res  = IdeResultOk
              $ WorkspaceEdit (Just $ H.singleton uri textEdits) Nothing
            textEdits = List
              [ TextEdit (Range (Position 0 0) (Position 32 0)) $ T.concat
                  [ "copyright: 2018 Author name here\n"
                  , "maintainer: example@example.com\n"
                  , "dependencies:\n"
                  , "- zlib\n"
                  , "- base >= 4.7 && < 5\n"
                  , "name: asdf\n"
                  , "version: 0.1.0.0\n"
                  , "extra-source-files:\n"
                  , "- README.md\n"
                  , "- ChangeLog.md\n"
                  , "author: Author name here\n"
                  , "github: githubuser/asdf\n"
                  , "license: BSD3\n"
                  , "executables:\n"
                  , "  asdf-exe:\n"
                  , "    source-dirs: app\n"
                  , "    main: Main.hs\n"
                  , "    ghc-options:\n"
                  , "    - -threaded\n"
                  , "    - -rtsopts\n"
                  , "    - -with-rtsopts=-N\n"
                  , "description: Please see the README on GitHub at <https://github.com/githubuser/asdf#readme>\n"
                  ]
              ]
          testCommand testPlugins act "package" "add" args res

    it "Add package to package.yaml to library component"
      $ withCurrentDirectory (testdata </> "hpack-lib")
      $ do
          let
            fp   = cwd </> testdata </> "hpack-lib"
            uri  = filePathToUri $ fp </> "package.yaml"
            args = AddParams fp (fp </> "app" </> "Asdf.hs") "zlib"
            act  = addCmd' args
            res  = IdeResultOk
              $ WorkspaceEdit (Just $ H.singleton uri textEdits) Nothing
            textEdits =
              List
                [ TextEdit (Range (Position 0 0) (Position 25 0)) $ T.concat
                    [ "library:\n"
                    , "  source-dirs: app\n"
                    , "  dependencies:\n"
                    , "  - zlib\n"
                    , "  - base >= 4.7 && < 5\n"
                    , "copyright: 2018 Author name here\n"
                    , "maintainer: example@example.com\n"
                    , "name: asdf\n"
                    , "version: 0.1.0.0\n"
                    , "extra-source-files:\n"
                    , "- README.md\n"
                    , "- ChangeLog.md\n"
                    , "author: Author name here\n"
                    , "github: githubuser/asdf\n"
                    , "license: BSD3\n"
                    , "description: Please see the README on GitHub at <https://github.com/githubuser/asdf#readme>\n"
                    ]
                ]
          testCommand testPlugins act "package" "add" args res

    it "Do nothing on NoPackage"
      $ withCurrentDirectory (testdata </> "invalid")
      $ do
          let
            fp   = cwd </> testdata </> "invalid"
            args = AddParams fp (fp </> "app" </> "Asdf.hs") "zlib"
            act  = addCmd' args
            res =
              IdeResultFail
                (IdeError PluginError
                          "No package.yaml or .cabal found"
                          Json.Null
                )
          testCommand testPlugins act "package" "add" args res
