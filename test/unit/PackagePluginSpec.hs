{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PackagePluginSpec where
import qualified Data.Aeson as Json
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Plugin.Package
import           System.FilePath
import           System.Directory
import           Test.Hspec
import           TestUtils

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Package plugin" packageSpec

testdata :: FilePath
testdata = "test/testdata/addPackageTest"

testPlugins :: IdePlugins
testPlugins = pluginDescToIdePlugins [packageDescriptor "package"]

packageSpec :: Spec
packageSpec = do
  cwd <- runIO getCurrentDirectory
  describe "Find correct package type" $ do
    it "Find cabal file in cabal project" $ do
      let fp = testdata </> "cabal"
      packageType <- findPackageType fp
      packageType `shouldBe` CabalPackage "add-package-test.cabal"

    it "Find package.yaml file in hpack project" $ do
      let fp = testdata </> "hpack"
      packageType <-findPackageType fp
      packageType `shouldBe` HpackPackage (fp </> "package.yaml")

    it "Find package.yaml file in hpack project with generated cabal" $ do
      let fp = testdata </> "hybrid"
      packageType <- findPackageType fp
      packageType `shouldBe` HpackPackage (fp </> "package.yaml")

    it "Find no project description if none is present " $ do
      let fp = testdata </> "invalid"
      packageType <- findPackageType fp
      packageType `shouldBe` NoPackage

    it "Throws exception if path is invalid" $ do
      let fp = testdata </> "unknownPath"
      findPackageType fp `shouldThrow` anyIOException
  describe "Add the package to the correct file" $ do
    it "Add package to .cabal" $ withCurrentDirectory (testdata </> "cabal") $ do
      let
        fp = cwd </> testdata </> "cabal"
        uri = filePathToUri $ fp </> "add-package-test.cabal"
        args = AddParams fp (fp </> "AddPackage.hs") "text"
        act = addCmd' args
        textEdits =
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
        res = IdeResultOk $ WorkspaceEdit
            (Just $ H.singleton uri textEdits)
            Nothing
      testCommand testPlugins act "package" "add" args res
    it "Add package to package.yaml" $ withCurrentDirectory (testdata </> "hpack") $ do
      let
        fp = cwd </> testdata </> "hpack"
        uri = filePathToUri $ fp </> "package.yaml"
        args = AddParams fp (fp </> "app" </>"Asdf.hs") "zlib"
        act = addCmd' args
        res = IdeResultOk $ WorkspaceEdit
              (Just $ H.singleton uri textEdits)
              Nothing
        textEdits = List
                [ TextEdit (Range (Position 0 0) (Position 48 0)) $ T.concat
                  [ "library:\n"
                  , "  source-dirs: src\n"
                  , "tests:\n"
                  , "  asdf-test:\n"
                  , "    source-dirs: test\n"
                  , "    main: Spec.hs\n"
                  , "    ghc-options:\n"
                  , "    - -threaded\n"
                  , "    - -rtsopts\n"
                  , "    - -with-rtsopts=-N\n"
                  , "    dependencies:\n"
                  , "    - asdf\n"
                  , "copyright: 2018 Author name here\n"
                  , "maintainer: example@example.com\n"
                  , "dependencies:\n"
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
                  , "    dependencies:\n"
                  , "    - zlib\n"
                  , "    - asdf\n"
                  , "description: Please see the README on GitHub at <https://github.com/githubuser/asdf#readme>\n"
                  ]
                ]
      testCommand testPlugins act "package" "add" args res
    it "Add package to package.yaml in hpack project with generated cabal" $ withCurrentDirectory (testdata </> "hybrid") $ do
      let
        fp = cwd </> testdata </> "hybrid"
        uri = filePathToUri $ fp </> "package.yaml"
        args = AddParams fp (fp </> "app" </>"Asdf.hs") "zlib"
        act = addCmd' args
        res = IdeResultOk $ WorkspaceEdit
              (Just $ H.singleton uri textEdits)
              Nothing
        textEdits = List
                [ TextEdit (Range (Position 0 0) (Position 48 0)) $ T.concat
                  [ "library:\n"
                  , "  source-dirs: src\n"
                  , "tests:\n"
                  , "  asdf-test:\n"
                  , "    source-dirs: test\n"
                  , "    main: Spec.hs\n"
                  , "    ghc-options:\n"
                  , "    - -threaded\n"
                  , "    - -rtsopts\n"
                  , "    - -with-rtsopts=-N\n"
                  , "    dependencies:\n"
                  , "    - asdf\n"
                  , "copyright: 2018 Author name here\n"
                  , "maintainer: example@example.com\n"
                  , "dependencies:\n"
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
                  , "    dependencies:\n"
                  , "    - zlib\n"
                  , "    - asdf\n"
                  , "description: Please see the README on GitHub at <https://github.com/githubuser/asdf#readme>\n"
                  ]
                ]
      testCommand testPlugins act "package" "add" args res

    it "Do nothing on NoPackage" $ withCurrentDirectory (testdata </> "invalid") $ do
      let
        fp = cwd </> testdata </> "invalid"
        args = AddParams fp (fp </> "app" </>"Asdf.hs") "zlib"
        act = addCmd' args
        res = IdeResultFail (IdeError PluginError "No package.yaml or .cabal found" Json.Null)
      testCommand testPlugins act "package" "add" args res