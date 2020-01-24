{-# LANGUAGE OverloadedStrings #-}
module CabalHelperSpec where

import Data.Maybe (isJust)
import Haskell.Ide.Engine.Cradle
import HIE.Bios.Types (runCradle, cradleOptsProg, Cradle, CradleLoadResult(..))
import Test.Hspec
import System.FilePath
import System.Directory (findExecutable, getCurrentDirectory, removeFile)
import TestUtils

rootPath :: FilePath -> FilePath
rootPath cwd = cwd </> "test" </> "testdata" </> "cabal-helper"

implicitExePath :: FilePath -> FilePath
implicitExePath cwd = rootPath cwd </> "implicit-exe"

monoRepoPath :: FilePath -> FilePath
monoRepoPath cwd = rootPath cwd </> "mono-repo"

subPackagePath :: FilePath -> FilePath
subPackagePath cwd = rootPath cwd </> "sub-package"

simpleCabalPath :: FilePath -> FilePath
simpleCabalPath cwd = rootPath cwd </> "simple-cabal"

simpleStackPath :: FilePath -> FilePath
simpleStackPath cwd = rootPath cwd </> "simple-stack"

multiSourceDirsPath :: FilePath -> FilePath
multiSourceDirsPath cwd = rootPath cwd </> "multi-source-dirs"

spec :: Spec
spec = beforeAll_ setupStackFiles $ do
  describe "stack and cabal executables should be accesible" $ do
    it "cabal is accesible" $ do
      stack <- findExecutable "cabal"
      stack `shouldSatisfy` isJust
    it "stack is accesible" $ do
      cabal <- findExecutable "stack"
      cabal `shouldSatisfy` isJust
  describe "cabal-helper spec" $ do
    describe "find entry point" findCabalHelperEntryPointSpec
    describe "cradle discovery and loading" cabalHelperCradleSpec

cabalHelperCradleSpec :: Spec
cabalHelperCradleSpec = do
  cwd <- runIO getCurrentDirectory
  describe "dummy filepath, finds none-cradle" $ do
    it "implicit exe" $ do
      crdl <- cabalHelperCradle (implicitExePath cwd </> "File.hs")
      crdl `shouldSatisfy` isCabalCradle
    it "mono repo" $ do
      crdl <- cabalHelperCradle (monoRepoPath cwd </> "File.hs")
      crdl `shouldSatisfy` isCabalCradle
    it "stack repo" $ do
      crdl <- cabalHelperCradle (simpleStackPath cwd </> "File.hs")
      crdl `shouldSatisfy` isStackCradle
    it "cabal repo" $
      pendingWith "Can not work because of global `cabal.project`"
      -- crdl <- cabalHelperCradle (simpleCabalPath cwd </> "File.hs")
      -- crdl `shouldSatisfy` isCabalCradle
    it "sub package" $ do
      crdl <- cabalHelperCradle (subPackagePath cwd </> "File.hs")
      crdl `shouldSatisfy` isStackCradle
    it "multi-source-dirs" $ do
      crdl <- cabalHelperCradle (multiSourceDirsPath cwd </> "File.hs")
      crdl `shouldSatisfy` isStackCradle

  describe "existing projects" $ do
    it "implicit exe" $ do
      let fp = implicitExePath cwd </> "src" </> "Exe.hs"
      componentTest fp isCabalCradle
    it "mono repo" $ do
      let fp = monoRepoPath cwd </> "A" </> "Main.hs"
      componentTest fp isCabalCradle
    it "stack repo" $ do
      let fp = simpleStackPath cwd </> "MyLib.hs"
      componentTest fp isStackCradle
    it "cabal repo" $
      pendingWith "Can not work because of global `cabal.project`"
      -- let fp = (simpleCabalPath cwd </> "MyLib.hs")
      -- componentTest fp isStackCradle
    it "sub package" $ do
      let fp = subPackagePath cwd </> "plugins-api" </> "PluginLib.hs"
      componentTest fp isStackCradle
    it "multi-source-dirs, nested dir" $ do
      let fp = multiSourceDirsPath cwd </> "src" </> "input" </> "Lib.hs"
      componentTest fp isStackCradle
    it "multi-source-dirs" $ do
      let fp = multiSourceDirsPath cwd </> "src" </> "BetterLib.hs"
      componentTest fp isStackCradle

componentTest :: FilePath -> (Cradle -> Bool) -> Expectation
componentTest fp testCradleType = do
  crdl <- cabalHelperCradle  fp
  crdl `shouldSatisfy` testCradleType
  -- TODO: this works but CI crashes
  -- loadComponent crdl fp

loadComponent :: Cradle -> FilePath -> Expectation
loadComponent crdl fp = do
  result <- runCradle (cradleOptsProg crdl) (\_ -> return ()) fp
  case result of
    CradleFail err -> expectationFailure $ "Loading should not have failed: " ++ show err
    _ -> return ()
  return ()

findCabalHelperEntryPointSpec :: Spec
findCabalHelperEntryPointSpec = do
    cwd <- runIO getCurrentDirectory
    describe "implicit exe" $ do
      it "dummy filepath" $ do
        let dummyFile = implicitExePath cwd </> "File.hs"
        cabalTest dummyFile
      it "source component" $ do
        let libFile = implicitExePath cwd </> "src" </> "Lib.hs"
        cabalTest libFile
      it "executable component" $ do
        let mainFile = implicitExePath cwd </> "src" </> "Exe.hs"
        cabalTest mainFile

    describe "mono repo" $ do
      it "dummy filepath" $ do
        let dummyFile = monoRepoPath cwd </> "File.hs"
        cabalTest dummyFile
      it "existing executable" $ do
        let mainFile = monoRepoPath cwd </> "A" </> "Main.hs"
        cabalTest mainFile

    describe "sub package repo" $ do
      it "dummy filepath" $ do
        let dummyFile = subPackagePath cwd </> "File.hs"
        stackTest dummyFile
      it "existing executable" $ do
        let mainFile = subPackagePath cwd </> "plugins-api" </> "PluginLib.hs"
        stackTest mainFile

    describe "stack repo" $ do
      it "dummy filepath" $ do
        let dummyFile = simpleStackPath cwd </> "File.hs"
        stackTest dummyFile
      it "real filepath" $ do
        let dummyFile = simpleStackPath cwd </> "MyLib.hs"
        stackTest dummyFile

    describe "multi-source-dirs" $ do
      it "dummy filepath" $ do
        let dummyFile = multiSourceDirsPath cwd </> "File.hs"
        stackTest dummyFile

      it "real filepath" $ do
        let dummyFile = multiSourceDirsPath cwd </> "src" </> "BetterLib.hs"
        stackTest dummyFile

      it "nested filpath" $ do
        let dummyFile = multiSourceDirsPath cwd </> "src" </> "input" </> "Lib.hs"
        stackTest dummyFile

    describe "simple cabal repo" $
      it "Find project root with dummy filepath" $
        pendingWith "Change test-setup, we will always find `cabal.project` in root dir"

-- -------------------------------------------------------------

cabalTest :: FilePath -> IO ()
cabalTest fp = do
  entryPoint <- findCabalHelperEntryPoint fp
  let Just proj = entryPoint
      isCabal = isCabalProject proj
  shouldBe isCabal True

stackTest :: FilePath -> IO ()
stackTest fp = do
  entryPoint <- findCabalHelperEntryPoint fp
  let Just proj = entryPoint
      isStack = isStackProject proj
  shouldBe isStack True

-- -------------------------------------------------------------

setupStackFiles :: IO ()
setupStackFiles = do
  resolver <- readResolver
  cwd      <- getCurrentDirectory
  writeFile (implicitExePath cwd </> "stack.yaml") (standardStackYaml resolver)
  writeFile (monoRepoPath cwd </> "stack.yaml")    (monoRepoStackYaml resolver)
  writeFile (subPackagePath cwd </> "stack.yaml") (subPackageStackYaml resolver)
  writeFile (simpleStackPath cwd </> "stack.yaml") (standardStackYaml resolver)
  writeFile (multiSourceDirsPath cwd </> "stack.yaml")
            (standardStackYaml resolver)


cleanupStackFiles :: IO ()
cleanupStackFiles = do
  cwd <- getCurrentDirectory
  removeFile (implicitExePath cwd </> "stack.yaml")
  removeFile (monoRepoPath cwd </> "stack.yaml")
  removeFile (subPackagePath cwd </> "stack.yaml")
  removeFile (simpleStackPath cwd </> "stack.yaml")
  removeFile (multiSourceDirsPath cwd </> "stack.yaml")

-- -------------------------------------------------------------

standardStackYaml :: String -> String
standardStackYaml resolver = unlines
  [ "# WARNING: THIS FILE IS AUTOGENERATED IN test/utils/CabalHelperSpec. IT WILL BE OVERWRITTEN ON EVERY TEST RUN"
  , "resolver: " ++ resolver
  , "packages:"
  , "- '.'"
  , "extra-deps: []"
  , "flags: {}"
  , "extra-package-dbs: []"
  ]

monoRepoStackYaml :: String -> String
monoRepoStackYaml resolver = unlines
  [ "# WARNING: THIS FILE IS AUTOGENERATED IN test/utils/CabalHelperSpec. IT WILL BE OVERWRITTEN ON EVERY TEST RUN"
  , "resolver: " ++ resolver
  , "packages:"
  , "- 'A'"
  , "- 'B'"
  , "- 'C'"
  , "extra-deps: []"
  , "flags: {}"
  , "extra-package-dbs: []"
  ]

subPackageStackYaml :: String -> String
subPackageStackYaml resolver = unlines
  [ "# WARNING: THIS FILE IS AUTOGENERATED IN test/unit/CabalHelperSpec. IT WILL BE OVERWRITTEN ON EVERY TEST RUN"
  , "resolver: " ++ resolver
  , "packages:"
  , "- '.'"
  , "- 'plugins-api'"
  , "extra-deps: []"
  , "flags: {}"
  , "extra-package-dbs: []"
  ]