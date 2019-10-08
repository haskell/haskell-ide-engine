-- |Module for Help messages and traget descriptions
module Help where

import           Development.Shake
import           Data.List                                ( intersperse
                                                          , intercalate
                                                          )

import           Env
import           Print
import           Version
import           BuildSystem
import           Cabal

printUsage :: Action ()
printUsage = do
  printLine ""
  printLine "Usage:"
  printLineIndented "stack install.hs <target>"
  printLineIndented "or"
  printLineIndented "cabal new-run install.hs --project-file install/shake.project <target>"

-- | short help message is printed by default
shortHelpMessage :: Action ()
shortHelpMessage = do
  hieVersions <- getHieVersions
  printUsage
  printLine ""
  printLine "Targets:"
  mapM_ (printLineIndented . showTarget (spaces hieVersions)) (targets hieVersions)
  printLine ""
 where
  spaces hieVersions = space (targets hieVersions)
  targets hieVersions =
    [ ("help", "Show help message including all targets")
    , emptyTarget
    , buildTarget
    , buildAllTarget
    , hieTarget $ last hieVersions
    , buildDataTarget
    , cabalGhcsTarget
    ]

-- | A record that specifies for each build system which versions of @hie@ can be built.
data BuildableVersions = BuildableVersions
  { stackVersions :: [VersionNumber]
  , cabalVersions :: [VersionNumber]
  }

getDefaultBuildSystemVersions :: BuildableVersions -> [VersionNumber]
getDefaultBuildSystemVersions BuildableVersions {..}
  | isRunFromStack = stackVersions
  | isRunFromCabal = cabalVersions
  | otherwise      = error $ "unknown build system: " ++ buildSystem

helpMessage :: BuildableVersions -> Action ()
helpMessage versions@BuildableVersions {..} = do
  printUsage
  printLine ""
  printLine "Targets:"
  mapM_ (printLineIndented . showTarget spaces) targets
  printLine ""
 where
  spaces = space targets
  -- All targets the shake file supports
  targets :: [(String, String)]
  targets = intercalate
    [emptyTarget]
    [ generalTargets
    , defaultTargets
    , stackTargets
    , cabalTargets
    , [macosIcuTarget]
    ]

  -- All targets with their respective help message.
  generalTargets = [helpTarget]

  defaultTargets = [buildTarget, buildAllTarget, buildDataTarget]
    ++ map hieTarget (getDefaultBuildSystemVersions versions)

  stackTargets =
    [ stackTarget buildTarget
      , stackTarget buildAllTarget
      , stackTarget buildDataTarget
      ]
      ++ (if isRunFromStack then [stackTarget installCabalTarget] else [])
      ++ map (stackTarget . hieTarget) stackVersions

  cabalTargets =
    [ cabalGhcsTarget
      , cabalTarget buildTarget
      , cabalTarget buildAllTarget
      , cabalTarget buildDataTarget
      ]
      ++ map (cabalTarget . hieTarget) cabalVersions

-- | Empty target. Purpose is to introduce a newline between the targets
emptyTarget :: (String, String)
emptyTarget = ("", "")

targetWithBuildSystem :: String -> TargetDescription -> TargetDescription
targetWithBuildSystem system (target, description) =
  (system ++ "-" ++ target, description ++ "; with " ++ system)

stackTarget :: TargetDescription -> TargetDescription
stackTarget = targetWithBuildSystem "stack"

cabalTarget :: TargetDescription -> TargetDescription
cabalTarget = targetWithBuildSystem "cabal"

hieTarget :: String -> TargetDescription
hieTarget version =
  ("hie-" ++ version, "Builds hie for GHC version " ++ version)

buildTarget :: TargetDescription
buildTarget = ("build", "Builds hie with all installed GHCs")

buildDataTarget :: TargetDescription
buildDataTarget =
  ("build-data", "Get the required data-files for `hie` (Hoogle DB)")

buildAllTarget :: TargetDescription
buildAllTarget =
  ("build-all", "Builds hie for all installed GHC versions and the data files")

-- speical targets

macosIcuTarget :: TargetDescription
macosIcuTarget = ("icu-macos-fix", "Fixes icu related problems in MacOS")

helpTarget :: TargetDescription
helpTarget = ("help", "Show help message including all targets")

cabalGhcsTarget :: TargetDescription
cabalGhcsTarget =
  ( "cabal-ghcs"
  , "Show all GHC versions that can be installed via `cabal-build` and `cabal-build-all`."
  )

installCabalTarget :: TargetDescription
installCabalTarget =
  ( "install-cabal"
  , "Install the cabal executable. It will install the required minimum version for hie (currently " ++ versionToString requiredCabalVersion ++ ") if it isn't already present in $PATH"
  )

-- | Creates a message of the form "a, b, c and d", where a,b,c,d are GHC versions.
-- If there is no GHC in the list of `hieVersions`
allVersionMessage :: [String] -> String
allVersionMessage wordList = case wordList of
  []  -> ""
  [a] -> show a
  (a : as) ->
    let msg         = intersperse ", " wordList
        lastVersion = last msg
    in  concat $ init (init msg) ++ [" and ", lastVersion]
