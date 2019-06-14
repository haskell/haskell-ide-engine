-- |Module for Help messages and traget descriptions
module Help where

import           Development.Shake
import           Data.List                                ( intersperse
                                                          , intercalate
                                                          )

import Env
import Print
import Version

printUsage :: Action ()
printUsage = do
  out ""
  out "Usage:"
  out' ("stack install.hs <target>")
  out' "or"
  out' ("cabal new-run install.hs --project-file shake.project <target>")

-- | short help message is printed by default
shortHelpMessage :: Action ()
shortHelpMessage = do
  hieVersions <- getHieVersions
  printUsage
  out ""
  out "Targets:"
  mapM_ (out' . showTarget (spaces hieVersions)) (targets hieVersions)
  out ""
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



helpMessage :: Action ()
helpMessage = do
  hieVersions <- getHieVersions
  printUsage
  out ""
  out "Targets:"
  mapM_ (out' . showTarget (spaces hieVersions)) (targets hieVersions)
  out ""
 where
  spaces hieVersions = space (targets hieVersions)
  -- All targets the shake file supports
  targets :: [VersionNumber] -> [(String, String)]
  targets hieVersions = intercalate
    [emptyTarget]
    [ generalTargets
    , defaultTargets hieVersions
    , stackTargets hieVersions
    , cabalTargets hieVersions
    , [macosIcuTarget]
    ]

  -- All targets with their respective help message.
  generalTargets =
    [ helpTarget
    ]

  defaultTargets hieVersions =
    [ buildTarget
    , buildAllTarget
    , buildDataTarget
    ]
      ++ map hieTarget hieVersions

  stackTargets hieVersions =
    [ stackTarget buildTarget
    , stackTarget buildAllTarget
    , stackTarget buildDataTarget
    ]
      ++ map (stackTarget . hieTarget) hieVersions

  cabalTargets hieVersions =
    [ cabalGhcsTarget
    , cabalTarget buildTarget
    , cabalTarget buildAllTarget
    , cabalTarget buildDataTarget
    ]
      ++ map (cabalTarget . hieTarget) hieVersions

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
  ( "hie-" ++ version
  , "Builds hie for GHC version " ++ version
  )

buildTarget :: TargetDescription
buildTarget =
  ("build", "Builds hie with all installed GHCs")

buildDataTarget :: TargetDescription
buildDataTarget =
  ("build-data", "Get the required data-files for `hie` (Hoogle DB)")

buildAllTarget :: TargetDescription
buildAllTarget =
  ( "build-all"
  , "Builds hie for all installed GHC versions and the data files"
  )

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
