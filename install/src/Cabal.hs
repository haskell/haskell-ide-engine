module Cabal where

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Control.Monad
import           Data.Maybe                               ( isNothing
                                                          , isJust
                                                          )
import           Control.Monad.Extra                      ( whenMaybe )
import           System.Directory                         ( findExecutable
                                                          , copyFile
                                                          )

import           Version
import           Print
import           Env
import           Stack


execCabal :: CmdResult r => [String] -> Action r
execCabal = command [] "cabal"

execCabal_ :: [String] -> Action ()
execCabal_ = command_ [] "cabal"

cabalBuildData :: Action ()
cabalBuildData = do
  execCabal_ ["new-build", "hoogle"]
  execCabal_ ["new-exec", "hoogle", "generate"]

cabalBuildHie :: VersionNumber -> Action ()
cabalBuildHie versionNumber = do
  ghcPath <- getGhcPathOf versionNumber >>= \case
    Nothing -> do
      printInStars $ ghcVersionNotFoundFailMsg versionNumber
      error (ghcVersionNotFoundFailMsg versionNumber)
    Just p -> return p
  execCabal_
    ["new-build", "-w", ghcPath, "--write-ghc-environment-files=never", "--max-backjumps=5000"]

cabalInstallHie :: VersionNumber -> Action ()
cabalInstallHie versionNumber = do
  localBin <- getLocalBin
  execCabal_
    [ "new-install"
    , "--write-ghc-environment-files=never"
    , "--symlink-bindir=" ++ localBin
    , "exe:hie"
    , "--overwrite-policy=always"
    ]
  liftIO $ do
    copyFile (localBin </> "hie" <.> exe)
             (localBin </> "hie-" ++ versionNumber <.> exe)
    copyFile (localBin </> "hie" <.> exe)
             (localBin </> "hie-" ++ dropExtension versionNumber <.> exe)

installCabal :: Action ()
installCabal = do
  -- try to find existing `cabal` executable with appropriate version
  cabalExeOk <- do
    c <- liftIO (findExecutable "cabal")
    when (isJust c) checkCabal
    return $ isJust c

  -- install `cabal-install` if not already installed
  unless cabalExeOk $ execStackShake_ ["install", "cabal-install"]

-- | check `stack` has the required version
checkCabal :: Action ()
checkCabal = do
  cabalVersion <- getCabalVersion
  unless (checkVersion requiredCabalVersion cabalVersion) $ do
    printInStars $ cabalInstallIsOldFailMsg cabalVersion
    error $ stackExeIsOldFailMsg cabalVersion

getCabalVersion :: Action String
getCabalVersion = trimmedStdout <$> execCabal ["--numeric-version"]

-- TODO: this restriction will be gone in the next release of cabal
validateCabalNewInstallIsSupported :: Action ()
validateCabalNewInstallIsSupported = when isWindowsSystem $ do
  printInStars cabalInstallNotSuportedFailMsg
  error cabalInstallNotSuportedFailMsg

-- | Error message when a windows system tries to install HIE via `cabal new-install`
cabalInstallNotSuportedFailMsg :: String
cabalInstallNotSuportedFailMsg =
  "This system has been identified as a windows system.\n"
    ++ "Unfortunately, `cabal new-install` is currently not supported on windows.\n"
    ++ "Please use one of the stack-based targets.\n\n"
    ++ "If this system has been falsely identified, please open an issue at:\n\thttps://github.com/haskell/haskell-ide-engine\n"


-- | Error message when the `stack` binary is an older version
cabalInstallIsOldFailMsg :: String -> String
cabalInstallIsOldFailMsg cabalVersion =
  "The `cabal` executable is outdated.\n"
    ++ "found version is `"
    ++ cabalVersion
    ++ "`.\n"
    ++ "required version is `"
    ++ versionToString requiredCabalVersion
    ++ "`."


requiredCabalVersion :: RequiredVersion
requiredCabalVersion = [2, 4, 1, 0]
