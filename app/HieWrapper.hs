{-# LANGUAGE CPP             #-}
-- | This module is based on the hie-wrapper.sh script in
-- https://github.com/alanz/vscode-hie-server
module Main where

#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import           Data.List
import           Data.Foldable
import           Data.Version                          (showVersion)
import           HIE.Bios
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.Options
import           Haskell.Ide.Engine.Plugin.Base
import qualified Language.Haskell.LSP.Core             as Core
import           Options.Applicative.Simple
import qualified Paths_haskell_ide_engine              as Meta
import           System.Directory
import           System.FilePath
import           System.Environment
import qualified System.Log.Logger as L
import           System.Process
import           System.Info

-- ---------------------------------------------------------------------

main :: IO ()
main = do
    let
        numericVersion :: Parser (a -> a)
        numericVersion =
            infoOption
                (showVersion Meta.version)
                (long "numeric-version" <>
                 help "Show only version number")
        compiler :: Parser (a -> a)
        compiler =
            infoOption
                hieGhcDisplayVersion
                (long "compiler" <>
                 help "Show only compiler and version supported")
    -- Parse the options and run
    (global, ()) <-
        simpleOptions
            version
            "hie-wrapper - Launch the appropriate haskell-ide-engine for a given project"
            ""
            (numericVersion <*> compiler <*> globalOptsParser)
            empty

    run global

-- ---------------------------------------------------------------------

run :: GlobalOpts -> IO ()
run opts = do
  let mLogFileName = optLogFile opts

      logLevel = if optDebugOn opts
                   then L.DEBUG
                   else L.INFO

  Core.setupLogger mLogFileName ["hie"] logLevel

  maybe (pure ()) setCurrentDirectory $ projectRoot opts


  progName <- getProgName
  logm $  "run entered for hie-wrapper(" ++ progName ++ ") " ++ version
  d <- getCurrentDirectory
  logm $ "Current directory:" ++ d
  logm $ "Operating system:" ++ os

  -- Get the cabal directory from the cradle
  cr <- findCradle (d </> "File.hs")
  let dir = cradleRootDir cr
  logm $ "Cradle directory:" ++ dir
  setCurrentDirectory dir

  ghcVersion <- getProjectGhcVersion
  logm $ "Project GHC version:" ++ ghcVersion

  let
    hieBin = "hie-" ++ ghcVersion
    backupHieBin =
      case dropWhileEnd (/='.') ghcVersion of
        [] -> "hie"
        xs -> "hie-" ++ init xs
    candidates' = [hieBin, backupHieBin, "hie"]
    candidates = map (++ exeExtension) candidates'

  logm $ "hie exe candidates :" ++ show candidates

  mexes <- traverse findExecutable candidates

  case asum mexes of
    Nothing -> logm $ "cannot find any hie exe, looked for:" ++ intercalate ", " candidates
    Just e -> do
      logm $ "found hie exe at:" ++ e
      args <- getArgs
      logm $ "args:" ++ show args
      logm "launching ....\n\n\n"
      callProcess e args
      logm "done"

-- ---------------------------------------------------------------------
