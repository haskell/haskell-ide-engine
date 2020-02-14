{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes   #-}
module Main where

import qualified Control.Exception                     as E
import           Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml
import           HIE.Bios.Types
import           Haskell.Ide.Engine.Cradle (findLocalCradle, cradleDisplay
                                           , getProjectGhcLibDir, CabalHelper)
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Options
import           Haskell.Ide.Engine.Scheduler
import           Haskell.Ide.Engine.Server
import           Haskell.Ide.Engine.Version
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Info

-- ---------------------------------------------------------------------

import           RunTest

-- ---------------------------------------------------------------------
-- plugins

import           Haskell.Ide.Engine.Plugin.ApplyRefact
import           Haskell.Ide.Engine.Plugin.Brittany
import           Haskell.Ide.Engine.Plugin.Example2
import           Haskell.Ide.Engine.Plugin.Floskell
import           Haskell.Ide.Engine.Plugin.Generic
import           Haskell.Ide.Engine.Plugin.GhcMod
-- import           Haskell.Ide.Engine.Plugin.HaRe
import           Haskell.Ide.Engine.Plugin.Haddock
import           Haskell.Ide.Engine.Plugin.HfaAlign
import           Haskell.Ide.Engine.Plugin.HsImport
import           Haskell.Ide.Engine.Plugin.Liquid
import           Haskell.Ide.Engine.Plugin.Ormolu
import           Haskell.Ide.Engine.Plugin.Package
import           Haskell.Ide.Engine.Plugin.Pragmas

-- ---------------------------------------------------------------------

-- | This will be read from a configuration, eventually
plugins :: Bool -> IdePlugins
plugins includeExamples = pluginDescToIdePlugins allPlugins
  where
    allPlugins = if includeExamples
                   then basePlugins ++ examplePlugins
                   else basePlugins
    basePlugins =
      [ applyRefactDescriptor "applyrefact"
      , brittanyDescriptor    "brittany"
      , haddockDescriptor     "haddock"
      -- , hareDescriptor        "hare"
      , hsimportDescriptor    "hsimport"
      , liquidDescriptor      "liquid"
      , packageDescriptor     "package"
      , pragmasDescriptor     "pragmas"
      , floskellDescriptor    "floskell"
      , genericDescriptor     "generic"
      , ghcmodDescriptor      "ghcmod"
      , ormoluDescriptor      "ormolu"
      ]
    examplePlugins =
      [example2Descriptor "eg2"
      ,hfaAlignDescriptor "hfaa"
      ]

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  origDir <- getCurrentDirectory
  opts <- initApp
    "haskell-ide-engine - Provide a common engine to power any Haskell IDE"
  progName <- getProgName
  args <- getArgs

  let plugins' = plugins (optExamplePlugin opts)

  case optMode opts of
    LspMode -> do
      -- Start up in LSP mode
      logm $ "Run entered for HIE(" ++ progName ++ ") " ++ hieVersion
      logm $ "Operating as a LSP server on stdio"
      logm $ "Current directory:" ++ origDir
      logm $ "Operating system:" ++ os
      logm $ "args:" ++ show args

      let initOpts = defaultCradleOpts { cradleOptsVerbosity = verbosity }
          verbosity = if optBiosVerbose opts then Verbose else Silent


      when (optBiosVerbose opts) $
        logm "Enabling verbose mode for hie-bios. This option currently doesn't do anything."

      when (optExamplePlugin opts) $
        logm "Enabling Example2 plugin, will insert constant diagnostics etc."

      -- launch the dispatcher.
      scheduler <- newScheduler plugins' initOpts
      server scheduler origDir plugins' (optCaptureFile opts)
    ProjectLoadingMode projectLoadingOpts -> do
      -- Provide debug info
      cliOut $  "Running HIE(" ++ progName ++ ")"
      cliOut $  "  " ++ hieVersion
      cliOut $ "To run as a LSP server on stdio, provide the '--lsp' argument"
      cliOut $ "Current directory:" ++ origDir
      -- args <- getArgs
      cliOut $ "\nargs:" ++ show args

      cliOut $ "\nLooking for project config cradle...\n"

      ecradle <- getCradleInfo origDir
      case ecradle of
        Left e       -> cliOut $ "Could not get cradle:" ++ show e
        Right cradle -> do
          projGhc <- getProjectGhcVersion cradle
          mlibdir <- getProjectGhcLibDir cradle
          cliOut "\n\n###################################################\n"
          cliOut $ "Cradle: " ++ cradleDisplay cradle
          cliOut $ "Project Ghc version: " ++ projGhc
          cliOut $ "Libdir: " ++ show mlibdir
          cliOut "Searching for Haskell source files..."
          targets <- case optFiles projectLoadingOpts of
            [] -> findAllSourceFiles origDir
            xs -> concat <$> mapM findAllSourceFiles xs

          cliOut $ "Found " ++ show (length targets) ++ " Haskell source files.\n"
          cliOut "###################################################"
          cliOut "\nFound the following files:\n"
          mapM_ cliOut targets
          cliOut ""

          unless (optDryRun projectLoadingOpts) $ do
            cliOut "\nLoad them all now. This may take a very long time.\n"
            loadDiagnostics <- runServer mlibdir plugins' targets

            cliOut ""
            cliOut "###################################################"
            cliOut "###################################################"
            cliOut "\nDumping diagnostics:\n\n"
            mapM_ (cliOut' . uncurry prettyPrintDiags) loadDiagnostics
            cliOut "\n\nNote: loading of 'Setup.hs' is not supported."


-- ---------------------------------------------------------------------

getCradleInfo :: FilePath -> IO (Either Yaml.ParseException (Cradle CabalHelper))
getCradleInfo currentDir = do
        let dummyCradleFile = currentDir </> "File.hs"
        cradleRes <- E.try (findLocalCradle dummyCradleFile)
        return cradleRes

-- ---------------------------------------------------------------------

cliOut :: String -> IO ()
cliOut = putStrLn

cliOut' :: T.Text -> IO ()
cliOut' = T.putStrLn

-- ---------------------------------------------------------------------
