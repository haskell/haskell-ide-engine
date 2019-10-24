{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes   #-}
module Main where

import qualified Control.Exception                     as E
import           Control.Monad
import           Data.Monoid                           ((<>))
import           Data.Version                          (showVersion)
import qualified Data.Yaml as Yaml
import           HIE.Bios.Types
import           Haskell.Ide.Engine.Cradle (findLocalCradle, cradleDisplay)
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Options
import           Haskell.Ide.Engine.Scheduler
import           Haskell.Ide.Engine.Server
import           Haskell.Ide.Engine.Version
import qualified Language.Haskell.LSP.Core             as Core
import           Options.Applicative.Simple
import qualified Paths_haskell_ide_engine              as Meta
import           System.Directory
import           System.Environment
import           System.FilePath ((</>))
import           System.Info
import           System.IO
import qualified System.Log.Logger                     as L

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
            hieVersion
            "haskell-ide-engine - Provide a common engine to power any Haskell IDE"
            ""
            (numericVersion <*> compiler <*> globalOptsParser)
            empty

    run global

-- ---------------------------------------------------------------------

run :: GlobalOpts -> IO ()
run opts = do
  hSetBuffering stderr LineBuffering
  let mLogFileName = optLogFile opts

      logLevel = if optDebugOn opts
                   then L.DEBUG
                   else L.INFO

  Core.setupLogger mLogFileName ["hie", "hie-bios"] logLevel

  origDir <- getCurrentDirectory

  maybe (pure ()) setCurrentDirectory $ projectRoot opts

  progName <- getProgName
  args <- getArgs

  if optLsp opts
    then do
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

      let plugins' = plugins (optExamplePlugin opts)

      -- launch the dispatcher.
      scheduler <- newScheduler plugins' initOpts
      server scheduler origDir plugins' (optCaptureFile opts)
    else do
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
        Right cradle -> cliOut $ "Cradle:" ++ cradleDisplay cradle

-- ---------------------------------------------------------------------

getCradleInfo :: FilePath -> IO (Either Yaml.ParseException Cradle)
getCradleInfo currentDir = do
        let dummyCradleFile = currentDir </> "File.hs"
        cradleRes <- E.try (findLocalCradle dummyCradleFile)
        return cradleRes

-- ---------------------------------------------------------------------

cliOut :: String -> IO ()
cliOut = putStrLn

-- ---------------------------------------------------------------------
