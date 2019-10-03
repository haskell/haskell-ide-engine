{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes   #-}
module Main where

import           Control.Monad
import           Data.Monoid                           ((<>))
import           Data.Version                          (showVersion)
import           Haskell.Ide.Engine.Cradle (findLocalCradle)
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Options
import           Haskell.Ide.Engine.Scheduler
import           Haskell.Ide.Engine.Transport.LspStdio
import           Haskell.Ide.Engine.Transport.JsonStdio
import qualified Language.Haskell.LSP.Core             as Core
import           Options.Applicative.Simple
import qualified Paths_haskell_ide_engine              as Meta
import           System.Directory
import           System.FilePath ((</>))
import           System.Environment
import qualified System.Log.Logger                     as L
import           HIE.Bios.Types
import           System.IO

-- ---------------------------------------------------------------------
-- plugins

import           Haskell.Ide.Engine.Plugin.ApplyRefact
import           Haskell.Ide.Engine.Plugin.Base
import           Haskell.Ide.Engine.Plugin.Brittany
import           Haskell.Ide.Engine.Plugin.Example2
import           Haskell.Ide.Engine.Plugin.Bios
import           Haskell.Ide.Engine.Plugin.HaRe
import           Haskell.Ide.Engine.Plugin.Haddock
import           Haskell.Ide.Engine.Plugin.HfaAlign
import           Haskell.Ide.Engine.Plugin.Hoogle
import           Haskell.Ide.Engine.Plugin.HsImport
import           Haskell.Ide.Engine.Plugin.Liquid
import           Haskell.Ide.Engine.Plugin.Package
import           Haskell.Ide.Engine.Plugin.Pragmas
import           Haskell.Ide.Engine.Plugin.Floskell
import           Haskell.Ide.Engine.Plugin.Generic

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
      , baseDescriptor        "base"
      , brittanyDescriptor    "brittany"
      , haddockDescriptor     "haddock"
      , hareDescriptor        "hare"
      , hoogleDescriptor      "hoogle"
      , hsimportDescriptor    "hsimport"
      , liquidDescriptor      "liquid"
      , packageDescriptor     "package"
      , pragmasDescriptor     "pragmas"
      , floskellDescriptor    "floskell"
      , biosDescriptor        "bios"
      , genericDescriptor     "generic"
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
            version
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

  d <- getCurrentDirectory
  -- Get the cabal directory from the cradle
  cradle <- findLocalCradle (d </> "File.hs")

  projGhcVersion <- getProjectGhcVersion cradle
  when (projGhcVersion /= hieGhcVersion) $
    warningm $ "Mismatching GHC versions: Project is " ++ projGhcVersion
            ++ ", HIE is " ++ hieGhcVersion

  origDir <- getCurrentDirectory

  maybe (pure ()) setCurrentDirectory $ projectRoot opts

  progName <- getProgName
  logm $  "Run entered for HIE(" ++ progName ++ ") " ++ version
  logm $ "Current directory:" ++ d
  args <- getArgs
  logm $ "args:" ++ show args

  let initOpts = defaultCradleOpts { cradleOptsVerbosity = verbosity }
      verbosity = if optBiosVerbose opts then Verbose else Silent
      -- biosLogLevel = if optBiosVerbose opts then L.DEBUG else L.INFO

  -- Core.setupLogger mLogFileName ["hie-bios"] biosLogLevel

  when (optBiosVerbose opts) $
    logm "Enabling verbose mode for hie-bios. This option currently doesn't do anything."

  when (optExamplePlugin opts) $
    logm "Enabling Example2 plugin, will insert constant diagnostics etc."

  let plugins' = plugins (optExamplePlugin opts)

  -- launch the dispatcher.
  if optJson opts then do
    scheduler <- newScheduler plugins' initOpts
    jsonStdioTransport scheduler
  else do
    scheduler <- newScheduler plugins' initOpts
    lspStdioTransport scheduler origDir plugins' (optCaptureFile opts)
