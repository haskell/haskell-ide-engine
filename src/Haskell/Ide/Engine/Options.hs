{-# LANGUAGE CPP #-}
module Haskell.Ide.Engine.Options where

#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
#if __GLASGOW_HASKELL__ < 808
import           Data.Monoid                           ((<>))
#endif
import           Data.Version                          (showVersion)
import           Haskell.Ide.Engine.Version
import qualified Language.Haskell.LSP.Core             as Core
import           Options.Applicative.Simple
import qualified Paths_haskell_ide_engine              as Meta
import           System.Directory
import           System.IO
import qualified System.Log.Logger                     as L
import           Data.Foldable

data GlobalOpts = GlobalOpts
  { optDebugOn       :: Bool
  , optLogFile       :: Maybe String
  , optLsp           :: Bool
  , projectRoot      :: Maybe String
  , optBiosVerbose   :: Bool
  , optCaptureFile   :: Maybe FilePath
  , optExamplePlugin :: Bool
  , optDryRun        :: Bool
  , optFiles         :: [FilePath]
  } deriving (Show)

initApp :: String -> IO GlobalOpts
initApp namedesc = do
  hSetBuffering stderr LineBuffering
  let numericVersion :: Parser (a -> a)
      numericVersion = infoOption (showVersion Meta.version)
        (long "numeric-version" <> help "Show only version number")
      compiler :: Parser (a -> a)
      compiler = infoOption hieGhcDisplayVersion
        (long "compiler" <> help "Show only compiler and version supported")
    -- Parse the options and run
  (opts, ()) <- simpleOptions
    hieVersion
    namedesc
    ""
    (numericVersion <*> compiler <*> globalOptsParser)
    empty             
  Core.setupLogger (optLogFile opts) ["hie", "hie-bios"]
    $ if optDebugOn opts then L.DEBUG else L.INFO
  traverse_ setCurrentDirectory $ projectRoot opts
  return opts

globalOptsParser :: Parser GlobalOpts
globalOptsParser = GlobalOpts
  <$> switch
       ( long "debug"
      <> short 'd'
      <> help "Generate debug output"
       )
  <*> optional (strOption
       ( long "logfile"
      <> short 'l'
      <> metavar "LOGFILE"
      <> help "File to log to, defaults to stdout"
       ))
  <*> flag False True
       ( long "lsp"
       <> help "Start HIE as an LSP server. Otherwise it dumps debug info to stdout")
  <*> optional (strOption
       ( long "project-root"
      <> short 'r'
      <> metavar "PROJECTROOT"
      <> help "Root directory of project, defaults to cwd"))
  <*> (switch
          ( long "bios-verbose"
          <> help "enable verbose logging for hie-bios"
          )
       <|>
       switch
          ( long "vomit"
          <> help "(deprecated) enable verbose logging for hie-bios"
          )
      )
  <*> optional (strOption
       ( long "capture"
      <> short 'c'
      <> metavar "CAPTUREFILE"
      <> help "File to capture the session to"
       ))
  <*> switch
       ( long "example"
       <> help "Enable Example2 plugin. Useful for developers only")
  <*> flag False True
     (  long "dry-run"
     <> help "Perform a dry-run of loading files. Only searches for Haskell source files to load. Does nothing if run as LSP server."
     )
  <*> many
     ( argument str
       (  metavar "FILES..."
       <> help "Directories and Filepaths to load. Does nothing if run as LSP server.")
     )

