module Haskell.Ide.Engine.Options where

import           Data.Semigroup             hiding (option)
import           Options.Applicative.Simple

data GlobalOpts = GlobalOpts
  { optConsole     :: Bool
  , optOneShot     :: Bool
      -- ^ Run one command and then exit. No attempt to cleanly shut down any
      -- other processes that may be running, as a result off the plugin init
      -- process.
  , optDebugOn     :: Bool
  , optLogFile     :: Maybe String
  , optHttp        :: Bool
  , optTcp         :: Bool
  , optLsp         :: Bool
  , projectRoot    :: Maybe String
  , optDumpSwagger :: Bool
  , optGhcModVomit :: Bool
  } deriving (Show)

globalOptsParser :: Parser GlobalOpts
globalOptsParser = GlobalOpts
  <$> flag False True
       ( long "console"
      <> long "repl"
      <> short 'c'
      <> help "Run a console REPL for simple testing"
       )
  <*> switch
       ( long "one-shot"
      <> help "Run a single command and then exit. Applies to stdio transport only, initially."
       )
  <*> switch
       ( long "debug"
      <> short 'd'
      <> help "Generate debug output"
       )
  <*> (optional $ strOption
       ( long "logfile"
      <> short 'l'
      <> metavar "LOGFILE"
      <> help "File to log to, defaults to stdout"
       ))
  <*> flag False True
       ( long "http"
       <> help "Enable the webinterface")
  <*> flag False True
       ( long "tcp"
       <> help "Enable the tcp transport")
  <*> flag False True
       ( long "lsp"
       <> help "Enable the Language Server Protocol transport on STDIO")
  <*> (optional $ strOption
       ( long "project-root"
      <> short 'r'
      <> metavar "PROJECTROOT"
      <> help "Root directory of project, defaults to cwd"))
  <*> switch
       ( long "swagger"
      <> short 'w'
      <> help "Generate a swagger.json file for the http API"
       )
  <*> flag False True
       ( long "vomit"
       <> help "enable vomit logging for ghc-mod")
