module Haskell.Ide.Engine.Options where

import           Data.Semigroup             hiding (option)
import           Options.Applicative.Simple

data GlobalOpts = GlobalOpts
  { optGetGhcVer   :: Bool
  , optDebugOn     :: Bool
  , optLogFile     :: Maybe String
  , optLsp         :: Bool
  , projectRoot    :: Maybe String
  , optGhcModVomit :: Bool
  , optEkg         :: Bool
  , optEkgPort     :: Int
  } deriving (Show)

globalOptsParser :: Parser GlobalOpts
globalOptsParser = GlobalOpts
  <$> flag False True
       ( long "get-ghc-version"
       <> help "Show only GHC version in use")
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
       ( long "lsp"
       <> help "Enable the Language Server Protocol transport on STDIO")
  <*> (optional $ strOption
       ( long "project-root"
      <> short 'r'
      <> metavar "PROJECTROOT"
      <> help "Root directory of project, defaults to cwd"))
  <*> flag False True
       ( long "vomit"
       <> help "enable vomit logging for ghc-mod")
  <*> flag False True
       ( long "ekg"
       <> help "enable ekg collection and display on http://localhost:8000")
  <*> (option auto
       ( long "port"
      <> short 'p'
      <> metavar "PORT"
      <> help "TCP port to use for EKG server. Only used if --ekg is set. Default 8000"
      <> value 8000
       ))
