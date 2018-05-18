{-# LANGUAGE CPP #-}
module Haskell.Ide.Engine.Options where

#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup             hiding (option)
#endif
import           Options.Applicative.Simple

data GlobalOpts = GlobalOpts
  { optDebugOn      :: Bool
  , optLogFile      :: Maybe String
  , optLsp          :: Bool
  , projectRoot     :: Maybe String
  , optGhcModVomit  :: Bool
  , optEkg          :: Bool
  , optEkgPort      :: Int
  , optRecordClient :: Maybe FilePath
  , optRecordServer :: Maybe FilePath
  } deriving (Show)

globalOptsParser :: Parser GlobalOpts
globalOptsParser = GlobalOpts
  <$> switch
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
       <> help "Enable vomit logging for ghc-mod")
  <*> flag False True
       ( long "ekg"
       <> help "Enable ekg collection and display on http://localhost:8000")
  <*> (option auto
       ( long "port"
      <> short 'p'
      <> metavar "PORT"
      <> help "TCP port to use for EKG server. Only used if --ekg is set. Default 8000"
      <> value 8000))
  <*> (optional $ strOption
       ( long "recordClient"
      <> short 'c'
      <> metavar "RECORDCLIENTFILE"
      <> help "File to record the input from the client to, if specified."))
  <*> (optional $ strOption
       ( long "recordServer"
      <> short 's'
      <> metavar "RECORDSERVERFILE"
      <> help "File to record the output from HIE to, if specified."))
