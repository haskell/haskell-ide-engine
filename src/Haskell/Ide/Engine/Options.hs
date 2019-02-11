{-# LANGUAGE CPP #-}
module Haskell.Ide.Engine.Options where

#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup             hiding (option)
#endif
import           Options.Applicative.Simple

data GlobalOpts = GlobalOpts
  { optDebugOn       :: Bool
  , optLogFile       :: Maybe String
  , projectRoot      :: Maybe String
  , optGhcModVomit   :: Bool
  , optCaptureFile   :: Maybe FilePath
  , optExamplePlugin :: Bool
  } deriving (Show)

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
  <*> optional (strOption
       ( long "project-root"
      <> short 'r'
      <> metavar "PROJECTROOT"
      <> help "Root directory of project, defaults to cwd"))
  <*> switch
       ( long "vomit"
       <> help "enable vomit logging for ghc-mod")
  <*> optional (strOption
       ( long "capture"
      <> short 'c'
      <> metavar "CAPTUREFILE"
      <> help "File to capture the session to"
       ))
  <*> switch
       ( long "example"
       <> help "Enable Example2 plugin. Useful for developers only")
