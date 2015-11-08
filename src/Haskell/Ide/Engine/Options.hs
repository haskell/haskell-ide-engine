module Haskell.Ide.Engine.Options where

import Options.Applicative.Simple

data GlobalOpts = GlobalOpts
    { optConsole :: Bool
    , optDebugOn :: Bool
    , optLogFile :: Maybe String
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
