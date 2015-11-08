module Haskell.Ide.Engine.Options where

import Network.Wai.Handler.Warp
import Options.Applicative.Simple

data GlobalOpts = GlobalOpts
    { optConsole :: Bool
    , optDebugOn :: Bool
    , optLogFile :: Maybe String
    , optPort :: Port
    , optHttp :: Bool
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
    <*> option
         auto
         ( long "port"
         <> short 'p'
         <> help "Port to use for webinterface"
         <> value 8001)
    <*> flag False True
         ( long "http"
         <> help "Enable the webinterface")
