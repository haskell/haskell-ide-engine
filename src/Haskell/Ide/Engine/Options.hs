module Haskell.Ide.Engine.Options where

import Options.Applicative.Simple

data GlobalOpts = GlobalOpts
    { optRepl :: Bool
    , optDebugOn :: Bool
    , optLogFile :: Maybe String
    } deriving (Show)

globalOptsParser :: Parser GlobalOpts
globalOptsParser = GlobalOpts
    <$> flag False True
         ( long "repl"
        <> help "Run a REPL for simple testing"
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
{-
data GlobalOpts = GlobalOpts
    { optPluginModules :: [String]
    , optPluginPackages :: [String]
    } deriving (Show)

globalOptsParser :: Parser GlobalOpts
globalOptsParser = GlobalOpts
    <$> many (strOption
        ( long "plugin"
       <> metavar "MODULE"
       <> help "Specify a plugin to load"
        ))
    <*>  many (strOption
        ( long "plugin-pkg"
       <> metavar "PACKAGE"
       <> help "Specify a package to use for loading plugins"
        ))
-}
