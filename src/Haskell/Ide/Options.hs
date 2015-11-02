module Haskell.Ide.Options where

import Options.Applicative.Simple

data GlobalOpts = GlobalOpts
    { optRepl :: Bool
    } deriving (Show)

globalOptsParser :: Parser GlobalOpts
globalOptsParser = GlobalOpts
    <$> ( flag False True
        ( long "repl"
       <> help "Run a REPL for simple testing"
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
