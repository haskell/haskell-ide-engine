module Options where

import Options.Applicative

data Config = Config { prefix :: FilePath }

config :: Parser Config
config =
  Config <$>
  strOption (long "prefix" <> metavar "PREFIX" <>
             help "directory where the docs are written to (defaults to current directory)")
