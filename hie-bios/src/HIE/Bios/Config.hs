{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module HIE.Bios.Config where

import Dhall
import qualified Data.Text.IO as T


data CradleConfig = Cabal
                  | Stack
                  | Bazel
                  | Obelisk
                  | Bios
                  deriving (Generic, Show)

instance Interpret CradleConfig

data Config = Config { cradle :: CradleConfig }
    deriving (Generic, Show)

instance Interpret Config

readConfig :: FilePath -> IO Config
readConfig fp = T.readFile fp >>= input auto
