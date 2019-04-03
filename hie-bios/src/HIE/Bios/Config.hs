{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module HIE.Bios.Config where

import Dhall
import qualified Data.Text.IO as T
import qualified Data.Text as T


data CradleConfig = Cabal
                  | Stack
                  | Bazel
                  | Obelisk
                  | Bios
                  | Default
                  deriving (Generic, Show)

instance Interpret CradleConfig

data Config = Config { cradle :: T.Text }
    deriving (Generic, Show)

instance Interpret Config

readConfig :: FilePath -> IO Config
readConfig fp = T.readFile fp >>= detailed . input auto

stringToCC :: T.Text -> CradleConfig
stringToCC t = case t of
                 "cabal" -> Cabal
                 "stack" -> Stack
                 "rules_haskell" -> Bazel
                 "obelisk" -> Obelisk
                 "bios"    -> Bios
                 "default" -> Default
                 _ -> Default
