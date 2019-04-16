{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module HIE.Bios.Config where

import Dhall
import qualified Data.Text.IO as T
import qualified Data.Text as T
-- import Lens.Family ( set )
-- import qualified Dhall.Context as C


data CradleConfig = Cabal { component :: Maybe String }
                  | Stack
                  | Bazel
                  | Obelisk
                  | Bios { prog :: FilePath }
                  | Default
                  deriving (Generic, Show)

instance Interpret CradleConfig

data Config = Config { cradle :: CradleConfig }
    deriving (Generic, Show)

instance Interpret Config

wrapper :: T.Text -> T.Text
wrapper t =
  "let CradleConfig : Type = < Cabal : { component : Optional Text } | Stack : {} | Bazel : {} | Obelisk : {} | Bios : { prog : Text} | Default : {} > in\n" <> t

readConfig :: FilePath -> IO Config
readConfig fp = T.readFile fp >>= input auto . wrapper
  where
    -- ip = (set startingContext sc defaultInputSettings)
    -- sc = C.insert "CradleConfig" (expected (auto @CradleConfig)) C.empty

{-
stringToCC :: T.Text -> CradleConfig
stringToCC t = case t of
                 "cabal" -> Cabal
                 "stack" -> Stack
                 "rules_haskell" -> Bazel
                 "obelisk" -> Obelisk
                 "bios"    -> Bios
                 "default" -> Default
                 _ -> Default
                 -}
