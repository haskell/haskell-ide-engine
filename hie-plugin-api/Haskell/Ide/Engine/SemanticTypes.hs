{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haskell.Ide.Engine.SemanticTypes where

import           Control.Applicative
import           Data.Aeson
import           Data.Algorithm.Diff
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import           GHC.Generics
import           Haskell.Ide.Engine.PluginDescriptor

-- ---------------------------------------------------------------------
-- Specific response type

-- | Type Information, from the most precise to the most generic
data TypeInfo = TypeInfo { results :: [TypeResult] }
  deriving (Show,Read,Eq,Ord,Generic)

-- | One type result from ghc-mod
data TypeResult = TypeResult
    { trStart :: (Int,Int) -- ^ start line/column
    , trEnd   :: (Int,Int) -- ^ end line/column
    , trText  :: T.Text -- ^ type text
    } deriving (Show,Read,Eq,Ord,Generic)

-- | Result of refactoring
data RefactorResult = RefactorResult
  { rrDiffs :: [HieDiff]
  } deriving (Show,Eq,Generic)

-- ---------------------------------------------------------------------

-- | A diff between two files, typically the first one will be the one from the
-- IDE, the second from the tool
data HieDiff = HieDiff
  { dFirst  :: !FilePath
  , dSecond :: !FilePath
  , dDiff   :: ![Diff (Int,T.Text)]
  } deriving (Show,Eq,Generic)

-- ---------------------------------------------------------------------

-- | A list of modules
data ModuleList = ModuleList {
    mModules :: [T.Text]
  } deriving (Show,Read,Eq,Ord,Generic)

-- ---------------------------------------------------------------------
-- JSON instances

instance ValidResponse TypeInfo where
  jsWrite (TypeInfo t) = H.fromList ["type_info" .= t]
  jsRead v = TypeInfo <$> v .: "type_info"

instance ToJSON TypeResult where
  toJSON (TypeResult s e t) =
      object [ "start" .= posToJSON s
             , "end" .= posToJSON e
             , "type" .= t
             ]

instance FromJSON TypeResult where
  parseJSON (Object v) = TypeResult
    <$> (jsonToPos =<< (v .: "start"))
    <*> (jsonToPos =<< (v .: "end"))
    <*> v .: "type"
  parseJSON _ = empty

-- ---------------------------------------------------------------------

instance ValidResponse RefactorResult where
  jsWrite (RefactorResult t) = H.fromList ["refactor" .= t]
  jsRead v = RefactorResult <$> v .: "refactor"

instance ToJSON HieDiff where
  toJSON (HieDiff f s d) =
      object [ "first" .= toJSON f
             , "second" .= toJSON s
             , "diff" .= toJSON d
             ]

instance FromJSON HieDiff where
  parseJSON (Object v) = HieDiff
    <$> (v .: "first")
    <*> (v .: "second")
    <*> (v .: "type")
  parseJSON _ = empty

instance ToJSON (Diff (Int,T.Text)) where
  toJSON (First v)    = object [ "f" .= toJSON v ]
  toJSON (Second v)   = object [ "s" .= toJSON v ]
  toJSON (Both v1 v2) = object [ "b" .= toJSON [ v1, v2 ] ]

instance FromJSON (Diff (Int,T.Text)) where
  parseJSON (Object v) = do
    mf <- fmap First <$> v .:? "f"
    ms <- fmap Second <$> v .:? "s"
    mbv <- v .:? "b"
    mb <- case mbv of
      Just [v1,v2] -> return $ Just (Both v1 v2)
      _            -> empty
    case mf <|> ms <|> mb of
      Just d -> return d
      _ -> empty
  parseJSON _ = empty

-- ---------------------------------------------------------------------

instance ValidResponse ModuleList where
  jsWrite (ModuleList ms) = H.fromList ["modules" .= ms]
  jsRead v = ModuleList <$> v .: "modules"
