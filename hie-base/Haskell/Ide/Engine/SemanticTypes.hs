{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haskell.Ide.Engine.SemanticTypes where

import           Control.Applicative
import           Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import           GHC.Generics
import           Haskell.Ide.Engine.PluginTypes

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
  , dDiff   :: !String
    {- ^ Diff of the form
    5,9c5,9
    < foo x = if odd x
    <         then
    <           x + 3
    <         else
    <           x
    ---
    > foo x = case odd x of
    >   True  ->
    >             x + 3
    >   False ->
    >             x
    -}
  } deriving (Show,Eq,Generic)

-- ---------------------------------------------------------------------

-- | A list of modules
data ModuleList = ModuleList {
    mModules :: [T.Text]
  } deriving (Show,Read,Eq,Ord,Generic)

-- ---------------------------------------------------------------------

-- | GHC AST
data AST = AST {
    astModule :: T.Text
  , astParsed      :: Value
  , astRenamed     :: Value
  , astTypechecked :: Value
  , astExports     :: Value
  } deriving (Eq,Show)


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

instance ValidResponse HieDiff where
  jsWrite d = H.fromList ["diff" .= d]
  jsRead v =  v .: "diff"

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
    <*> (v .: "diff")
  parseJSON _ = empty

-- ---------------------------------------------------------------------

instance ValidResponse ModuleList where
  jsWrite (ModuleList ms) = H.fromList ["modules" .= ms]
  jsRead v = ModuleList <$> v .: "modules"

-- ---------------------------------------------------------------------

instance ValidResponse AST where
  jsWrite (AST m p r t e) = H.fromList ["module" .= m, "parsed" .= p
    , "renamed" .= r, "typechecked" .= t, "exports" .= e ]
  jsRead v = AST
    <$> v .: "module"
    <*> v .: "parsed"
    <*> v .: "renamed"
    <*> v .: "typechecked"
    <*> v .: "exports"
