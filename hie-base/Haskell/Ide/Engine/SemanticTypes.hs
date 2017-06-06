{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haskell.Ide.Engine.SemanticTypes
  ( TypeInfo(..)
  , TypeResult(..)
  , RefactorResult(..)
  , WorkspaceEdit(..)
  , ModuleList(..)
  , AST(..)
  , FileDiagnostics(..)
  , Diagnostic(..)
  , Position(..)
  , Range(..)
  , DiagnosticSeverity(..)
  , TextDocumentIdentifier(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Algorithm.Diff
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import           GHC.Generics
import           Haskell.Ide.Engine.PluginTypes
import           Language.Haskell.LSP.TH.DataTypesJSON (Diagnostic(..), Position(..), Range(..), DiagnosticSeverity(..), TextDocumentIdentifier(..), WorkspaceEdit(..))

-- ---------------------------------------------------------------------
-- Specific response type

-- | Type Information, from the most precise to the most generic
data TypeInfo = TypeInfo { results :: ![TypeResult] }
  deriving (Show,Read,Eq,Ord,Generic)

-- | One type result from ghc-mod
data TypeResult = TypeResult
    { trStart :: !Position -- ^ start line/column
    , trEnd   :: !Position -- ^ end line/column
    , trText  :: !T.Text -- ^ type text
    } deriving (Show,Read,Eq,Ord,Generic)

-- | Result of refactoring
data RefactorResult = RefactorResult
  { rrDiffs :: ![HieDiff]
  } deriving (Show,Eq,Generic)

-- ---------------------------------------------------------------------

-- | A diff between two files, typically the first one will be the one from the
-- IDE, the second from the tool
type HieDiff = WorkspaceEdit
deriving instance Generic (Diff [String])

-- ---------------------------------------------------------------------

-- | A list of modules
data ModuleList = ModuleList {
    mModules :: ![T.Text]
  } deriving (Show,Read,Eq,Ord,Generic)

-- ---------------------------------------------------------------------

-- | GHC AST
data AST = AST {
    astModule      :: !T.Text
  , astParsed      :: !Value
  , astRenamed     :: !Value
  , astTypechecked :: !Value
  , astExports     :: !Value
  } deriving (Eq,Show,Generic)

-- ---------------------------------------------------------------------

data FileDiagnostics =
  FileDiagnostics
    { fdFileName    :: Uri
    , fdDiagnostics :: [Diagnostic]
    } deriving (Show, Read, Eq,Generic)

-- ---------------------------------------------------------------------
-- JSON instances

instance ValidResponse TypeInfo where
  jsWrite (TypeInfo t) = H.fromList ["type_info" .= t]
  jsRead v = TypeInfo <$> v .: "type_info"

instance ToJSON TypeInfo where
  toJSON x = Object (jsWrite x)

instance FromJSON TypeInfo where
  parseJSON (Object o) = jsRead o
  parseJSON _          = mempty

instance ToJSON TypeResult where
  toJSON (TypeResult s e t) =
      object [ "start" .= toJSON s
             , "end"   .= toJSON e
             , "type"  .= t
             ]

instance FromJSON TypeResult where
  parseJSON = withObject "TypeResult" $ \v -> TypeResult <$> v .: "start" <*> v .: "end" <*> v .: "type"

-- ---------------------------------------------------------------------

instance ValidResponse RefactorResult where
  jsWrite (RefactorResult t) = H.fromList ["refactor" .= t]
  jsRead v = RefactorResult <$> v .: "refactor"

instance ToJSON RefactorResult where
  toJSON x = Object (jsWrite x)

instance FromJSON RefactorResult where
  parseJSON (Object o) = jsRead o
  parseJSON _          = mempty

instance ValidResponse HieDiff where
  jsWrite d = H.fromList ["diff" .= d]
  jsRead v =  v .: "diff"

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

-- ---------------------------------------------------------------------

instance ValidResponse FileDiagnostics where
  jsWrite (FileDiagnostics fn ds) = H.fromList
              [ "uri"         .= fn
              , "diagnostics" .= ds
              ]
  jsRead v = FileDiagnostics
    <$> v .: "uri"
    <*> v .: "diagnostics"

-- ---------------------------------------------------------------------

instance ValidResponse [Diagnostic] where
  jsWrite ss = H.fromList ["ok" .= ss]
  jsRead o = o .: "ok"

instance ValidResponse Diagnostic where
  jsWrite (Diagnostic r ms mc msrc m) = H.fromList $ stripNulls
               [ "range"    .= r
               , "severity" .= ms
               , "code"     .= mc
               , "source"   .= msrc
               , "message"  .= m
               ]
  jsRead v = Diagnostic
    <$> v .: "range"
    <*> v .: "severity"
    <*> v .: "code"
    <*> v .: "source"
    <*> v .: "message"

stripNulls :: [Pair] -> [Pair]
stripNulls xs = filter (\(_,v) -> v /= Null) xs

-- ---------------------------------------------------------------------

-- -- Sum type for easy JSON decoding of received params
-- data LspParam
--   = LspTextDocumentId TextDocumentIdentifier
--   | LspPosition       Position
--   | LspRange          Range
--   deriving (Read, Show, Eq)
