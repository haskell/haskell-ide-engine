{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haskell.Ide.Engine.SemanticTypes
  ( TypeInfo(..)
  , TypeResult(..)
  , WorkspaceEdit(..)
  , ModuleList(..)
  , AST(..)
  , Diagnostic(..)
  , Position(..)
  , Range(..)
  , DiagnosticSeverity(..)
  , TextDocumentIdentifier(..)
  , PublishDiagnosticsParams(..)
  , List(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Algorithm.Diff
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import qualified Data.Text as T
import           GHC.Generics
import           Haskell.Ide.Engine.PluginTypes
import           Language.Haskell.LSP.TH.DataTypesJSON (Diagnostic(..), Position(..), Range(..), DiagnosticSeverity(..), TextDocumentIdentifier(..), WorkspaceEdit(..), PublishDiagnosticsParams(..), List(..))


instance ToJSON ExtendedCommandDescriptor where
  toJSON (ExtendedCommandDescriptor cmdDescriptor pname) =
    object
      [ "name"              .= cmdName cmdDescriptor
      , "ui_description"    .= cmdUiDescription cmdDescriptor
      , "file_extensions"   .= cmdFileExtensions cmdDescriptor
      , "contexts"          .= cmdContexts cmdDescriptor
      , "additional_params" .= cmdAdditionalParams cmdDescriptor
      , "return_type"       .= cmdReturnType cmdDescriptor
      , "plugin_name"       .= pname
      , "save"              .= cmdSave cmdDescriptor ]
instance FromJSON ExtendedCommandDescriptor where
  parseJSON = withObject "ExtenedCommandDescriptor" $ \v ->
    ExtendedCommandDescriptor
    <$> (CommandDesc
      <$> v .: "name"
      <*> v .: "ui_description"
      <*> v .: "file_extensions"
      <*> v .: "contexts"
      <*> v .: "additional_params"
      <*> v .: "return_type"
      <*> v .: "save")

    <*> v.: "plugin_name"

instance ToJSON IdePlugins where
  toJSON (IdePlugins m) = object
                ["plugins" .= H.fromList
                ( map (uncurry (.=))
                $ Map.assocs m :: [Pair])]

instance FromJSON IdePlugins where
  parseJSON = withObject "IdePlugins" $ \v -> do
    ps <- v .: "plugins"
    fmap (IdePlugins . Map.fromList) $ mapM (\(k,vp) -> do
            p<-parseJSON vp
            return (k,p)) $ H.toList ps

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

deriving instance Generic (Diff [String])

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

-- JSON instances

instance ToJSON TypeInfo where
  toJSON x = Object (jsWrite x)
    where jsWrite (TypeInfo t) = H.fromList ["type_info" .= t]

instance FromJSON TypeInfo where
  parseJSON (Object o) = jsRead o
    where jsRead v = TypeInfo <$> v .: "type_info"
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


instance ToJSON ModuleList where
  toJSON (ModuleList ms) = object ["modules" .= ms]

instance FromJSON ModuleList where
  parseJSON = withObject "ModuleList" $ \v -> ModuleList <$> v .: "modules"

-- ---------------------------------------------------------------------

instance ToJSON AST where
  toJSON (AST m p r t e) = object ["module" .= m, "parsed" .= p
    , "renamed" .= r, "typechecked" .= t, "exports" .= e ]
instance FromJSON AST where
  parseJSON = withObject "Ast" $ \v -> AST
    <$> v .: "module"
    <*> v .: "parsed"
    <*> v .: "renamed"
    <*> v .: "typechecked"
    <*> v .: "exports"

-- ---------------------------------------------------------------------
