{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haskell.Ide.Engine.SemanticTypes
  ( ValidResponse(..)
  , TypeInfo(..)
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
import           Control.Applicative
import           Data.Algorithm.Diff
import qualified Data.HashMap.Strict as H
import           Data.Typeable
import qualified Data.Map as Map
import qualified Data.Text as T
import           GHC.Generics
import           Haskell.Ide.Engine.PluginTypes
import           Language.Haskell.LSP.TH.DataTypesJSON (Diagnostic(..), Position(..), Range(..), DiagnosticSeverity(..), TextDocumentIdentifier(..), WorkspaceEdit(..), PublishDiagnosticsParams(..), List(..))

-- | The typeclass for valid response types
class (Typeable a) => ValidResponse a where
  jsWrite :: a -> Object -- ^ Serialize to JSON Object
  jsRead  :: Object -> Parser a -- ^ Read from JSON Object

-- ---------------------------------------------------------------------
-- ValidResponse instances

ok :: T.Text
ok = "ok"

instance ValidResponse T.Text where
  jsWrite s = H.fromList [ok .= s]
  jsRead o = o .: ok


instance (ToJSON a, FromJSON a, ValidResponse a) => ValidResponse [a] where
  jsWrite ss = H.fromList [ok .= ss]
  jsRead o = o .: ok

instance ValidResponse () where
  jsWrite _ = H.fromList [ok .= String ok]
  jsRead o = do
    r <- o .: ok
    if r == String ok
      then pure ()
      else empty

instance ValidResponse Object where
  jsWrite = id
  jsRead = pure
deriving instance Generic Value

instance ValidResponse UntaggedCommandDescriptor where
  jsWrite cmdDescriptor = case toJSON cmdDescriptor of
    Object o -> o
    _ -> error "impossible"
  jsRead v = parseJSON (Object v)

instance ValidResponse ExtendedCommandDescriptor where
  jsWrite (ExtendedCommandDescriptor cmdDescriptor pname) =
    H.fromList
      [ "name"              .= cmdName cmdDescriptor
      , "ui_description"    .= cmdUiDescription cmdDescriptor
      , "file_extensions"   .= cmdFileExtensions cmdDescriptor
      , "contexts"          .= cmdContexts cmdDescriptor
      , "additional_params" .= cmdAdditionalParams cmdDescriptor
      , "return_type"       .= cmdReturnType cmdDescriptor
      , "plugin_name"       .= pname
      , "save"              .= cmdSave cmdDescriptor ]
  jsRead v =
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

instance ValidResponse IdePlugins where
  jsWrite (IdePlugins m) = H.fromList ["plugins" .= H.fromList
                ( map (uncurry (.=))
                $ Map.assocs m :: [Pair])]
  jsRead v = do
    ps <- v .: "plugins"
    fmap (IdePlugins . Map.fromList) $ mapM (\(k,vp) -> do
            p<-parseJSON vp
            return (k,p)) $ H.toList ps

-- ---------------------------------------------------------------------
-- JSON instances

instance (ValidResponse a) => ToJSON (IdeResponse a) where
 toJSON (IdeResponseOk v) = Object (jsWrite v)
 toJSON (IdeResponseFail v) = object [ "fail" .= v ]
 toJSON (IdeResponseError v) = object [ "error" .= v ]

instance (ValidResponse a) => FromJSON (IdeResponse a) where
 parseJSON = withObject "IdeResponse" $ \v -> do
   mf <- fmap IdeResponseFail <$> v .:? "fail"
   me <- fmap IdeResponseError <$> v .:? "error"
   let mo = IdeResponseOk <$> parseMaybe jsRead v
   case (mf <|> me <|> mo) of
     Just r -> return r
     Nothing -> empty

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

-- ---------------------------------------------------------------------

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

instance ValidResponse WorkspaceEdit where
  jsWrite d = case toJSON d of
    (Object o) -> o
    _ -> error "impossible"
  jsRead v = parseJSON (Object v)

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

instance ValidResponse PublishDiagnosticsParams where
  jsWrite (PublishDiagnosticsParams fn ds) = H.fromList
              [ "uri"         .= fn
              , "diagnostics" .= ds
              ]
  jsRead v = PublishDiagnosticsParams
    <$> v .: "uri"
    <*> v .: "diagnostics"

-- ---------------------------------------------------------------------

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
