{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haskell.Ide.Engine.PluginTypes
  (
  -- * LSP types
    Uri(..)
  , uriToFilePath
  , filePathToUri
  , Position(..)
  , Range(..)
  , Location(..)
  , TextDocumentIdentifier(..)
  , TextDocumentPositionParams(..)
  , WorkspaceEdit(..)
  , Diagnostic(..)
  , DiagnosticSeverity(..)
  , PublishDiagnosticsParams(..)
  , List(..)
  ) where
    
import           Language.Haskell.LSP.Types (Diagnostic (..),
                                             DiagnosticSeverity (..),
                                             List (..),
                                             Location (..),
                                             Position (..),
                                             PublishDiagnosticsParams (..),
                                             Range (..),
                                             TextDocumentIdentifier (..),
                                             TextDocumentPositionParams (..),
                                             Uri (..),
                                             WorkspaceEdit (..),
                                             filePathToUri,
                                             uriToFilePath)