{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haskell.Ide.Engine.PluginTypes
  (
  -- * Interface types
    IdeFailure(..)
  , IdeResponse
  , pattern IdeResponseOk
  , pattern IdeResponseError
  , pattern IdeResponseFail
  , IdeError(..)
  , IdeErrorCode(..)
  -- * LSP types
  , Uri(..)
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

import           Control.Applicative
import           Data.Aeson
import qualified Data.Text                             as T
import           GHC.Generics
import           Language.Haskell.LSP.TH.DataTypesJSON (Diagnostic (..),
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


-- ---------------------------------------------------------------------

data IdeFailure = IdeRFail IdeError
                | IdeRErr  IdeError
                deriving(Show, Eq, Generic)

instance ToJSON IdeFailure where
 toJSON (IdeRFail v) = object [ "fail" .= v ]
 toJSON (IdeRErr v)  = object [ "error" .= v ]

instance FromJSON IdeFailure where
  parseJSON = withObject "IdeFailure" $ \v -> do
   mf <- fmap IdeRFail <$> v .:? "fail"
   me <- fmap IdeRErr <$> v .:? "error"
   case mf <|> me of
     Just r  -> return r
     Nothing -> empty

-- | The IDE response, with the type of response it contains
type IdeResponse a = Either IdeFailure a
pattern IdeResponseOk :: a -> IdeResponse a
pattern IdeResponseOk a = Right a
pattern IdeResponseFail :: IdeError -> IdeResponse a
pattern IdeResponseFail a = Left (IdeRFail a)
pattern IdeResponseError :: IdeError -> IdeResponse a
pattern IdeResponseError a = Left (IdeRErr a)

-- | Error codes. Add as required
data IdeErrorCode
 = ParameterError  -- ^ Wrong parameter type
 | PluginError             -- ^ An error returned by a plugin
 | InternalError           -- ^ Code error (case not handled or deemed
                           --   impossible)
 | UnknownPlugin           -- ^ Plugin is not registered
 | UnknownCommand          -- ^ Command is not registered
 | InvalidContext          -- ^ Context invalid for command
 | OtherError              -- ^ An error for which there's no better code
 | ParseError              -- ^ Input could not be parsed
 deriving (Show,Read,Eq,Ord,Bounded,Enum,Generic)

instance ToJSON IdeErrorCode
instance FromJSON IdeErrorCode

-- | A more structured error than just a string
data IdeError = IdeError
 { ideCode    :: IdeErrorCode -- ^ The error code
 , ideMessage :: T.Text       -- ^ A human readable message
 , ideInfo    :: Value  -- ^ Additional information
 }
 deriving (Show,Read,Eq,Generic)

instance ToJSON IdeError
instance FromJSON IdeError
