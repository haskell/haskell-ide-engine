{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Config where

import           Control.Applicative
import           Data.Aeson
import           Data.Default
import qualified Data.Text as T
import           Language.Haskell.LSP.Types

-- ---------------------------------------------------------------------

-- | Given a DidChangeConfigurationNotification message, this function returns the parsed
-- Config object if possible.
getConfigFromNotification :: DidChangeConfigurationNotification -> Either T.Text Config
getConfigFromNotification (NotificationMessage _ _ (DidChangeConfigurationParams p)) =
  case fromJSON p of
    Success c -> Right c
    Error err -> Left $ T.pack err

-- | Given an InitializeRequest message, this function returns the parsed
-- Config object if possible. Otherwise, it returns the default configuration
getInitialConfig :: InitializeRequest -> Either T.Text Config
getInitialConfig (RequestMessage _ _ _ InitializeParams{_initializationOptions = Nothing }) = Right def
getInitialConfig (RequestMessage _ _ _ InitializeParams{_initializationOptions = Just opts}) =
  case fromJSON opts of
    Success c -> Right c
    Error err -> Left $ T.pack err

-- ---------------------------------------------------------------------

data Config =
  Config
    { hlintOn                     :: Bool
    , diagnosticsOnChange         :: Bool
    , maxNumberOfProblems         :: Int
    , diagnosticsDebounceDuration :: Int
    , liquidOn                    :: Bool
    , completionSnippetsOn        :: Bool
    , formatOnImportOn            :: Bool
    , formattingProvider          :: T.Text
    } deriving (Show,Eq)

instance Default Config where
  def = Config
    { hlintOn                     = True
    , diagnosticsOnChange         = True
    , maxNumberOfProblems         = 100
    , diagnosticsDebounceDuration = 350000
    , liquidOn                    = False
    , completionSnippetsOn        = True
    , formatOnImportOn            = True
    , formattingProvider          = "brittany"
    }

-- TODO: Add API for plugins to expose their own LSP config options
instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> do
    c <- v .:? "haskell" <|> v.:? "languageServerHaskell"
    case c of
      Nothing -> return def
      Just s -> flip (withObject "Config.settings") s $ \o -> Config
        <$> o .:? "hlintOn"                     .!= hlintOn def
        <*> o .:? "diagnosticsOnChange"         .!= diagnosticsOnChange def
        <*> o .:? "maxNumberOfProblems"         .!= maxNumberOfProblems def
        <*> o .:? "diagnosticsDebounceDuration" .!= diagnosticsDebounceDuration def
        <*> o .:? "liquidOn"                    .!= liquidOn def
        <*> o .:? "completionSnippetsOn"        .!= completionSnippetsOn def
        <*> o .:? "formatOnImportOn"            .!= formatOnImportOn def
        <*> o .:? "formattingProvider"          .!= formattingProvider def

-- 2017-10-09 23:22:00.710515298 [ThreadId 11] - ---> {"jsonrpc":"2.0","method":"workspace/didChangeConfiguration","params":{"settings":{"haskell":{"maxNumberOfProblems":100,"hlintOn":true}}}}
-- 2017-10-09 23:22:00.710667381 [ThreadId 15] - reactor:got didChangeConfiguration notification:
-- NotificationMessage
--   {_jsonrpc = "2.0"
--   , _method = WorkspaceDidChangeConfiguration
--   , _params = DidChangeConfigurationParams
--                 {_settings = Object (fromList [("haskell",Object (fromList [("hlintOn",Bool True)
--                                                                                          ,("maxNumberOfProblems",Number 100.0)]))])}}

instance ToJSON Config where
  toJSON (Config h diag m d l c f fp) = object [ "haskell" .= r ]
    where
      r = object [ "hlintOn"                     .= h
                 , "diagnosticsOnChange"         .= diag
                 , "maxNumberOfProblems"         .= m
                 , "diagnosticsDebounceDuration" .= d
                 , "liquidOn"                    .= l
                 , "completionSnippetsOn"        .= c
                 , "formatOnImportOn"            .= f
                 , "formattingProvider"          .= fp
                 ]
