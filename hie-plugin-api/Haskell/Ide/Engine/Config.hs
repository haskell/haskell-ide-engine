{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Config where

import           Data.Aeson
import           Data.Default
import qualified Data.Text as T
import           Language.Haskell.LSP.Types

-- ---------------------------------------------------------------------

-- | Callback from haskell-lsp core to convert the generic message to the
-- specific one for hie
getConfigFromNotification :: DidChangeConfigurationNotification -> Either T.Text Config
getConfigFromNotification (NotificationMessage _ _ (DidChangeConfigurationParams p)) =
  case fromJSON p of
    Success c -> Right c
    Error err -> Left $ T.pack err

-- ---------------------------------------------------------------------

data Config =
  Config
    { hlintOn              :: Bool
    , maxNumberOfProblems  :: Int
    , liquidOn             :: Bool
    , completionSnippetsOn :: Bool
    , formatOnImportOn     :: Bool
    } deriving (Show,Eq)

instance Default Config where
  def = Config
    { hlintOn                 = True
    , maxNumberOfProblems     = 100
    , liquidOn                = False
    , completionSnippetsOn    = True
    , formatOnImportOn        = True
    }

-- TODO: Add API for plugins to expose their own LSP config options
instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> do
    s <- v .: "languageServerHaskell"
    flip (withObject "Config.settings") s $ \o -> Config
      <$> o .:? "hlintOn"              .!= hlintOn def
      <*> o .:? "maxNumberOfProblems"  .!= maxNumberOfProblems def
      <*> o .:? "liquidOn"             .!= liquidOn def
      <*> o .:? "completionSnippetsOn" .!= completionSnippetsOn def
      <*> o .:? "formatOnImportOn"     .!= formatOnImportOn def 

-- 2017-10-09 23:22:00.710515298 [ThreadId 11] - ---> {"jsonrpc":"2.0","method":"workspace/didChangeConfiguration","params":{"settings":{"languageServerHaskell":{"maxNumberOfProblems":100,"hlintOn":true}}}}
-- 2017-10-09 23:22:00.710667381 [ThreadId 15] - reactor:got didChangeConfiguration notification:
-- NotificationMessage
--   {_jsonrpc = "2.0"
--   , _method = WorkspaceDidChangeConfiguration
--   , _params = DidChangeConfigurationParams
--                 {_settings = Object (fromList [("languageServerHaskell",Object (fromList [("hlintOn",Bool True)
--                                                                                          ,("maxNumberOfProblems",Number 100.0)]))])}}

instance ToJSON Config where
  toJSON (Config h m l c f) = object [ "languageServerHaskell" .= r ]
    where
      r = object [ "hlintOn"              .= h
                 , "maxNumberOfProblems"  .= m
                 , "liquidOn"             .= l
                 , "completionSnippetsOn" .= c
                 , "formatOnImportOn"     .= f
                 ]
