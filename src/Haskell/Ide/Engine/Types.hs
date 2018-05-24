{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Types where

import           Data.Aeson
import           Haskell.Ide.Engine.MonadTypes
import qualified Language.Haskell.LSP.Types as J

-- ---------------------------------------------------------------------

-- | A callback from a request. 
type RequestCallback m a = a -> m ()

-- | Requests are parametric in the monad m
-- that their callback expects to be in.
pattern GReq :: Maybe Uri
                -> Maybe (Uri, Int)
                -> Maybe J.LspId
                -> RequestCallback m a1
                -> IdeGhcM (IdeResult a1)
                -> PluginRequest m
pattern GReq a b c d e = Right (GhcRequest   a b c d e)

pattern IReq :: J.LspId -> (RequestCallback m a) -> IdeM (IdeResponse a) -> Either (IdeRequest m) b
pattern IReq a b c     = Left  (IdeRequest a b c)

type PluginRequest m = Either (IdeRequest m) (GhcRequest m)

data GhcRequest m = forall a. GhcRequest
  { pinContext   :: Maybe J.Uri
  , pinDocVer    :: Maybe (J.Uri, Int)
  , pinLspReqId  :: Maybe J.LspId
  , pinCallback  :: RequestCallback m a
  , pinReq       :: IdeGhcM (IdeResult a)
  }

data IdeRequest m = forall a. IdeRequest
  { pureReqId :: J.LspId
  , pureReqCallback :: RequestCallback m a
  , pureReq :: IdeM (IdeResponse a)
  }

data Config =
  Config
    { hlintOn             :: Bool
    , maxNumberOfProblems :: Int
    } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> do
    s <- v .: "languageServerHaskell"
    flip (withObject "Config.settings") s $ \o -> Config
      <$> o .:? "hlintOn" .!= True
      <*> o .: "maxNumberOfProblems"