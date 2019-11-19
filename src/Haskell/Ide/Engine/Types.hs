{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
module Haskell.Ide.Engine.Types where

import           Haskell.Ide.Engine.MonadTypes
import qualified Language.Haskell.LSP.Types as J

-- ---------------------------------------------------------------------

-- | A callback from a request.
type RequestCallback m a = a -> m ()

-- | Used to track a request through the system, for logging
type TrackingNumber = Int

-- | Requests are parametric in the monad m
-- that their callback expects to be in.
pattern GReq :: TrackingNumber
             -> String
             -> Maybe Uri
             -> Maybe (Uri, Int)
             -> Maybe J.LspId
             -> RequestCallback m a1
             -> a1
             -> IdeGhcM (IdeResult a1)
             -> PluginRequest m
pattern GReq a s b c d e f g = Right (GhcRequest   a s b c d e f g)

pattern IReq :: TrackingNumber -> String -> J.LspId -> RequestCallback m a -> IdeDeferM (IdeResult a) -> Either (IdeRequest m) b
pattern IReq a s b c d   = Left  (IdeRequest a s b c d)

type PluginRequest m = Either (IdeRequest m) (GhcRequest m)

data GhcRequest m = forall a. GhcRequest
  { pinMsgNum    :: TrackingNumber -- ^ Exists to facilitate logging/tracing
  , pinDesc      :: String -- ^ Description of the request for debugging
  , pinContext   :: Maybe J.Uri
  , pinDocVer    :: Maybe (J.Uri, Int)
  , pinLspReqId  :: Maybe J.LspId
  , pinCallback  :: RequestCallback m a
  , pinDefault   :: a
  , pinReq       :: IdeGhcM (IdeResult a)
  }

data IdeRequest m = forall a. IdeRequest
  { pureMsgNum      :: TrackingNumber -- ^ Exists to facilitate logging/tracing
  , pureDesc        :: String
  , pureReqId       :: J.LspId
  , pureReqCallback :: RequestCallback m a
  , pureReq         :: IdeDeferM (IdeResult a)
  }

-- ---------------------------------------------------------------------
