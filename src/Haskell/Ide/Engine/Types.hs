{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
module Haskell.Ide.Engine.Types where

import           Haskell.Ide.Engine.MonadTypes
import qualified Language.Haskell.LSP.Types as J

-- ---------------------------------------------------------------------

pattern GReq :: Maybe Uri
                -> Maybe (Uri, Int)
                -> Maybe J.LspId
                -> (a1 -> IO ())
                -> IdeGhcM a1
                -> PluginRequest
pattern GReq a b c d e = Right (GhcRequest   a b c d e)

pattern IReq :: J.LspId -> (IdeResponse a -> IO ()) -> IdeM (IdeResponse a) -> PluginRequest
pattern IReq a b c     = Left (IdeRequest a b c)

type PluginRequest = Either IdeRequest GhcRequest

data GhcRequest = forall a. GhcRequest
  { pinContext   :: Maybe J.Uri
  , pinDocVer    :: Maybe (J.Uri, Int)
  , pinLspReqId  :: Maybe J.LspId
  , pinCallback  :: a -> IO ()
  , pinReq       :: IdeGhcM a
  }

data IdeRequest = forall a. IdeRequest
  { pureReqId :: J.LspId
  , pureReqCallback :: IdeResponse a -> IO ()
  , pureReq :: IdeM (IdeResponse a)
  }
