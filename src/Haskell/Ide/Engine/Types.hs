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
                -> ((IdeResult a1) -> IO ())
                -> IdeGhcM (IdeResponse a1)
                -> PluginRequest
pattern GReq a b c d e = Right (GhcRequest   a b c d e)

pattern IReq :: J.LspId -> (a -> IO ()) -> IdeM a -> Either IdeRequest b
pattern IReq a b c     = Left  (IdeRequest a b c)

type PluginRequest = Either IdeRequest GhcRequest

data GhcRequest = forall a. GhcRequest
  { pinContext   :: Maybe J.Uri
  , pinDocVer    :: Maybe (J.Uri, Int)
  , pinLspReqId  :: Maybe J.LspId
  , pinCallback  :: IdeResult a -> IO ()
  , pinReq       :: IdeGhcM (IdeResponse a)
  }

data IdeRequest = forall a. IdeRequest
  { pureReqId :: J.LspId
  , pureReqCallback :: a -> IO ()
  , pureReq :: IdeM a
  }
