{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
module Haskell.Ide.Engine.Types where

import           Haskell.Ide.Engine.MonadTypes
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J

-- ---------------------------------------------------------------------

pattern IReq a b c d e = Right (IdeRequest a b c d e)
pattern AReq a b c = Left (AsyncRequest a b c)

type PluginRequest = Either AsyncRequest IdeRequest

data IdeRequest = forall a. IdeRequest
  { pinContext   :: Maybe J.Uri
  , pinDocVer    :: Maybe (J.Uri, Int)
  , pinLspReqId  :: Maybe J.LspId
  , pinCallback  :: a -> IO ()
  , pinReq       :: IdeM a
  }

data AsyncRequest = forall a. AsyncRequest
  { pureReqId :: J.LspId
  , pureReqCallback :: a -> IO ()
  , pureReq :: AsyncM a
  }
