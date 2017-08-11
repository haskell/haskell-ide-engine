{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.Engine.Types where

import           Haskell.Ide.Engine.MonadTypes
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J

-- ---------------------------------------------------------------------

data PluginRequest = forall a. PReq
  { pinContext   :: Maybe J.Uri
  , pinDocVer    :: Maybe (J.Uri, Int)
  , pinLspReqId  :: Maybe J.LspId
  , pinCallback  :: a -> IO ()
  , pinReq       :: IdeM a
  }
