{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
module Haskell.Ide.Engine.LSP.CodeActions where

import Control.Lens
import Control.Monad.Reader
import qualified Data.Aeson as J
import qualified Data.Text as T
import Data.Foldable
import qualified GHC.Generics as G
import Haskell.Ide.Engine.LSP.Reactor
import Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Core as Core
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Capabilities as C
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Messages
import Haskell.Ide.Engine.IdeFunctions
import Haskell.Ide.Engine.PluginsIdeMonads
import Control.Monad.Trans.Cont

data FallbackCodeActionParams =
  FallbackCodeActionParams
    { fallbackWorkspaceEdit :: Maybe J.WorkspaceEdit
    , fallbackCommand       :: Maybe J.Command
    }
  deriving (G.Generic, J.ToJSON, J.FromJSON)

handleCodeActionReq :: TrackingNumber -> J.CodeActionRequest -> R ()
handleCodeActionReq tn req = do

  maybeRootDir <- asksLspFuncs Core.rootPath

  vfsFunc <- asksLspFuncs Core.getVirtualFileFunc
  docVersion <- fmap _version <$> liftIO (vfsFunc docUri)
  let docId = J.VersionedTextDocumentIdentifier docUri docVersion

  let getProviders :: IdeM (IdeResponse [CodeActionProvider])
      getProviders = do
        IdePlugins m <- lift getPlugins
        return $ IdeResponseOk $ map snd $ toList m

      providersCb :: [CodeActionProvider] -> R ()
      providersCb providers =
        let reqs = map (\f -> f docId maybeRootDir range context) providers
        in collectRequests reqs (send . concat)

  collectRequest getProviders providersCb

  where
    params = req ^. J.params
    docUri = params ^. J.textDocument . J.uri
    range = params ^. J.range
    context = params ^. J.context

    wrapCodeAction :: J.CodeAction -> R J.CommandOrCodeAction
    wrapCodeAction action' = do
      prefix <- asks commandPrefixer :: R (T.Text -> T.Text)
 
      let action :: J.CodeAction
          action = (J.command . _Just . J.command %~ prefix) action'
      supported <- asksLspFuncs $ has
        $ to Core.clientCapabilities . to C._textDocument . _Just . to C._codeAction . _Just . to C._codeActionLiteralSupport . _Just
        . maybe united (\k -> to C._codeActionKind . to C._valueSet . traverse . only k) (action ^. J.kind)

      return $ if supported
        then J.CommandOrCodeActionCodeAction action
        else
          let cmd = J.Command (action ^. J.title) cmdName (Just cmdParams)
              cmdName = prefix "hie:fallbackCodeAction"
              cmdParams = J.List [J.toJSON (FallbackCodeActionParams (action ^. J.edit) (action ^. J.command))]
            in J.CommandOrCodeActionCommand cmd

    send :: [J.CodeAction] -> R ()
    send = reactorSend . RspCodeAction . Core.makeResponseMessage req . J.List <=< mapM wrapCodeAction

    -- | Execute multiple ide requests sequentially
    collectRequests :: [IdeM (IdeResponse a)] -- ^ The requests to make
                    -> ([a] -> R ())     -- ^ Callback with the request inputs and results
                    -> R ()
    collectRequests = alaf ContT traverse collectRequest
    
    collectRequest :: IdeM (IdeResponse a) -> (a -> R ()) -> R ()
    collectRequest x c = makeRequest $ IReq tn (req ^. J.id) c x
        
  -- TODO: make context specific commands for all sorts of things, such as refactorings          
