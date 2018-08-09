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
        return $ IdeResponseOk $ mapMaybe pluginCodeActionProvider $ toList m

      providersCb :: [CodeActionProvider] -> R ()
      providersCb providers =
        let reqs = map (\f -> f docId maybeRootDir range context) providers
        in makeRequests reqs tn (req ^. J.id) (send . concat)

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
          action = action' & J.command . _Just . J.command %~ prefix
      supported <- asksLspFuncs $ has
        $ to Core.clientCapabilities . to C._textDocument . _Just . to C._codeAction . _Just . to C._codeActionLiteralSupport . _Just
        . maybe united (\k -> to C._codeActionKind . to C._valueSet . traverse . only k) (action ^. J.kind)

      return $ if supported
        then J.CACodeAction action
        else 
          let cmdName = prefix "hie:fallbackCodeAction"
              cmdParams = J.List [J.toJSON (FallbackCodeActionParams (action ^. J.edit) (action ^. J.command))]
            in J.CACommand $ J.Command (action ^. J.title) cmdName (Just cmdParams)

    send :: [J.CodeAction] -> R ()
    send = reactorSend . RspCodeAction . Core.makeResponseMessage req . J.List <=< mapM wrapCodeAction

  -- TODO: make context specific commands for all sorts of things, such as refactorings          
