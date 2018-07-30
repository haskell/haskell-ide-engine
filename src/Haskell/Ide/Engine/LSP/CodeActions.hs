{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
module Haskell.Ide.Engine.LSP.CodeActions where

import Control.Lens
import Control.Monad.Reader
import qualified Data.Aeson as J
import Data.Maybe
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

data FallbackCodeActionParams =
  FallbackCodeActionParams
    { fallbackWorkspaceEdit :: Maybe J.WorkspaceEdit
    , fallbackCommand       :: Maybe J.Command
    }
  deriving (G.Generic, J.ToJSON, J.FromJSON)

fallbackCodeActionCommandId :: CommandId
fallbackCodeActionCommandId = CommandId "hie" "fallbackCodeAction"

fallbackCodeActionCommand :: PluginCommand
fallbackCodeActionCommand =
  PluginCommand fallbackCodeActionCommandId
                "A shim for providing 3.8 style code actions in older clients"
                -- Param is ignored by mkLSPCommand
                (CmdSync $ \(FallbackCodeActionParams _ _) -> return $ IdeResultOk ())

                

handleCodeActionReq :: TrackingNumber -> J.CodeActionRequest -> R ()
handleCodeActionReq tn req = do

  maybeRootDir <- asksLspFuncs Core.rootPath

  vfsFunc <- asksLspFuncs Core.getVirtualFileFunc
  docVersion <- fmap _version <$> liftIO (vfsFunc docUri)
  let docId = J.VersionedTextDocumentIdentifier docUri docVersion

  let getProviders :: IdeM (IdeResponse [CodeActionProvider])
      getProviders = do
        IdePlugins m <- lift getPlugins
        return $ IdeResponseOk $ map pluginCodeActionProvider $ toList m

      providersCb :: [CodeActionProvider] -> R ()
      providersCb providers =
        let reqs = map (\f -> f docId maybeRootDir range context) providers
        in collectRequests reqs (send . concat)

  makeRequest (IReq tn (req ^. J.id) providersCb getProviders)

  where
    params = req ^. J.params
    docUri = params ^. J.textDocument . J.uri
    range = params ^. J.range
    context = params ^. J.context

    wrapCodeAction :: Bool -> J.CodeAction -> IdeM (IdeResponse J.CommandOrCodeAction)

    -- Code action literal support
    wrapCodeAction True action = return $ IdeResponseOk (J.CommandOrCodeActionCodeAction action)

    -- No code action literal support
    wrapCodeAction False action = do
      let cmdParams = FallbackCodeActionParams (action ^. J.edit) (action ^. J.command)
      cmd <- mkLSPCommand fallbackCodeActionCommand (action ^. J.title) cmdParams 
      return $ IdeResponseOk (J.CommandOrCodeActionCommand cmd)

    send :: [J.CodeAction] -> R ()
    send codeActions = do
      (C.ClientCapabilities _ textDocCaps _) <- asksLspFuncs Core.clientCapabilities
      let literalSupport = isJust $ textDocCaps >>= C._codeAction >>= C._codeActionLiteralSupport
          wrapReqs = map (wrapCodeAction literalSupport) codeActions

      collectRequests wrapReqs $ \wrappedCodeActions ->
        reactorSend $ RspCodeAction $ Core.makeResponseMessage req (J.List wrappedCodeActions)

    -- | Execute multiple ide requests sequentially
    collectRequests :: [IdeM (IdeResponse a)] -- ^ The requests to make
                    -> ([a] -> R ())     -- ^ Callback with the request inputs and results
                    -> R ()
    collectRequests = go []
      where
        go acc [] callback = callback acc
        go acc (x:xs) callback =
          let reqCallback result = go (acc ++ [result]) xs callback
          in makeRequest $ IReq tn (req ^. J.id) reqCallback x

  -- TODO: make context specific commands for all sorts of things, such as refactorings          
