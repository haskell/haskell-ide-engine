{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Haskell.Ide.Engine.LSP.CodeActions where

import Control.Lens
import Control.Monad.Reader
import qualified Data.Aeson as J
import qualified Data.Bimap as BM
import qualified Data.Text as T
import Data.Maybe
import Data.Foldable
import Haskell.Ide.Engine.LSP.Reactor
import Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Core as Core
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Capabilities as C
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Messages
import Haskell.Ide.Engine.IdeFunctions
import Haskell.Ide.Engine.PluginsIdeMonads
import Control.Applicative
import Control.Monad
import Data.Traversable

handleCodeActionReq :: TrackingNumber -> BM.Bimap T.Text T.Text -> J.CodeActionRequest -> R ()
handleCodeActionReq tn commandMap req = do

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

  makeRequest (IReq tn (req ^. J.id) providersCb getProviders)
  
  where
    params = req ^. J.params
    docUri = params ^. J.textDocument . J.uri
    range = params ^. J.range
    context = params ^. J.context

    wrapCodeAction :: J.CodeAction -> R J.CommandOrCodeAction
    wrapCodeAction action@J.CodeAction{..} = asksLspFuncs Core.clientCapabilities <&> \(C.ClientCapabilities _ textDocCaps _) ->
      case (textDocCaps >>= C._codeAction >>= C._codeActionLiteralSupport, _edit, _command) of
        (Just _,_,_) -> J.CommandOrCodeActionCodeAction action
        (_,Just e,_) -> J.CommandOrCodeActionCommand
          $ J.Command _title (commandMap BM.! "hie:applyWorkspaceEdit") $ Just $ J.toJSON [J.ApplyWorkspaceEditParams e]
        (_,_,Just c) -> J.CommandOrCodeActionCommand $ J.command %~ (commandMap BM.!) $ c
        _ -> error "A code action needs either a workspace edit or a command"

    send :: [J.CodeAction] -> R ()
    send = reactorSend . RspCodeAction . Core.makeResponseMessage req . J.List <=< traverse wrapCodeAction

    -- | Execute multiple ide requests sequentially
    collectRequests :: [IdeM (IdeResponse a)] -- ^ The requests to make
                    -> ([a] -> R ())     -- ^ Callback with the request inputs and results
                    -> R ()
    collectRequests = foldr go ($[]) where
      go x goxs callback = makeRequest $ IReq tn (req ^. J.id) (goxs . (callback .) . (:)) x

  -- TODO: make context specific commands for all sorts of things, such as refactorings          

