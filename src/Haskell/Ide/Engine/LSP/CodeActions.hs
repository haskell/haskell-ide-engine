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

    wrapCodeAction :: J.CodeAction -> R (Maybe J.CommandOrCodeAction)
    wrapCodeAction action = do
      (C.ClientCapabilities _ textDocCaps _) <- asksLspFuncs Core.clientCapabilities
      let literalSupport = textDocCaps >>= C._codeAction >>= C._codeActionLiteralSupport
      case literalSupport of
        Nothing ->
          case (action ^. J.edit, action ^. J.command) of
            (Just e, _) -> 
              let cmd = J.Command (action ^. J.title) cmdName (Just cmdParams)
                  cmdName = commandMap BM.! "hie:applyWorkspaceEdit"
                  cmdParams = J.toJSON [J.ApplyWorkspaceEditParams e]
              in return $ Just (J.CommandOrCodeActionCommand cmd)
            (_, Just (J.Command title cmdName args)) -> do
              let cmd = J.Command title (commandMap BM.! cmdName) args
              return $ Just (J.CommandOrCodeActionCommand cmd)
            _ -> error "A code action needs either a workspace edit or a command"
        Just _ -> return $ Just (J.CommandOrCodeActionCodeAction action)

    send :: [J.CodeAction] -> R ()
    send codeActions = do
      body <- J.List . catMaybes <$> mapM wrapCodeAction codeActions
      reactorSend $ RspCodeAction $ Core.makeResponseMessage req body

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

