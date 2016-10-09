{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Ide.Engine.Transport.LspStdio
  (
    lspStdioTransport
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import qualified Control.Exception as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import qualified Data.Aeson as J
import           Data.Default
import           Data.Either.Utils
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as T
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Control  as CTRL
import qualified Language.Haskell.LSP.Core     as GUI
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import qualified Language.Haskell.LSP.Utility  as U
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import           System.Exit
import           System.IO
import qualified System.Log.Logger as L

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}

-- ---------------------------------------------------------------------

lspStdioTransport :: TChan ChannelRequest -> IO ()
lspStdioTransport cin = do
  run cin >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c


-- ---------------------------------------------------------------------

run :: TChan ChannelRequest -> IO Int
run cin = flip E.catches handlers $ do

  cout <- atomically newTChan :: IO (TChan ChannelResponse)
  _ <- forkIO $ responseHandler cout

  flip E.finally finalProc $ do
    CTRL.run (cin,cout) hieHandlers hieOptions

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = L.removeAllHandlers
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

instance Default (TChan ChannelRequest) where
  def = error $ "code smell, default for TChan ChannelRequest does not make sense"

-- ---------------------------------------------------------------------

responseHandler :: TChan ChannelResponse -> IO ()
responseHandler cout = do
  U.logm $ "responseHandler starting up"
  forever $ do
    CResp pid rid res <- liftIO $ atomically $ readTChan cout
    CTRL.sendResponseMessage $ GUI.makeResponseMessage rid res
    return ()

-- ---------------------------------------------------------------------

hieOptions :: GUI.Options
hieOptions = def
-- hieOptions = def { GUI.textDocumentSync = Just J.TdSyncFull
--                  , GUI.codeLensProvider = Just def
--                  }

hieHandlers :: GUI.Handlers (TChan ChannelRequest,TChan ChannelResponse)
hieHandlers
  = def { GUI.renameHandler   = Just renameRequestHandler
        -- , GUI.codeLensHandler = Just codeLensHandler
        , GUI.didOpenTextDocumentNotificationHandler = Just didOpenTextDocumentNotificationHandler
        , GUI.didSaveTextDocumentNotificationHandler = Just didSaveTextDocumentNotificationHandler
        }

-- ---------------------------------------------------------------------

renameRequestHandler :: (TChan ChannelRequest,TChan ChannelResponse) -> J.RenameRequest -> IO J.RenameResponse
renameRequestHandler cin (J.RequestMessage _ origId _ _) = do
  let loc = def :: J.Location
      res  = GUI.makeResponseMessage origId loc
  return res

-- ---------------------------------------------------------------------

codeLensHandler :: (TChan ChannelRequest,TChan ChannelResponse) -> J.CodeLensRequest -> IO J.CodeLensResponse
codeLensHandler cin (J.RequestMessage _ origId _ _) = do
  let
    lens = J.CodeLens (J.Range (J.Position 1 1) (J.Position 1 10)) (Just (J.Command "codeLensCmd" "actionCmd" Nothing)) Nothing
    lenses = [lens]
    res = GUI.makeResponseMessage origId lenses
  return res

-- ---------------------------------------------------------------------

didOpenTextDocumentNotificationHandler :: (TChan ChannelRequest,TChan ChannelResponse) -> J.DidOpenTextDocumentNotification -> IO ()
didOpenTextDocumentNotificationHandler cin notification = do
  U.logm "\n******** got didOpenTextDocumentNotificationHandler, ignoring"

-- ---------------------------------------------------------------------

didSaveTextDocumentNotificationHandler :: (TChan ChannelRequest,TChan ChannelResponse)
                                       -> J.DidSaveTextDocumentNotification -> IO ()
didSaveTextDocumentNotificationHandler (cin,cout) notification = do
  U.logm $ "\n****** didSaveTextDocumentNotificationHandler: not processing"
  let J.TextDocumentIdentifier doc = J.textDocumentDidSaveTextDocumentParams $ fromJust $ J.paramsNotificationMessage notification
      fileName = drop (length ("file://"::String)) doc
  U.logs $ "\n********* doc=" ++ show doc
  -- let req = CReq "ghcmod" 1 (IdeRequest "lint" (Map.fromList [("file", ParamFileP (T.pack fileName))])) cout
  let req = CReq "applyrefact" 1 (IdeRequest "lint" (Map.fromList [("file", ParamFileP (T.pack fileName))])) cout
  atomically $ writeTChan cin req
