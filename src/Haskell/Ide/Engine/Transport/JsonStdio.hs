{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes   #-}

module Haskell.Ide.Engine.Transport.JsonStdio
  (
    jsonStdioTransport
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import qualified Control.Exception                     as E
import           Control.Monad
import           Control.Monad.STM
import           Control.Monad.IO.Class
import qualified Data.Aeson                            as J
import qualified Data.ByteString.Builder               as B
import qualified Data.ByteString.Lazy.Char8            as B
import           Data.Default
import qualified Data.Map                              as Map
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Set                              as S
import qualified Data.Text                             as T
import           GHC.Generics
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Types            as J
import           Language.Haskell.LSP.Types.Capabilities
import           System.Exit
import           System.IO
import qualified System.Log.Logger                     as L

-- ---------------------------------------------------------------------

{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}

-- ---------------------------------------------------------------------

jsonStdioTransport :: (DispatcherEnv -> ErrorHandler -> CallbackHandler IO -> ClientCapabilities -> IO ()) -> TChan (PluginRequest IO) -> IO ()
jsonStdioTransport hieDispatcherProc cin = do
  run hieDispatcherProc cin >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- ---------------------------------------------------------------------

data ReactorInput =
  ReactorInput
  { reqId   :: Int
  , plugin  :: T.Text
  , command :: T.Text
  , context :: Maybe J.Uri
  , arg     :: J.Value
  } deriving (Eq, Show, Generic, J.ToJSON, J.FromJSON)

data ReactorOutput = ReactorOutput
  { _resId    :: Int
  , _response :: J.Value
  } deriving (Eq, Show, Generic, J.ToJSON, J.FromJSON)

run :: (DispatcherEnv -> ErrorHandler -> CallbackHandler IO -> ClientCapabilities -> IO ()) -> TChan (PluginRequest IO) -> IO Int
run dispatcherProc cin = flip E.catches handlers $ do
  flip E.finally finalProc $ do

    rout <- atomically newTChan :: IO (TChan ReactorOutput)
    cancelTVar      <- atomically $ newTVar S.empty
    wipTVar         <- atomically $ newTVar S.empty
    versionTVar     <- atomically $ newTVar Map.empty
    let dispatcherEnv = DispatcherEnv
          { cancelReqsTVar     = cancelTVar
          , wipReqsTVar        = wipTVar
          , docVersionTVar     = versionTVar
          }

    let race3_ a b c = race_ a (race_ b c)

    let errorHandler lid _ e = liftIO $ hPutStrLn stderr $ "Got an error for request " ++ show lid ++ ": " ++ T.unpack e
        callbackHandler callback x = callback x

    race3_ (dispatcherProc dispatcherEnv errorHandler callbackHandler def)
           (outWriter rout)
           (reactor rout)

    return 0

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = L.removeAllHandlers
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

    outWriter rout = forever $ do
      out <- atomically $ readTChan rout
      B.putStr $ J.encode out
      putChar '\STX'

    reactor rout =
      let sendResponse rid resp = atomically $ writeTChan rout (ReactorOutput rid resp) in
      forever $ do
        req <- getNextReq
        let preq = GReq 0 (context req) Nothing (Just $ J.IdInt rid) (liftIO . callback)
              $ runPluginCommand (CommandId (plugin req) (command req)) (arg req)
            rid = reqId req
            callback = sendResponse rid . dynToJSON
        atomically $ writeTChan cin preq

getNextReq :: IO ReactorInput
getNextReq = do
  mbs <- fmap B.toLazyByteString <$> readReqByteString
  case mbs of
    -- EOF
    Nothing -> exitSuccess
    Just bs -> case J.eitherDecode bs of
      Left err  -> do
        hPutStrLn stderr $ "Couldn't parse" ++ B.unpack bs ++ "\n got error" ++ show err
        getNextReq
      Right req -> return req
  where
    readReqByteString = do
      eof <- isEOF
      if eof then
        return Nothing
      else do
        char <- getChar
        if char == '\STX' then
          return $ Just ""
        else do
          rest <- readReqByteString
          let cur = B.charUtf8 char
          return $ Just $ maybe cur (cur <>) rest


