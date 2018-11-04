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
import qualified Control.Exception                     as E
import           Control.Monad
import           Control.Monad.STM
import           Control.Monad.IO.Class
import qualified Data.Aeson                            as J
import qualified Data.ByteString.Builder               as B
import qualified Data.ByteString.Lazy.Char8            as B
import           Data.Default
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Text                             as T
import           GHC.Generics
import qualified Haskell.Ide.Engine.Scheduler          as Scheduler
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Types            as J
import           System.Exit
import           System.IO
import qualified System.Log.Logger                     as L

-- ---------------------------------------------------------------------

{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}

-- ---------------------------------------------------------------------

jsonStdioTransport :: Scheduler.Scheduler IO -> IO ()
jsonStdioTransport scheduler = do
  run scheduler >>= \case
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

run :: Scheduler.Scheduler IO -> IO Int
run scheduler = flip E.catches handlers $ do
  flip E.finally finalProc $ do
    rout <- atomically newTChan :: IO (TChan ReactorOutput)

    let race3_ a b c = race_ a (race_ b c)

    let errorHandler lid _ e = liftIO $ hPutStrLn stderr $ "Got an error for request " ++ show lid ++ ": " ++ T.unpack e
        callbackHandler callback x = callback x

    race3_ (Scheduler.runScheduler scheduler errorHandler callbackHandler def)
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
        mreq <- getNextReq
        case mreq of
          Nothing -> return()
          Just req -> do
            let vfsFunc _ = return Nothing -- TODO: Stub for now, what to do?
            let preq = GReq 0 (context req) Nothing (Just $ J.IdInt rid) (liftIO . callback)
                  $ runPluginCommand (plugin req) (command req) vfsFunc (arg req)
                rid = reqId req
                callback = sendResponse rid . dynToJSON
            Scheduler.sendRequest scheduler Nothing preq

getNextReq :: IO (Maybe ReactorInput)
getNextReq = do
  mbs <- fmap B.toLazyByteString <$> readReqByteString
  case mbs of
    -- EOF
    Nothing -> return Nothing
    Just bs -> case J.eitherDecode bs of
      Left err  -> do
        hPutStrLn stderr $ "Couldn't parse" ++ B.unpack bs ++ "\n got error" ++ show err
        getNextReq
      Right req -> return $ Just req
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