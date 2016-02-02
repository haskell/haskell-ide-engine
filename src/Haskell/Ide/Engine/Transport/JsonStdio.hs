{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-- |Provide a protocol adapter/transport for JSON over stdio

module Haskell.Ide.Engine.Transport.JsonStdio where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Haskell.Ide.Engine.Transport.Pipes
import           Haskell.Ide.Engine.Types
import qualified Pipes as P
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import           System.IO

-- TODO: Can pass in a handle, then it is general
jsonStdioTransport :: Bool -> TChan ChannelRequest -> IO ()
jsonStdioTransport oneShot cin = do
  cout <- atomically $ newTChan :: IO (TChan ChannelResponse)
  hSetBuffering stdout NoBuffering
  _ <- forkIO $ P.runEffect (parseFrames PB.stdin P.>-> parseToJsonPipe oneShot cin cout 1)
  P.runEffect (tchanProducer oneShot cout P.>-> encodePipe P.>-> serializePipe P.>-> PB.stdout)


-- to help with type inference
printTest :: (MonadIO m) => P.Consumer' [Int] m r
printTest = P.print
