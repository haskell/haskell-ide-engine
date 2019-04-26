module Haskell.Ide.Engine.Channel
  ( InChan
  , OutChan
  , newChan
  , newChanSTM
  , readChan
  , readChanSTM
  , writeChan
  , writeChanSTM
  )
where

import qualified Control.Concurrent.STM.TChan  as TChan
import qualified Control.Concurrent.STM        as STM

-- | The writing end of a STM channel, only values of type 'a' can be written
-- to the channel
newtype InChan a = InChan (TChan.TChan a)

-- | The reading end of a STM channel, values of type 'a' can be expected to
-- be read.
newtype OutChan a = OutChan (TChan.TChan a)

-- | Returns the reading and writing ends of a channel able to trasmit values of
-- a single given type
newChan :: IO (InChan a, OutChan a)
newChan = STM.atomically newChanSTM

-- | STM version of 'newChan', useful for chaining many STM calls inside a single
-- 'atomically' block.
newChanSTM :: STM.STM (InChan a, OutChan a)
newChanSTM = do
  chan <- TChan.newTChan
  return (InChan chan, OutChan chan)

-- | Consumes and returns the next value of the given channel
readChan :: OutChan a -> IO a
readChan = STM.atomically . readChanSTM

-- | STM version of 'readChan', useful for chaining many STM calls inside a single
-- 'atomically' block.
readChanSTM :: OutChan a -> STM.STM a
readChanSTM (OutChan chan) = STM.readTChan chan

-- | Writes a value to a channel.
writeChan :: InChan a -> a -> IO ()
writeChan chan val = STM.atomically (writeChanSTM chan val)

-- | STM version of 'writeChan', useful for chaining many STM calls inside a single
-- 'atomically' block.
writeChanSTM :: InChan a -> a -> STM.STM ()
writeChanSTM (InChan chan) = STM.writeTChan chan
