-- |Provide a protocol adapter/transport for JSON over stdio

module Haskell.Ide.Transport.JsonStdio where

import           Control.Concurrent
import           Control.Lens
import           Data.Aeson
import           Data.Char
import           Data.List
import qualified Data.Map as Map
import           Haskell.Ide.Monad
import           Haskell.Ide.Options
import           Haskell.Ide.Plugin
import           Haskell.Ide.PluginDescriptor
import           Haskell.Ide.Types
import           Pipes
import qualified Pipes.Aeson as P
import qualified Pipes.ByteString as P
import qualified Pipes.Prelude as P
import qualified Data.ByteString.Char8 as B
import           Pipes.Parse
import           Pipes.Prelude hiding (show)
import           System.IO

jsonStdioTransport :: Chan ChannelRequest -> IO ()
jsonStdioTransport cin = do
  cout <- newChan :: IO (Chan ChannelResponse)
  hSetBuffering stdout NoBuffering
  let
    loop cid stream = do
      putStrLn "calling go"
      -- r <- go
      (r,stream') <- runStateT decodeMsg stream
      putStrLn $ "got:" ++ show r
      loop (cid + 1) stream'
  loop 1 P.stdin

decodeMsg :: (Monad m) => Parser B.ByteString m (Maybe (Either P.DecodingError [Int]))
decodeMsg = P.decode

-- to help with type inference
printTest :: (MonadIO m) => Consumer' [Int] m r
printTest = P.print

