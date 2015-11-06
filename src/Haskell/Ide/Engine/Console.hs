{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Console where

import           Control.Concurrent
import           Control.Monad.IO.Class
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Text as T
import           Haskell.Ide.Engine.BasePlugin
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import           System.Console.Haskeline

-- ---------------------------------------------------------------------

data ReplEnv = ReplEnv
      { envContext :: Context
      } deriving (Show)

emptyEnv :: ReplEnv
emptyEnv = ReplEnv emptyContext

-- ---------------------------------------------------------------------

consoleListener :: Plugins -> Chan ChannelRequest -> IO ()
consoleListener plugins cin = do
  cout <- newChan :: IO (Chan ChannelResponse)
  let
    loop :: ReplEnv -> Int -> InputT IO ()
    loop env cid = do
        minput <- getInputLine "HIE> "
        case fmap T.pack minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just cmdArg -> do
              let
                req = case T.words cmdArg of
                  [] -> Left $ "empty command"
                  (cmdStr:ps) -> case Map.lookup cmdStr (replPluginInfo plugins) of
                                  Nothing -> Left $ "unrecognised command:" <> cmdStr
                                  Just (plugin,cmd) -> Right $ (plugin,IdeRequest (cmdName $ cmdDesc cmd) (envContext env) (convertToParams ps))
              case req of
                Left err -> outputStrLn (T.unpack err)
                Right (plugin,reqVal) -> do
                  liftIO $ writeChan cin (CReq plugin cid reqVal cout)
                  rsp <- liftIO $ readChan cout
                  outputStrLn $ show (coutResp rsp)
                  -- outputStrLn $ "Input was: " ++ cmdArg
              loop env (cid + 1)

  runInputT defaultSettings (loop emptyEnv 1)

-- ---------------------------------------------------------------------

-- TODO: Delete this
{-
replListener :: Plugins -> Chan ChannelRequest -> IO ()
replListener plugins cin = do
  cout <- newChan :: IO (Chan ChannelResponse)
  hSetBuffering stdout NoBuffering
  putStrLn $ "HIE version : " ++ version
  let
    prompt = "HIE> "
    loop env cid = do
      putStr prompt
      cmdArg <- getLine
      let
        req = case words cmdArg of
          [] -> Left $ "empty command"
          (cmdStr:ps) -> case Map.lookup cmdStr (replPluginInfo plugins) of
                          Nothing -> Left $ "unrecognised command:" ++ cmdStr
                          Just (plugin,cmd) -> Right $ (plugin,IdeRequest (cmdName $ cmdDesc cmd) (envContext env) (convertToParams ps))
      case req of
        Left err -> putStrLn err
        Right (plugin,reqVal) -> do
          writeChan cin (CReq plugin cid reqVal cout)
          rsp <- readChan cout
          putStrLn $ show (coutResp rsp)
      loop env (cid + 1)
  loop emptyEnv 1
-}

-- ---------------------------------------------------------------------

convertToParams :: [T.Text] -> Map.Map T.Text T.Text
convertToParams ss = Map.fromList $ map splitOnColon ss

-- ---------------------------------------------------------------------

-- |Split a string of the form "first:rest" into ("first","rest")
splitOnColon :: T.Text -> (T.Text,T.Text)
splitOnColon "" = ("","")
splitOnColon s = (first,second)
  where
    (first,rest) = T.break (==':') s
    second = case rest of
      "" -> ""
      _ -> T.tail rest

-- ---------------------------------------------------------------------
