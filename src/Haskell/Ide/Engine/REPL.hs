{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.REPL where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Haskell.Ide.Engine.BasePlugin
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import qualified Data.Map as Map
import           System.Console.Haskeline
import           System.IO

-- ---------------------------------------------------------------------

data ReplEnv = ReplEnv
      { envContext :: Context
      } deriving (Show)

emptyEnv :: ReplEnv
emptyEnv = ReplEnv emptyContext

-- ---------------------------------------------------------------------

replListener' :: Plugins -> Chan ChannelRequest -> IO ()
replListener' plugins cin = do
  cout <- newChan :: IO (Chan ChannelResponse)
  let
    loop :: ReplEnv -> Int -> InputT IO ()
    loop env cid = do
        minput <- getInputLine "HIE> "
        case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just cmdArg -> do
              let
                req = case words cmdArg of
                  [] -> Left $ "empty command"
                  (cmdStr:ps) -> case Map.lookup cmdStr (replPluginInfo plugins) of
                                  Nothing -> Left $ "unrecognised command:" ++ cmdStr
                                  Just (plugin,cmd) -> Right $ (plugin,IdeRequest (cmdName $ cmdDesc cmd) (envContext env) (convertToParams ps))
              case req of
                Left err -> outputStrLn err
                Right (plugin,reqVal) -> do
                  liftIO $ writeChan cin (CReq plugin cid reqVal cout)
                  rsp <- liftIO $ readChan cout
                  outputStrLn $ show (coutResp rsp)
                  -- outputStrLn $ "Input was: " ++ cmdArg
              loop env (cid + 1)

  runInputT defaultSettings (loop emptyEnv 1)

-- ---------------------------------------------------------------------

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

-- ---------------------------------------------------------------------

convertToParams :: [String] -> Map.Map String String
convertToParams ss = Map.fromList $ map splitOnColon ss

-- ---------------------------------------------------------------------

-- |Split a string of the form "first:rest" into ("first","rest")
splitOnColon :: String -> (String,String)
splitOnColon "" = ("","")
splitOnColon s = (first,second)
  where
    (first,rest) = break (==':') s
    second = case rest of
      "" -> ""
      _ -> tail rest

-- ---------------------------------------------------------------------
