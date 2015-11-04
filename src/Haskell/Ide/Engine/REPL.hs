{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.REPL where

import           Control.Concurrent
-- import           Data.List
import           Haskell.Ide.Engine.BasePlugin
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types
import qualified Data.Map as Map
import           System.IO
-- import           Prelude hiding (log)

-- ---------------------------------------------------------------------

data ReplEnv = ReplEnv
      { envContext :: Context
      } deriving (Show)

emptyEnv :: ReplEnv
emptyEnv = ReplEnv emptyContext

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
                          Just (plugin,uic) -> Right $ (plugin,IdeRequest (uiCmdName uic) (envContext env) (convertToParams ps))
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
