{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.REPL where

import           Control.Concurrent
import           Control.Exception
import           Control.Logging
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Char
import           Data.Foldable
import           Data.IORef
import           Data.List
import           Data.Traversable
import qualified Data.Text as T
import           Data.Version (showVersion)
import           Development.GitRev (gitCommitCount)
import           Distribution.System (buildArch)
import           Distribution.Text (display)
import           Haskell.Ide.Engine.BasePlugin
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.Options
import           Haskell.Ide.Engine.Plugin
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Transport.JsonStdio
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.GhcMod.LightGhc as GM
import qualified Language.Haskell.GhcMod.Monad as GM
import qualified Language.Haskell.GhcMod.Types as GM
import           Module (mkModuleName)
import           Options.Applicative.Simple
import qualified Data.Map as Map
import qualified Paths_haskell_ide_engine as Meta
import           Data.Time
import           System.IO
import           Prelude hiding (log)

-- ---------------------------------------------------------------------

data ReplEnv = ReplEnv
      { envContext :: Context
      } deriving (Show)

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
      -- This command parsing should be built up from the PluginDescriptors
      let
        req = case words cmdArg of
          -- "hello"   -> Right $ ("eg2", IdeRequest "sayHello" emptyContext Map.empty)
          -- "version" -> Right $ ("base",IdeRequest "version"  emptyContext Map.empty)
          -- "plugins" -> Right $ ("base",IdeRequest "plugins"  emptyContext Map.empty)
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
splitOnColon str = (first,second)
  where
    (first,rest) = break (==':') str
    second = case rest of
      "" -> ""
      _ -> tail rest

-- ---------------------------------------------------------------------
