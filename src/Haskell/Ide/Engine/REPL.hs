{-# LANGUAGE TemplateHaskell #-}
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
        req = case dropWhileEnd isSpace cmdArg of
          "hello"   -> Right $ ("eg2", IdeRequest "sayHello" emptyContext Map.empty)
          "version" -> Right $ ("base",IdeRequest "version"  emptyContext Map.empty)
          "plugins" -> Right $ ("base",IdeRequest "plugins"  emptyContext Map.empty)
          cmdStr   -> case Map.lookup cmdStr (replPluginInfo plugins) of
                          Nothing -> Left $ "unrecognised command:" ++ cmdStr
                          Just (plugin,uic) -> Right $ (plugin,IdeRequest (uiCmdName uic) (envContext env) Map.empty)
      case req of
        Left err -> putStrLn err
        Right (plugin,reqVal) -> do
          writeChan cin (CReq plugin cid reqVal cout)
          rsp <- readChan cout
          putStrLn $ show (coutResp rsp)
      loop env (cid + 1)
  loop emptyEnv 1

-- ---------------------------------------------------------------------

version =
    let commitCount = $gitCommitCount
    in  concat $ concat
            [ [$(simpleVersion Meta.version)]
              -- Leave out number of commits for --depth=1 clone
              -- See https://github.com/commercialhaskell/stack/issues/792
            , [" (" ++ commitCount ++ " commits)" | commitCount /= ("1"::String) &&
                                                    commitCount /= ("UNKNOWN" :: String)]
            , [" ", display buildArch]
            ]

-- ---------------------------------------------------------------------

replPluginInfo :: Plugins -> Map.Map String (String,UiCommand)
replPluginInfo plugins = Map.fromList commands
  where
    commands = concatMap extractCommands $ Map.toList plugins
    extractCommands (pluginName,PluginReg descriptor _) = cmds
      where
        cmds = map (\uic -> (pluginName ++ ":" ++ (uiCmdName uic),(pluginName,uic))) $ pdUiCommands descriptor
