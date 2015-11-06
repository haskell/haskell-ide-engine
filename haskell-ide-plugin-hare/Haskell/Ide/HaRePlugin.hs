{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.HaRePlugin where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Aeson
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Language.Haskell.Refact.HaRe
-- import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Language.Haskell.GhcMod as GM (defaultOptions)

-- ---------------------------------------------------------------------

hareDescriptor :: PluginDescriptor
hareDescriptor = PluginDescriptor
  {
    pdCommands =
      [
        Command
          { cmdDesc = CommandDesc
                     { cmdName = "rename"
                     , cmdContexts = [CtxPoint]
                     , cmdAdditionalParams = [RP "name"]
                     }
          , cmdFunc = renameCmd
          }
      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

renameCmd :: Dispatcher
renameCmd req = do
  case getParams ["name"] req of
    Left err -> return err
    Right [name] -> do
      let
        mf = do -- Maybe monad
              fileName <- ctxFile (ideContext req)
              pos      <- ctxStartPos (ideContext req)
              return (fileName,pos)
      case mf of
        Nothing -> return (IdeResponseFail (String (T.pack $ "wrong context, needed file and pos")))
        Just (filename,pos) -> do
          res <- liftIO $ catchException $ rename defaultSettings GM.defaultOptions filename name pos
          case res of
            Left err -> return (IdeResponseFail (toJSON err))
            Right fs -> return (IdeResponseOk (toJSON fs))
    Right _ -> error "HarePlugin.renameCmd: should never get here"

-- rename :: RefactSettings -> Options -> FilePath -> String -> SimpPos -> IO [FilePath] 

-- ---------------------------------------------------------------------

catchException :: (IO t) -> IO (Either String t)
catchException f = do
  res <- handle handler (f >>= \r -> return $ Right r)
  return res
  where
    handler:: SomeException -> IO (Either String t)
    handler e = return (Left (show e))
