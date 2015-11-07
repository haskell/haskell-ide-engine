{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.GhcModPlugin where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Aeson
import           Haskell.Ide.Engine.PluginDescriptor
-- import           Haskell.Ide.Engine.PluginUtils
import qualified Data.Text as T
import qualified GHC as GHC
-- import qualified Language.Haskell.GhcMod as GM

-- ---------------------------------------------------------------------

hareDescriptor :: PluginDescriptor
hareDescriptor = PluginDescriptor
  {
    pdCommands =
      [
        Command
          { cmdDesc = CommandDesc
                     { cmdName = "check"
                     , cmdUiDescription = "check a file for GHC warnings and errors"
                     , cmdContexts = [CtxFile]
                     , cmdAdditionalParams = []
                     }
          , cmdFunc = checkCmd
          }
      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

checkCmd :: Dispatcher
checkCmd req = do
  let
    mf = do -- Maybe monad
          fileName <- ctxFile (ideContext req)
          return fileName
  case mf of
    Nothing -> return (IdeResponseFail (String (T.pack $ "wrong context, needed file and pos")))
    Just fileName -> do
      doCheck fileName

-- --   Warnings and errors are returned.
-- checkSyntax :: IOish m
--             => [FilePath]  -- ^ The target files.
--             -> GhcModT m String
-- checkSyntax []    = return ""
-- checkSyntax files = either id id <$> check files

-- ---------------------------------------------------------------------

doCheck :: (MonadIO m,GHC.GhcMonad m,HasIdeState m) => FilePath -> m IdeResponse
doCheck _fileName = do
  return $ IdeResponseError (toJSON $ T.pack "not implemented (yet)")
  -- runGhcModT $ do s <- getSession; liftIO $ runGhcModT $ do setSession s; check fileName

-- ---------------------------------------------------------------------

catchException :: (IO t) -> IO (Either String t)
catchException f = do
  res <- handle handler (f >>= \r -> return $ Right r)
  return res
  where
    handler:: SomeException -> IO (Either String t)
    handler e = return (Left (show e))

