{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.GhcModPlugin where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Aeson
import           Haskell.Ide.Engine.PluginDescriptor
-- import           Haskell.Ide.Engine.PluginUtils
import qualified Data.Text as T
-- import qualified GHC as GHC
import qualified Language.Haskell.GhcMod as GM
-- import qualified Language.Haskell.GhcMod.Types as GM
import qualified Language.Haskell.GhcMod.Monad as GM

-- ---------------------------------------------------------------------

ghcmodDescriptor :: PluginDescriptor
ghcmodDescriptor = PluginDescriptor
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
      liftIO $ doCheck fileName

-- --   Warnings and errors are returned.
-- checkSyntax :: IOish m
--             => [FilePath]  -- ^ The target files.
--             -> GhcModT m String
-- checkSyntax []    = return ""
-- checkSyntax files = either id id <$> check files

-- ---------------------------------------------------------------------

-- doCheck :: (MonadIO m,GHC.GhcMonad m,HasIdeState m) => FilePath -> m IdeResponse
doCheck :: FilePath -> IO IdeResponse
doCheck fileName = do
  -- r <- GM.checkSyntax [fileName]
  let opts = GM.defaultOptions
  (r,_l) <- GM.runGmOutT opts $ GM.runGhcModT opts $ do
    -- s <- GM.getSession
      -- GM.setSession s
      -- setTargets [fileName]
      GM.checkSyntax [fileName]
  -- (Either GM.GhcModError String, GM.GhcModLog)
  case r of
    Left e -> return $ IdeResponseError (toJSON $ T.pack $ "doCheck:got " ++ show e)
    Right checkResult -> return $ (IdeResponseOk (toJSON checkResult))
  -- runGhcModT $ do s <- getSession; liftIO $ runGhcModT $ do setSession s; check fileName

-- ---------------------------------------------------------------------

catchException :: (IO t) -> IO (Either String t)
catchException f = do
  res <- handle handler (f >>= \r -> return $ Right r)
  return res
  where
    handler:: SomeException -> IO (Either String t)
    handler e = return (Left (show e))

