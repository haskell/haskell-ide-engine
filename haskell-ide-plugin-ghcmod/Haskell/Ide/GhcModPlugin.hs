{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.GhcModPlugin where

import           Control.Exception
-- import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
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
                     , cmdFileExtensions = [".hs",".lhs"]
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
  case getParams ["file"] req of
    Left err -> return err
    Right [ParamFile fileName] -> do
      liftIO $ doCheck (T.unpack fileName)
    Right x -> error $ "GhcModPlugin.checkCmd: got unexpected file param:" ++ show x

-- --   Warnings and errors are returned.
-- checkSyntax :: IOish m
--             => [FilePath]  -- ^ The target files.
--             -> GhcModT m String
-- checkSyntax []    = return ""
-- checkSyntax files = either id id <$> check files

-- ---------------------------------------------------------------------

-- doCheck :: (MonadIO m,GHC.GhcMonad m,HasIdeState m) => FilePath -> m IdeResponse
doCheck :: FilePath -> IO IdeResponse
-- doCheck :: GHC.GhcMonad m => FilePath -> m IdeResponse
doCheck fileName = runGhcModCommand (GM.checkSyntax [fileName])

-- ---------------------------------------------------------------------

-- TODO: Need to thread the session through as in the commented out code below.
runGhcModCommand :: (ToJSON a) => GM.GmT (GM.GmOutT (GM.GmOutT IO)) a -> IO IdeResponse
runGhcModCommand cmd = do
  let opts = GM.defaultOptions
  -- s <- GHC.getSession
  (r,_l) <- GM.runGmOutT opts $ GM.runGhcModT opts $ do
  -- (r,_l) <- GM.runGhcModT opts $ do
      -- GHC.setSession s
      -- s <- GM.getSession
      -- GM.setSession s
      -- setTargets [fileName]
      cr <- cmd
      -- s' <- GHC.getSession
      let s' = undefined
      return (cr,s')
  -- (Either GM.GhcModError String, GM.GhcModLog)
  case r of
    Left e -> return $ IdeResponseError (toJSON $ T.pack $ "doCheck:got " ++ show e)
    Right (checkResult,_s3) -> do
      -- GHC.setSession s3
      return $ (IdeResponseOk (toJSON checkResult))

{-
dispatcher = runGmlT $ forever $ do
    s <- getSession
    (r, s') <- runGhcModT $ do
      setSession s
      r <- checkSyntax
      s <- getSession
      return (r,s)
    setSession s'

-}

-- ---------------------------------------------------------------------

catchException :: (IO t) -> IO (Either String t)
catchException f = do
  res <- handle handler (f >>= \r -> return $ Right r)
  return res
  where
    handler:: SomeException -> IO (Either String t)
    handler e = return (Left (show e))

