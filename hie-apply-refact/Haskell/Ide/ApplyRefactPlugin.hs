{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.ApplyRefactPlugin where

import           Control.Exception
import           Control.Monad.IO.Class
import qualified Data.Text as T
import           Data.Vinyl
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import qualified Language.Haskell.GhcMod as GM (defaultOptions)
import           Language.Haskell.HLint
import           System.Directory
import           System.FilePath.Posix
import           System.IO
import           System.IO.Extra

-- ---------------------------------------------------------------------

applyRefactDescriptor :: PluginDescriptor
applyRefactDescriptor = PluginDescriptor
  {
    pdUIShortName = "ApplyRefact"
  , pdUIOverview = "apply-refact applies refactorings specified by the refact package. It is currently integrated into hlint to enable the automatic application of suggestions."
    , pdCommands =
      [
        buildCommand applyOneCmd "applyOne" "Apply a single hint"
                    [".hs"] [CtxPoint] []

      -- , buildCommand applyAllCmd "applyAll" "Apply all hints to the file"
      --               [".hs"] [CtxFile] []

      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

applyOneCmd :: CommandFunc HieDiff
applyOneCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& RNil) -> do
      res <- liftIO $ catchException $ applyHint (T.unpack fileName) pos
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "applyOne: " ++ show err) Nothing)
        Right fs -> do
          -- r <- liftIO $ makeRefactorResult [fs]
          return (IdeResponseOk fs)
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "ApplyRefactPlugin.demoteCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- demote :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

applyHint :: FilePath -> Pos -> IO HieDiff
applyHint file pos = do
  withTempFile $ \f -> do
    let opts = "--output " ++ f
    hlint ["--refactor", "-", "--refactor-options=" ++ show opts, file]
    diff <- makeDiffResult file f
    return diff

-- ---------------------------------------------------------------------

makeDiffResult :: FilePath -> FilePath -> IO HieDiff
makeDiffResult orig new = do
  (HieDiff f s d) <- diffFiles orig new
  f' <- liftIO $ makeRelativeToCurrentDirectory f
  s' <- liftIO $ makeRelativeToCurrentDirectory s
  return (HieDiff f' s' d)

-- ---------------------------------------------------------------------

catchException :: (IO t) -> IO (Either String t)
catchException f = do
  res <- handle handler (f >>= \r -> return $ Right r)
  return res
  where
    handler:: SomeException -> IO (Either String t)
    handler e = return (Left (show e))
