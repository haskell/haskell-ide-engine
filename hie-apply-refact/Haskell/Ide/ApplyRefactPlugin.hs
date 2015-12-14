{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.ApplyRefactPlugin where

import           Control.Exception
import           Control.Monad.IO.Class
import qualified Data.Text as T
import           Data.Vinyl
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import qualified Language.Haskell.GhcMod as GM (defaultOptions)
import           Language.Haskell.HLint
import           System.Directory
import           System.Exit
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


-- ---------------------------------------------------------------------

applyHint :: FilePath -> Pos -> IO (Either t HieDiff)
applyHint file pos = do
  withTempFile $ \f -> do
    absFile <- makeAbsolute file
    -- hlint /tmp/Foo.hs --refactor --refactor-options="-o /tmp/Bar.hs --pos 2,8"

    -- let opts = "-o " ++ f
    let opts = "-o /tmp/BarOne.hs"
    -- let hlintOpts = [absFile, "--refactor", "--refactor-options=" ++ show opts ]
    let hlintOpts = ["/tmp/Foo.hs", "--refactor", "--refactor-options=" ++ opts ]
    -- let hlintOpts = ["/tmp/Foo.hs", "--refactor" ]
    logm $ "applyHint=" ++ show hlintOpts
    res <- catchException $ hlint hlintOpts
    logm $ "applyHint:res=" ++ show res
    case res of
      Left ExitSuccess -> do
        diff <- makeDiffResult file f
        logm $ "applyHint:diff=" ++ show diff
        return $ Right diff
      _ -> return res

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
