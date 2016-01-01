{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.ApplyRefactPlugin where

import           Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Vinyl
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import           Language.Haskell.HLint
import           Language.Haskell.HLint3
import           Refact.Apply
import qualified Refact.Types as R
import           Refact.Types hiding (SrcSpan)
import           System.Directory
import           System.IO.Extra

-- ---------------------------------------------------------------------

applyRefactDescriptor :: TaggedPluginDescriptor '["applyOne","applyAll"]
applyRefactDescriptor = PluginDescriptor
  {
    pdUIShortName = "ApplyRefact"
  , pdUIOverview = "apply-refact applies refactorings specified by the refact package. It is currently integrated into hlint to enable the automatic application of suggestions."
    , pdCommands =

        buildCommand applyOneCmd Proxy "Apply a single hint"
                    [".hs"] [CtxPoint] []

      :& buildCommand applyAllCmd Proxy "Apply all hints to the file"
                     [".hs"] [CtxFile] []

      :& RNil
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

applyOneCmd :: CommandFunc HieDiff
applyOneCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& RNil) -> do
      res <- liftIO $ applyHint (T.unpack fileName) (Just pos)
      logm $ "applyOneCmd:res=" ++ show res
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "applyOne: " ++ show err) Nothing)
        Right fs -> return (IdeResponseOk fs)
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "ApplyRefactPlugin.applyOneCmd: ghc’s exhaustiveness checker is broken" Nothing)


-- ---------------------------------------------------------------------

applyAllCmd :: CommandFunc HieDiff
applyAllCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& RNil) -> do
      res <- liftIO $ applyHint (T.unpack fileName) Nothing
      logm $ "applyAllCmd:res=" ++ show res
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "applyOne: " ++ show err) Nothing)
        Right fs -> return (IdeResponseOk fs)
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "ApplyRefactPlugin.applyOneCmd: ghc’s exhaustiveness checker is broken" Nothing)


-- ---------------------------------------------------------------------

applyHint :: FilePath -> Maybe Pos -> IO (Either String HieDiff)
applyHint file mpos = do
  withTempFile $ \f -> do
    -- absFile <- makeAbsolute file
    -- hlint /tmp/Foo.hs --refactor --refactor-options="-o /tmp/Bar.hs --pos 2,8"

    let
      optsf = "-o " ++ f
      opts = case mpos of
        Nothing -> optsf
        Just (r,c) -> optsf ++ " --pos " ++ show r ++ "," ++ show c
    -- let hlintOpts = [file, "--quiet", "--refactor", "--refactor-options=" ++ opts ]
    let hlintOpts = [file, "--quiet" ]
    logm $ "applyHint=" ++ show hlintOpts
    res <- catchException $ hlint hlintOpts
    logm $ "applyHint:res=" ++ show res
    -- res <- hlint hlintOpts
    case res of
      Left x  -> return $ Left (show x)
      Right x -> do
        let commands = makeApplyRefact x
        logm $ "applyHint:commands=" ++ show commands
        appliedFile <- applyRefactorings mpos commands file
        diff <- makeDiffResult file (T.pack appliedFile)
        logm $ "applyHint:diff=" ++ show diff
        return $ Right diff

-- ---------------------------------------------------------------------

makeApplyRefact :: [Suggestion] -> [(String, [Refactoring R.SrcSpan])]
makeApplyRefact suggestions =
  map (\(Suggestion i) -> (show i, ideaRefactoring i)) suggestions

-- ---------------------------------------------------------------------


makeDiffResult :: FilePath -> T.Text -> IO HieDiff
makeDiffResult orig new = do
  origText <- T.readFile orig
  let (HieDiff f s d) = diffText (orig,origText) ("changed",new)
  f' <- liftIO $ makeRelativeToCurrentDirectory f
  -- return (HieDiff f' s' d)
  return (HieDiff f' "changed" d)
