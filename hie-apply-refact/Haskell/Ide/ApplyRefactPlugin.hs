{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.ApplyRefactPlugin where

import           Control.Arrow
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Vinyl
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import           Language.Haskell.HLint3 as Hlint
import           Refact.Apply
import           System.Directory
import           System.IO.Extra

-- ---------------------------------------------------------------------

applyRefactDescriptor :: TaggedPluginDescriptor _
applyRefactDescriptor = PluginDescriptor
  {
    pdUIShortName = "ApplyRefact"
  , pdUIOverview = "apply-refact applies refactorings specified by the refact package. It is currently integrated into hlint to enable the automatic application of suggestions."
    , pdCommands =

        buildCommand applyOneCmd (Proxy :: Proxy "applyOne") "Apply a single hint"
                   [".hs"] (SCtxPoint :& RNil) RNil SaveAll

      :& buildCommand applyAllCmd (Proxy :: Proxy "applyAll") "Apply all hints to the file"
                   [".hs"] (SCtxFile :& RNil) RNil SaveAll

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
                      (T.pack $ "applyOne: " ++ show err) Null)
        Right fs -> return (IdeResponseOk fs)
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "ApplyRefactPlugin.applyOneCmd: ghc’s exhaustiveness checker is broken" Null)


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
                      (T.pack $ "applyOne: " ++ show err) Null)
        Right fs -> return (IdeResponseOk fs)
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "ApplyRefactPlugin.applyOneCmd: ghc’s exhaustiveness checker is broken" Null)


-- ---------------------------------------------------------------------

applyHint :: FilePath -> Maybe Pos -> IO (Either String HieDiff)
applyHint file mpos = do
  withTempFile $ \f -> do
    let optsf = "-o " ++ f
        opts = case mpos of
                 Nothing -> optsf
                 Just (Pos (Line l) (Col c)) -> optsf ++ " --pos " ++ show l ++ "," ++ show c
        hlintOpts = [file, "--quiet", "--refactor", "--refactor-options=" ++ opts ]
    runEitherT $ do
      ideas <- runHlint file hlintOpts
      liftIO $ logm $ "applyHint:ideas=" ++ show ideas
      let commands = map (show &&& ideaRefactoring) ideas
      appliedFile <- liftIO $ applyRefactorings (fmap unPos mpos) commands file
      diff <- liftIO $ makeDiffResult file (T.pack appliedFile)
      liftIO $ logm $ "applyHint:diff=" ++ show diff
      return diff


runHlint :: FilePath -> [String] -> EitherT String IO [Idea]
runHlint file args =
  do (flags,classify,hint) <- liftIO $ argsSettings args
     res <- bimapEitherT showParseError id $ EitherT $ parseModuleEx flags file Nothing
     pure $ applyHints classify hint [res]

showParseError :: Hlint.ParseError -> String
showParseError (Hlint.ParseError loc message content) = unlines [show loc, message, content]

makeDiffResult :: FilePath -> T.Text -> IO HieDiff
makeDiffResult orig new = do
  origText <- T.readFile orig
  let (HieDiff f _ d) = diffText (orig,origText) ("changed",new)
  f' <- liftIO $ makeRelativeToCurrentDirectory f
  -- return (HieDiff f' s' d)
  return (HieDiff f' "changed" d)
