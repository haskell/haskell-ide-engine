{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.HaRePlugin where

import           Control.Exception
import           Control.Monad.IO.Class
import qualified Data.Text as T
import           Data.Vinyl
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import qualified Language.Haskell.GhcMod as GM (defaultOptions)
import           Language.Haskell.Refact.HaRe
import           System.Directory

-- ---------------------------------------------------------------------

hareDescriptor :: PluginDescriptor
hareDescriptor = PluginDescriptor
  {
    pdUIShortName = "HaRe"
  , pdUIOverview = "A Haskell 2010 refactoring tool. HaRe supports the full \
\Haskell 2010 standard, through making use of the GHC API.  HaRe attempts to \
\operate in a safe way, by first writing new files with proposed changes, and \
\only swapping these with the originals when the change is accepted. "
    , pdCommands =
      [
        buildCommand demoteCmd "demote" "Move a definition one level down"
                    [".hs"] [CtxPoint] []

      , buildCommand dupdefCmd "dupdef" "Duplicate a definition"
                     [".hs"] [CtxPoint] [RP "name" "the new name" PtText]

      , buildCommand iftocaseCmd "iftocase" "Converts an if statement to a case statement"
                     [".hs"] [CtxRegion] []

      , buildCommand liftonelevelCmd "liftonelevel" "Move a definition one level up from where it is now"
                     [".hs"] [CtxPoint] []

      , buildCommand lifttotoplevelCmd "lifttotoplevel" "Move a definition to the top level from where it is now"
                     [".hs"] [CtxPoint] []

      , buildCommand renameCmd "rename" "rename a variable or type"
                     [".hs"] [CtxPoint] [RP "name" "the new name" PtText]

      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

demoteCmd :: CommandFunc RefactorResult
demoteCmd  = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& RNil) -> do
      res <- liftIO $ catchException $ demote defaultSettings GM.defaultOptions (T.unpack fileName) pos
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "demote: " ++ show err) Nothing)
        Right fs -> do
          fs' <- liftIO $ mapM makeRelativeToCurrentDirectory fs
          return (IdeResponseOk $ RefactorResult fs')
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.demoteCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- demote :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

dupdefCmd :: CommandFunc RefactorResult
dupdefCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& IdPos "start_pos" :& IdText "name" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& ParamText name :& RNil) -> do
      res <- liftIO $ catchException $ duplicateDef defaultSettings GM.defaultOptions (T.unpack fileName) (T.unpack name) pos
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "dupdef: " ++ show err) Nothing)
        Right fs -> do
          fs' <- liftIO $ mapM makeRelativeToCurrentDirectory fs
          return (IdeResponseOk $ RefactorResult fs')
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.dupdefCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- duplicateDef :: RefactSettings -> GM.Options -> FilePath -> String -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

iftocaseCmd :: CommandFunc RefactorResult
iftocaseCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& IdPos "start_pos" :& IdPos "end_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos start :& ParamPos end :& RNil) -> do
      res <- liftIO $ catchException $ ifToCase defaultSettings GM.defaultOptions (T.unpack fileName) start end
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "ifToCase: " ++ show err) Nothing)
        Right fs -> do
          fs' <- liftIO $ mapM makeRelativeToCurrentDirectory fs
          return (IdeResponseOk $ RefactorResult fs')
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.ifToCaseCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- ifToCase :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

liftonelevelCmd :: CommandFunc RefactorResult
liftonelevelCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& RNil) -> do
      res <- liftIO $ catchException $ liftOneLevel defaultSettings GM.defaultOptions (T.unpack fileName) pos
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "liftOneLevel: " ++ show err) Nothing)
        Right fs -> do
          fs' <- liftIO $ mapM makeRelativeToCurrentDirectory fs
          return (IdeResponseOk $ RefactorResult fs')
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.liftOneLevel: ghc’s exhaustiveness checker is broken" Nothing)

-- liftOneLevel :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

lifttotoplevelCmd :: CommandFunc RefactorResult
lifttotoplevelCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& RNil) -> do
      res <- liftIO $ catchException $ liftToTopLevel defaultSettings GM.defaultOptions (T.unpack fileName) pos
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "liftToTopLevel: " ++ show err) Nothing)
        Right fs -> do
          fs' <- liftIO $ mapM makeRelativeToCurrentDirectory fs
          return (IdeResponseOk $ RefactorResult fs')
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.liftToTopLevel: ghc’s exhaustiveness checker is broken" Nothing)

-- liftToTopLevel :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

renameCmd :: CommandFunc RefactorResult
renameCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& IdPos "start_pos" :& IdText "name" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& ParamText name :& RNil) -> do
      res <- liftIO $ catchException $ rename defaultSettings GM.defaultOptions (T.unpack fileName) (T.unpack name) pos
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "rename: " ++ show err) Nothing)
        Right fs -> do
          fs' <- liftIO $ mapM makeRelativeToCurrentDirectory fs
          return (IdeResponseOk $ RefactorResult fs')
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.renameCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- rename :: RefactSettings -> Options -> FilePath -> String -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

catchException :: (IO t) -> IO (Either String t)
catchException f = do
  res <- handle handler (f >>= \r -> return $ Right r)
  return res
  where
    handler:: SomeException -> IO (Either String t)
    handler e = return (Left (show e))
