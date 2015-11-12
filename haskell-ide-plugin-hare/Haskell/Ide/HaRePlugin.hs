{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.HaRePlugin where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Aeson
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
    pdCommands =
      [
        Command
          { cmdDesc = CommandDesc
                     { cmdName = "demote"
                     , cmdUiDescription = "Move a definition one level down"
                     , cmdFileExtensions = [".hs"]
                     , cmdContexts = [CtxPoint]
                     , cmdAdditionalParams = []
                     }
          , cmdFunc = demoteCmd
          }
      , Command
          { cmdDesc = CommandDesc
                     { cmdName = "dupdef"
                     , cmdUiDescription = "Duplicate a definition"
                     , cmdFileExtensions = [".hs"]
                     , cmdContexts = [CtxPoint]
                     , cmdAdditionalParams = [RP "name" "the new name" PtText]
                     }
          , cmdFunc = dupdefCmd
          }
      , Command
          { cmdDesc = CommandDesc
                     { cmdName = "iftocase"
                     , cmdUiDescription = "Converts an if statement to a case statement"
                     , cmdFileExtensions = [".hs"]
                     , cmdContexts = [CtxRegion]
                     , cmdAdditionalParams = []
                     }
          , cmdFunc = ifToCaseCmd
          }
      , Command
          { cmdDesc = CommandDesc
                     { cmdName = "liftOneLevel"
                     , cmdUiDescription = "Move a definition one level up from where it is now"
                     , cmdFileExtensions = [".hs"]
                     , cmdContexts = [CtxPoint]
                     , cmdAdditionalParams = []
                     }
          , cmdFunc = liftOneLevelCmd
          }
      , Command
          { cmdDesc = CommandDesc
                     { cmdName = "liftToTopLevel"
                     , cmdUiDescription = "Move a definition to the top level from where it is now"
                     , cmdFileExtensions = [".hs"]
                     , cmdContexts = [CtxPoint]
                     , cmdAdditionalParams = []
                     }
          , cmdFunc = liftToTopLevelCmd
          }
      , Command
          { cmdDesc = CommandDesc
                     { cmdName = "rename"
                     , cmdUiDescription = "rename a variable or type"
                     , cmdFileExtensions = [".hs"]
                     , cmdContexts = [CtxPoint]
                     , cmdAdditionalParams = [RP "name" "the new name" PtText]
                     }
          , cmdFunc = renameCmd
          }
      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

demoteCmd :: CommandFunc
demoteCmd _ctxs req = do
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& RNil) -> do
      res <- liftIO $ catchException $ demote defaultSettings GM.defaultOptions (T.unpack fileName) pos
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "demote: " ++ show err) Nothing)
        Right fs -> do
          fs' <- liftIO $ mapM makeRelativeToCurrentDirectory fs
          return (IdeResponseOk (toJSON fs'))
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.demoteCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- demote :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

dupdefCmd :: CommandFunc
dupdefCmd _ctxs req = do
  case getParams (IdFile "file" :& IdPos "start_pos" :& IdText "name" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& ParamText name :& RNil) -> do
      res <- liftIO $ catchException $ duplicateDef defaultSettings GM.defaultOptions (T.unpack fileName) (T.unpack name) pos
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "dupdef: " ++ show err) Nothing)
        Right fs -> do
          fs' <- liftIO $ mapM makeRelativeToCurrentDirectory fs
          return (IdeResponseOk (toJSON fs'))
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.dupdefCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- duplicateDef :: RefactSettings -> GM.Options -> FilePath -> String -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

ifToCaseCmd :: CommandFunc
ifToCaseCmd _ctxs req = do
  case getParams (IdFile "file" :& IdPos "start_pos" :& IdPos "end_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos start :& ParamPos end :& RNil) -> do
      res <- liftIO $ catchException $ ifToCase defaultSettings GM.defaultOptions (T.unpack fileName) start end
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "ifToCase: " ++ show err) Nothing)
        Right fs -> do
          fs' <- liftIO $ mapM makeRelativeToCurrentDirectory fs
          return (IdeResponseOk (toJSON fs'))
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.ifToCaseCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- ifToCase :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

liftOneLevelCmd :: CommandFunc
liftOneLevelCmd _ctxs req = do
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& RNil) -> do
      res <- liftIO $ catchException $ liftOneLevel defaultSettings GM.defaultOptions (T.unpack fileName) pos
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "liftOneLevel: " ++ show err) Nothing)
        Right fs -> do
          fs' <- liftIO $ mapM makeRelativeToCurrentDirectory fs
          return (IdeResponseOk (toJSON fs'))
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.liftOneLevel: ghc’s exhaustiveness checker is broken" Nothing)

-- liftOneLevel :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

liftToTopLevelCmd :: CommandFunc
liftToTopLevelCmd _ctxs req = do
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& RNil) -> do
      res <- liftIO $ catchException $ liftToTopLevel defaultSettings GM.defaultOptions (T.unpack fileName) pos
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "liftToTopLevel: " ++ show err) Nothing)
        Right fs -> do
          fs' <- liftIO $ mapM makeRelativeToCurrentDirectory fs
          return (IdeResponseOk (toJSON fs'))
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.liftToTopLevel: ghc’s exhaustiveness checker is broken" Nothing)

-- liftToTopLevel :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

renameCmd :: CommandFunc
renameCmd _ctxs req = do
  case getParams (IdFile "file" :& IdPos "start_pos" :& IdText "name" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& ParamText name :& RNil) -> do
      res <- liftIO $ catchException $ rename defaultSettings GM.defaultOptions (T.unpack fileName) (T.unpack name) pos
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "rename: " ++ show err) Nothing)
        Right fs -> do
          fs' <- liftIO $ mapM makeRelativeToCurrentDirectory fs
          return (IdeResponseOk (toJSON fs'))
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
