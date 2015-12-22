{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.HaRePlugin where

import           Control.Monad.IO.Class
import qualified Data.Text as T
import           Data.Vinyl
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import           Language.Haskell.Refact.HaRe


import qualified Language.Haskell.GhcMod as GM
import qualified Language.Haskell.GhcMod.Monad as GM
import           System.FilePath
import           System.Directory
import qualified Exception as G

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
demoteCmd  = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& RNil) ->
      runHareCommand fileName "demote" (\s o f -> demote s o f pos)
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.demoteCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- demote :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

dupdefCmd :: CommandFunc RefactorResult
dupdefCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& IdText "name" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& ParamText name :& RNil) ->
      runHareCommand fileName "duplicateDef" (\s o f -> duplicateDef s o f (T.unpack name) pos)
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.dupdefCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- duplicateDef :: RefactSettings -> GM.Options -> FilePath -> String -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

iftocaseCmd :: CommandFunc RefactorResult
iftocaseCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& IdPos "end_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos start :& ParamPos end :& RNil) ->
      runHareCommand fileName "ifToCase" (\s o f -> ifToCase s o f start end)
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.ifToCaseCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- ifToCase :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

liftonelevelCmd :: CommandFunc RefactorResult
liftonelevelCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& RNil) ->
      runHareCommand fileName "liftOneLevel" (\s o f -> liftOneLevel s o f pos)
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.liftOneLevel: ghc’s exhaustiveness checker is broken" Nothing)

-- liftOneLevel :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

lifttotoplevelCmd :: CommandFunc RefactorResult
lifttotoplevelCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& RNil) ->
      runHareCommand fileName "liftToTopLevel" (\s o f -> liftToTopLevel s o f pos)
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.liftToTopLevel: ghc’s exhaustiveness checker is broken" Nothing)

-- liftToTopLevel :: RefactSettings -> GM.Options -> FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

renameCmd :: CommandFunc RefactorResult
renameCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& IdText "name" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& ParamText name :& RNil) ->
      runHareCommand fileName "rename" (\s o f -> rename s o f (T.unpack name) pos)
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "HaRePlugin.renameCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- rename :: RefactSettings -> Options -> FilePath -> String -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

makeRefactorResult :: [FilePath] -> IO RefactorResult
makeRefactorResult changedFiles = do
  let
    diffOne f1 = do
      let (baseFileName,ext) = splitExtension f1
          f2 = (baseFileName ++ ".refactored" ++ ext)
      (HieDiff f s d) <- diffFiles f1 f2
      f' <- liftIO $ makeRelativeToCurrentDirectory f
      s' <- liftIO $ makeRelativeToCurrentDirectory s
      return (HieDiff f s d)
  diffs <- mapM diffOne changedFiles
  return (RefactorResult diffs)

-- ---------------------------------------------------------------------


runHareCommand :: T.Text -- ^ The file name we'll operate on
                 -> String -- ^ command name for log
                 -> (RefactSettings -> GM.Options -> FilePath -> IO [FilePath])
                 -> IdeM (IdeResponse RefactorResult)
runHareCommand fp name cmd = do
  let (dir,_) = fileInfo fp
  let opts = GM.defaultOptions
  old <- liftIO getCurrentDirectory
  G.gbracket (liftIO $ setCurrentDirectory dir)
             (\_ -> liftIO $ setCurrentDirectory old)
             (\_ -> do
                -- we need to get the root of our folder
                -- ghc-mod returns a new line at the end...
                root <- takeWhile (`notElem` ['\r','\n']) <$> GM.runGmOutT opts GM.rootInfo
                liftIO $ setCurrentDirectory root
                res <- liftIO $ catchException $ cmd defaultSettings GM.defaultOptions (T.unpack fp)
                liftIO $ setCurrentDirectory old
                case res of
                  Left err -> return $ IdeResponseFail (IdeError PluginError
                                (T.pack $ name ++ ": " ++ show err) Nothing)
                  Right fs -> do
                    r <- liftIO $ makeRefactorResult fs
                    return (IdeResponseOk r)
              )
