{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Haskell.Ide.HaRePlugin where

import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.Aeson
import           Data.Monoid
import qualified Data.Text as T
import           Exception
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import qualified Language.Haskell.GhcMod.Monad as GM
import qualified Language.Haskell.GhcMod.Error as GM
import           Language.Haskell.Refact.HaRe
import           Language.Haskell.Refact.Utils.Monad
import           Language.Haskell.Refact.Utils.Types
import           Language.Haskell.Refact.Utils.Utils
import           System.FilePath

-- ---------------------------------------------------------------------

hareDescriptor :: TaggedPluginDescriptor _
hareDescriptor = PluginDescriptor
  {
    pdUIShortName = "HaRe"
  , pdUIOverview = "A Haskell 2010 refactoring tool. HaRe supports the full "
              <> "Haskell 2010 standard, through making use of the GHC API.  HaRe attempts to "
              <> "operate in a safe way, by first writing new files with proposed changes, and "
              <> "only swapping these with the originals when the change is accepted. "
    , pdCommands =
        buildCommand demoteCmd (Proxy :: Proxy "demote") "Move a definition one level down"
                    [".hs"] (SCtxPoint :& RNil) RNil SaveAll

      :& buildCommand dupdefCmd (Proxy :: Proxy "dupdef") "Duplicate a definition"
                     [".hs"] (SCtxPoint :& RNil)
                     (  SParamDesc (Proxy :: Proxy "name") (Proxy :: Proxy "the new name") SPtText SRequired
                     :& RNil) SaveAll

      :& buildCommand iftocaseCmd (Proxy :: Proxy "iftocase") "Converts an if statement to a case statement"
                     [".hs"] (SCtxRegion :& RNil) RNil SaveAll

      :& buildCommand liftonelevelCmd (Proxy :: Proxy "liftonelevel") "Move a definition one level up from where it is now"
                     [".hs"] (SCtxPoint :& RNil) RNil SaveAll

      :& buildCommand lifttotoplevelCmd (Proxy :: Proxy "lifttotoplevel") "Move a definition to the top level from where it is now"
                     [".hs"] (SCtxPoint :& RNil) RNil SaveAll

      :& buildCommand renameCmd (Proxy :: Proxy "rename") "rename a variable or type"
                     [".hs"] (SCtxPoint :& RNil)
                     (  SParamDesc (Proxy :: Proxy "name") (Proxy :: Proxy "the new name") SPtText SRequired
                     :& RNil) SaveAll

      :& RNil
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

demoteCmd :: CommandFunc RefactorResult
demoteCmd  = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& RNil) ->
      runHareCommand "demote" (compDemote (T.unpack fileName) (unPos pos))

-- compDemote :: FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

dupdefCmd :: CommandFunc RefactorResult
dupdefCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& IdText "name" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& ParamText name :& RNil) ->
      runHareCommand "dupdef" (compDuplicateDef (T.unpack fileName) (T.unpack name) (unPos pos))

-- compDuplicateDef :: FilePath -> String -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

iftocaseCmd :: CommandFunc RefactorResult
iftocaseCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& IdPos "end_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos startPos :& ParamPos endPos :& RNil) ->
      runHareCommand "iftocase" (compIfToCase (T.unpack fileName) (unPos startPos) (unPos endPos))

-- compIfToCase :: FilePath -> SimpPos -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

liftonelevelCmd :: CommandFunc RefactorResult
liftonelevelCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& RNil) ->
      runHareCommand "liftonelevel" (compLiftOneLevel (T.unpack fileName) (unPos pos))

-- compLiftOneLevel :: FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

lifttotoplevelCmd :: CommandFunc RefactorResult
lifttotoplevelCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& RNil) ->
      runHareCommand "lifttotoplevel" (compLiftToTopLevel (T.unpack fileName) (unPos pos))

-- compLiftToTopLevel :: FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

renameCmd :: CommandFunc RefactorResult
renameCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& IdText "name" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos pos :& ParamText name :& RNil) ->
      runHareCommand "rename" (compRename (T.unpack fileName) (T.unpack name) (unPos pos))

-- compRename :: FilePath -> String -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

makeRefactorResult :: [FilePath] -> IO RefactorResult
makeRefactorResult changedFiles = do
  let
    diffOne f1 = do
      let (baseFileName,ext) = splitExtension f1
          f2 = (baseFileName ++ ".refactored" ++ ext)
      diffFiles f1 f2
  diffs <- mapM diffOne changedFiles
  return (RefactorResult diffs)

-- ---------------------------------------------------------------------


runHareCommand :: String -> RefactGhc [ApplyRefacResult]
                 -> IdeM (IdeResponse RefactorResult)
runHareCommand name cmd =
  do let initialState =
           RefSt {rsSettings = defaultSettings
                 ,rsUniqState = 1
                 ,rsSrcSpanCol = 1
                 ,rsFlags = RefFlags False
                 ,rsStorage = StorageNone
                 ,rsCurrentTarget = Nothing
                 ,rsModule = Nothing}
     let cmd' = unRefactGhc cmd
         embeddedCmd =
           GM.unGmlT $
           hoist (liftIO . flip evalStateT initialState)
                 (GM.GmlT cmd')
         handlers
           :: Applicative m
           => [GM.GHandler m (Either String a)]
         handlers =
           [GM.GHandler (\(ErrorCall e) -> pure (Left e))
           ,GM.GHandler (\(err :: GM.GhcModError) -> pure (Left (show err)))]
     eitherRes <- fmap Right embeddedCmd `GM.gcatches` handlers
     case eitherRes of
       Left err ->
         pure (IdeResponseFail
                 (IdeError PluginError
                           (T.pack $ name <> ": \"" <> err <> "\"")
                           Null))
       Right res ->
         do liftIO $
              writeRefactoredFiles (rsetVerboseLevel defaultSettings)
                                   res
            let files = modifiedFiles res
            refactRes <- liftIO $ makeRefactorResult files
            pure (IdeResponseOk refactRes)

-- | This is like hoist from the mmorph package, but build on
-- `MonadTransControl` since we don’t have an `MFunctor` instance.
hoist
  :: (MonadTransControl t,Monad (t m'),Monad m',Monad m)
  => (forall b. m b -> m' b) -> t m a -> t m' a
hoist f a =
  liftWith (\run ->
              let b = run a
                  c = f b
              in pure c) >>=
  restoreT
