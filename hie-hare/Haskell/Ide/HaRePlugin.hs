{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Haskell.Ide.HaRePlugin where

import           Control.Lens                          ((^.))
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.Aeson
import           Data.Either
import           Data.Foldable
import           Data.Monoid
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as T
import           Exception
import           FastString
import           GHC
import qualified GhcMod.Error                          as GM
import qualified GhcMod.Monad                          as GM
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import           Language.Haskell.GHC.ExactPrint.Print
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import           Language.Haskell.Refact.API
import           Language.Haskell.Refact.HaRe
import           Language.Haskell.Refact.Utils.Monad
import           Name
import           SrcLoc
import           System.Directory

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

      :& buildCommand deleteDefCmd (Proxy :: Proxy "deletedef") "Delete a definition"
                    [".hs"] (SCtxPoint :& RNil) RNil SaveAll

      :& buildCommand genApplicativeCommand (Proxy :: Proxy "genapplicative") "Generalise a monadic function to use applicative"
                    [".hs"] (SCtxPoint :& RNil) RNil SaveAll

      :& RNil
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

demoteCmd :: CommandFunc WorkspaceEdit
demoteCmd  = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile uri :& ParamPos pos :& RNil) ->
      demoteCmd' (TextDocumentPositionParams (TextDocumentIdentifier uri) pos)

demoteCmd' :: TextDocumentPositionParams -> IdeM (IdeResponse WorkspaceEdit)
demoteCmd' (TextDocumentPositionParams tdi pos) =
  pluginGetFile "demote: " (tdi ^. J.uri) $ \file -> do
    runHareCommand "demote" (compDemote file (unPos pos))

-- compDemote :: FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

dupdefCmd :: CommandFunc WorkspaceEdit
dupdefCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& IdText "name" :& RNil) req of
    Left err -> return err
    Right (ParamFile uri :& ParamPos pos :& ParamText name :& RNil) ->
      dupdefCmd' (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) name

dupdefCmd' :: TextDocumentPositionParams -> T.Text -> IdeM (IdeResponse WorkspaceEdit)
dupdefCmd' (TextDocumentPositionParams tdi pos) name =
  pluginGetFile "dupdef: " (tdi ^. J.uri) $ \file -> do
    runHareCommand  "dupdef" (compDuplicateDef file (T.unpack name) (unPos pos))

-- compDuplicateDef :: FilePath -> String -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

iftocaseCmd :: CommandFunc WorkspaceEdit
iftocaseCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& IdPos "end_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile uri :& ParamPos startPos :& ParamPos endPos :& RNil) ->
      iftocaseCmd' (Location uri (Range startPos endPos))

iftocaseCmd' :: Location -> IdeM (IdeResponse WorkspaceEdit)
iftocaseCmd' (Location uri (Range startPos endPos)) =
  pluginGetFile "iftocase: " uri $ \file -> do
    runHareCommand "iftocase" (compIfToCase file (unPos startPos) (unPos endPos))

-- compIfToCase :: FilePath -> SimpPos -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

liftonelevelCmd :: CommandFunc WorkspaceEdit
liftonelevelCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile uri :& ParamPos pos :& RNil) ->
      liftonelevelCmd' (TextDocumentPositionParams (TextDocumentIdentifier uri) pos)

liftonelevelCmd' :: TextDocumentPositionParams -> IdeM (IdeResponse WorkspaceEdit)
liftonelevelCmd' (TextDocumentPositionParams tdi pos) =
  pluginGetFile "liftonelevelCmd: " (tdi ^. J.uri) $ \file -> do
    runHareCommand "liftonelevel" (compLiftOneLevel file (unPos pos))

-- compLiftOneLevel :: FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

lifttotoplevelCmd :: CommandFunc WorkspaceEdit
lifttotoplevelCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile uri :& ParamPos pos :& RNil) ->
      lifttotoplevelCmd' (TextDocumentPositionParams (TextDocumentIdentifier uri) pos)

lifttotoplevelCmd' :: TextDocumentPositionParams -> IdeM (IdeResponse WorkspaceEdit)
lifttotoplevelCmd' (TextDocumentPositionParams tdi pos) =
  pluginGetFile "lifttotoplevelCmd: " (tdi ^. J.uri) $ \file -> do
    runHareCommand "lifttotoplevel" (compLiftToTopLevel file (unPos pos))

-- compLiftToTopLevel :: FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

renameCmd :: CommandFunc WorkspaceEdit
renameCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& IdText "name" :& RNil) req of
    Left err -> return err
    Right (ParamFile uri :& ParamPos pos :& ParamText name :& RNil) ->
      renameCmd' (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) name

renameCmd' :: TextDocumentPositionParams -> T.Text -> IdeM (IdeResponse WorkspaceEdit)
renameCmd' (TextDocumentPositionParams tdi pos) name =
  pluginGetFile "rename: " (tdi ^. J.uri) $ \file -> do
      runHareCommand "rename" (compRename file (T.unpack name) (unPos pos))

-- compRename :: FilePath -> String -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

deleteDefCmd :: CommandFunc WorkspaceEdit
deleteDefCmd  = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile uri :& ParamPos pos :& RNil) ->
      deleteDefCmd' (TextDocumentPositionParams (TextDocumentIdentifier uri) pos)

deleteDefCmd' :: TextDocumentPositionParams -> IdeM (IdeResponse WorkspaceEdit)
deleteDefCmd' (TextDocumentPositionParams tdi pos) =
  pluginGetFile "deletedef: " (tdi ^. J.uri) $ \file -> do
      runHareCommand "deltetedef" (compDeleteDef file (unPos pos))

-- compDeleteDef ::FilePath -> SimpPos -> RefactGhc [ApplyRefacResult]

-- ---------------------------------------------------------------------

genApplicativeCommand :: CommandFunc WorkspaceEdit
genApplicativeCommand  = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile uri :& ParamPos pos :& RNil) ->
      genApplicativeCommand' (TextDocumentPositionParams (TextDocumentIdentifier uri) pos)

genApplicativeCommand' :: TextDocumentPositionParams -> IdeM (IdeResponse WorkspaceEdit)
genApplicativeCommand' (TextDocumentPositionParams tdi pos) =
  pluginGetFile "genapplicative: " (tdi ^. J.uri) $ \file -> do
      runHareCommand "genapplicative" (compGenApplicative file (unPos pos))


-- ---------------------------------------------------------------------

getRefactorResult :: [ApplyRefacResult] -> [(FilePath,T.Text)]
getRefactorResult = map getNewFile . filter fileModified
  where fileModified ((_,m),_) = m == RefacModified
        getNewFile ((file,_),(ann, parsed)) = (file, T.pack $ exactPrint parsed ann)

makeRefactorResult :: [(FilePath,T.Text)] -> IO WorkspaceEdit
makeRefactorResult changedFiles = do
  let
    diffOne (fp, newText) = do
      origText <- T.readFile fp
      return $ diffText (filePathToUri fp, origText) newText
  diffs <- mapM diffOne changedFiles
  return $ fold diffs

-- ---------------------------------------------------------------------

findDefCmd :: TextDocumentPositionParams -> IdeM (IdeResponse Location)
findDefCmd (TextDocumentPositionParams tdi pos) =
  pluginGetFile "findDef: " (tdi ^. J.uri) $ \file -> do
    eitherRes <- runHareCommand' $ findDef file (unPos pos)
    case eitherRes of
      Right x -> return x
      Left err ->
         pure (IdeResponseFail
                 (IdeError PluginError
                           (T.pack $ "hare:findDefCmd" <> ": \"" <> err <> "\"")
                           Null))

findDef :: FilePath -> (Int,Int) -> RefactGhc (IdeResponse Location)
findDef fileName (row, col) = do
  parseSourceFileGhc fileName
  parsed <- getRefactParsed
  case locToRdrName (row, col) parsed of
    Just pn -> do
      n <- rdrName2Name pn
      res <- srcLoc2Loc $ nameSrcSpan n
      case res of
        Right l -> return $ IdeResponseOk l
        Left x -> do
          let failure = pure (IdeResponseFail
                                (IdeError PluginError
                                          (T.pack $ "hare:findDef" <> ": \"" <> x <> "\"")
                                          Null))
          case nameModule_maybe n of
            Just m -> do
              let mName = moduleName m
              b <- isLoaded mName
              if b then do
                mLoc <- ms_location <$> getModSummary mName
                case ml_hs_file mLoc of
                  Just fp -> do
                    parseSourceFileGhc fp
                    newNames <- equivalentNameInNewMod n
                    eithers <- mapM (srcLoc2Loc . nameSrcSpan) newNames
                    case rights eithers of
                      (l:_) -> return $ IdeResponseOk l
                      []    -> failure
                  Nothing -> failure
                else failure
            Nothing -> failure
    Nothing ->
          pure (IdeResponseFail
                 (IdeError PluginError
                           (T.pack $ "hare:findDef" <> ": \"" <> "Invalid cursor position" <> "\"")
                           Null))

srcLoc2Loc :: MonadIO m => SrcSpan -> m (Either String Location)
srcLoc2Loc (RealSrcSpan r) = do
  file <- liftIO $ makeAbsolute $ unpackFS $ srcSpanFile r
  return $ Right $ Location (filePathToUri file) $ Range (toPos (l1,c1)) (toPos (l2,c2))
  where s = realSrcSpanStart r
        l1 = srcLocLine s
        c1 = srcLocCol s
        e = realSrcSpanEnd r
        l2 = srcLocLine e
        c2 = srcLocCol e
srcLoc2Loc (UnhelpfulSpan x) = return $ Left $ unpackFS x

-- ---------------------------------------------------------------------


runHareCommand :: String -> RefactGhc [ApplyRefacResult]
                 -> IdeM (IdeResponse WorkspaceEdit)
runHareCommand name cmd = do
     eitherRes <- runHareCommand' cmd
     case eitherRes of
       Left err ->
         pure (IdeResponseFail
                 (IdeError PluginError
                           (T.pack $ name <> ": \"" <> err <> "\"")
                           Null))
       Right res -> do
            let changes = getRefactorResult res
            refactRes <- liftIO $ makeRefactorResult changes
            pure (IdeResponseOk refactRes)

-- ---------------------------------------------------------------------

runHareCommand' :: RefactGhc a
                 -> IdeM (Either String a)
runHareCommand' cmd =
  do let initialState =
           RefSt {rsSettings = defaultSettings
           -- RefSt {rsSettings = logSettings
                 ,rsUniqState = 1
                 ,rsSrcSpanCol = 1
                 ,rsFlags = RefFlags False
                 ,rsStorage = StorageNone
                 ,rsCurrentTarget = Nothing
                 ,rsModule = Nothing
                 ,rsHookIORef = Nothing}
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
     fmap Right embeddedCmd `GM.gcatches` handlers

-- ---------------------------------------------------------------------
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
