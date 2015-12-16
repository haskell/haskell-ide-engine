{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Haskell.Ide.GhcModPlugin where

import           Control.Exception
import           Data.Either
import           Data.Vinyl
import           Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import qualified Language.Haskell.GhcMod as GM
import qualified Language.Haskell.GhcMod.Monad as GM
import qualified Language.Haskell.GhcMod.Types as GM
import qualified Language.Haskell.GhcMod.Utils as GM
import           System.FilePath
import           System.Directory
import qualified Exception as G

-- ---------------------------------------------------------------------

ghcmodDescriptor :: PluginDescriptor
ghcmodDescriptor = PluginDescriptor
  {
    pdUIShortName = "ghc-mod"
  , pdUIOverview = "ghc-mod is a backend program to enrich Haskell programming \
\in editors. It strives to offer most of the features one has come to expect \
\from modern IDEs in any editor."
  , pdCommands =
      [
        buildCommand checkCmd "check" "check a file for GHC warnings and errors"
                     [".hs",".lhs"] [CtxFile] []

      , buildCommand lintCmd "lint" "Check files using `hlint'"
                     [".hs",".lhs"] [CtxFile] []

      , buildCommand findCmd "find" "List all modules that define SYMBOL"
                     [".hs",".lhs"] [CtxProject] [RP "symbol" "The SYMBOL to look up" PtText]

      , buildCommand infoCmd "info" "Look up an identifier in the context of FILE (like ghci's `:info')"
                     [".hs",".lhs"] [CtxFile] [RP "expr" "The EXPR to provide info on" PtText]

      , buildCommand typeCmd "type" "Get the type of the expression under (LINE,COL)"
                     [".hs",".lhs"] [CtxPoint] []

      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }
{-
        "check"  -> checkSyntaxCmd [arg]
        "lint"   -> lintCmd [arg]
        "find"    -> do
            db <- getDb symdbreq >>= checkDb symdbreq
            lookupSymbol arg db

        "info"   -> infoCmd [head args, concat $ tail args']
        "type"   -> typesCmd args
        "split"  -> splitsCmd args

        "sig"    -> sigCmd args
        "auto"   -> autoCmd args
        "refine" -> refineCmd args

        "boot"   -> bootCmd []
        "browse" -> browseCmd args

-}

-- ---------------------------------------------------------------------

checkCmd :: CommandFunc T.Text
checkCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& RNil) -> do
      fmap T.pack <$> runGhcModCommand fileName (\f->GM.checkSyntax [f])
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcModPlugin.checkCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- ---------------------------------------------------------------------

-- | Runs the find command from the given directory, for the given symbol
findCmd :: CommandFunc ModuleList
findCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "dir" :& IdText "symbol" :& RNil) req of
    Left err -> return err
    Right (ParamFile dirName :& ParamText symbol :& RNil) -> do
      runGhcModCommand (T.pack (T.unpack dirName </> "dummy")) (\_->
          do
            -- adapted from ghc-mod find command, which launches the executable again
            tmpdir <- GM.cradleTempDir <$> GM.cradle
            sf <- takeWhile (`notElem` ['\r','\n']) <$> GM.dumpSymbol tmpdir
            db <- M.fromAscList . map conv . lines <$> liftIO (readFile sf)
            let f = M.findWithDefault ([]::[GM.ModuleString]) symbol db
            return $ ModuleList $ map (T.pack . GM.getModuleString) f
          )

      -- return (IdeResponseOk "Placholder:Need to debug this in ghc-mod, returns 'does not exist (No such file or directory)'")
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcModPlugin.findCmd: ghc’s exhaustiveness checker is broken" Nothing)
  where
    conv :: String -> (T.Text, [GM.ModuleString])
    conv = read

-- ---------------------------------------------------------------------

lintCmd :: CommandFunc T.Text
lintCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& RNil) -> do
      fmap T.pack <$> runGhcModCommand fileName (GM.lint GM.defaultLintOpts)
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcModPlugin.lintCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- ---------------------------------------------------------------------

infoCmd :: CommandFunc T.Text
infoCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& IdText "expr" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamText expr :& RNil) -> do
      fmap T.pack <$> runGhcModCommand fileName (flip GM.info (GM.Expression (T.unpack expr)))
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcModPlugin.infoCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- ---------------------------------------------------------------------

typeCmd :: CommandFunc TypeInfo
typeCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos (r,c) :& RNil) -> do
      fmap (toTypeInfo . T.lines . T.pack) <$> runGhcModCommand fileName (\f->GM.types f r c)
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcModPlugin.typesCmd: ghc’s exhaustiveness checker is broken" Nothing)


-- | Transform output from ghc-mod type into TypeInfo
toTypeInfo :: [T.Text] -> TypeInfo
toTypeInfo = TypeInfo . rights . map readTypeResult

-- | Parse one type result
readTypeResult :: T.Text -> Either String TypeResult
readTypeResult t = do
    (sl,r0) <- T.decimal t
    (sc,r1) <- T.decimal $ T.stripStart r0
    (el,r2) <- T.decimal $ T.stripStart r1
    (ec,r3) <- T.decimal $ T.stripStart r2
    let typ = T.dropEnd 1 $ T.drop 1 $ T.stripStart r3
    return $ TypeResult (sl,sc) (el,ec) typ

-- ---------------------------------------------------------------------


-- TODO: Need to thread the session through as in the commented out code below.
runGhcModCommand :: T.Text -- ^ The file name we'll operate on
                 -> (FilePath -> IdeM a)
                 -> IdeM (IdeResponse a)
runGhcModCommand fp cmd = do
  let (dir,f) = fileInfo fp
  let opts = GM.defaultOptions
  old <- liftIO getCurrentDirectory
  G.gbracket (liftIO $ setCurrentDirectory dir)
          (\_ -> liftIO $ setCurrentDirectory old)
          (\_ -> do
            -- we need to get the root of our folder
            -- ghc-mod returns a new line at the end...
            root <- takeWhile (`notElem` ['\r','\n']) <$> GM.runGmOutT opts GM.rootInfo
            liftIO $ setCurrentDirectory root
            tmp <- liftIO $ GM.newTempDir root
            let setRoot e = e{GM.gmCradle = (GM.gmCradle e){GM.cradleRootDir=root,GM.cradleTempDir=tmp}}
            (IdeResponseOk <$> GM.gmeLocal setRoot (cmd f)) `G.gcatch` \(e :: GM.GhcModError) ->
               return $ IdeResponseFail $ IdeError PluginError (T.pack $ "hie-ghc-mod: " ++ show e) Nothing
          )

-- ---------------------------------------------------------------------

-- | Returns the directory and file name
fileInfo :: T.Text -> (FilePath,FilePath)
fileInfo tfileName =
  let sfileName = T.unpack tfileName
      dir = takeDirectory sfileName
  in (dir,sfileName)

catchException :: (IO t) -> IO (Either String t)
catchException f = do
  res <- handle handler (f >>= \r -> return $ Right r)
  return res
  where
    handler:: SomeException -> IO (Either String t)
    handler e = return (Left (show e))
