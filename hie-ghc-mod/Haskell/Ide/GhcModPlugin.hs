{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.GhcModPlugin where

import           Control.Exception
import           Data.Char
import           Data.Either
import           Data.Vinyl
-- import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import qualified Language.Haskell.GhcMod as GM
import qualified Language.Haskell.GhcMod.Monad as GM
import           System.FilePath
import           System.Directory

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
        Command
          { cmdDesc = CommandDesc
                     { cmdName = "check"
                     , cmdUiDescription = "check a file for GHC warnings and errors"
                     , cmdFileExtensions = [".hs",".lhs"]
                     , cmdContexts = [CtxFile]
                     , cmdAdditionalParams = []
                     }
          , cmdFunc = checkCmd
          }
      , Command
          { cmdDesc = CommandDesc
                     { cmdName = "lint"
                     , cmdUiDescription = "Check files using `hlint'"
                     , cmdFileExtensions = [".hs",".lhs"]
                     , cmdContexts = [CtxFile]
                     , cmdAdditionalParams = []
                     }
          , cmdFunc = lintCmd
          }
      , Command
          { cmdDesc = CommandDesc
                     { cmdName = "find"
                     , cmdUiDescription = "List all modules that define SYMBOL"
                     , cmdFileExtensions = [".hs",".lhs"]
                     , cmdContexts = [CtxProject]
                     , cmdAdditionalParams = [RP "symbol" "The SYMBOL to look up" PtText]
                     }
          , cmdFunc = findCmd
          }
      , Command
          { cmdDesc = CommandDesc
                     { cmdName = "info"
                     , cmdUiDescription = "Look up an identifier in the context of FILE (like ghci's `:info')"
                     , cmdFileExtensions = [".hs",".lhs"]
                     , cmdContexts = [CtxFile]
                     , cmdAdditionalParams = [RP "expr" "The EXPR to provide info on" PtText]
                     }
          , cmdFunc = infoCmd
          }
      , Command
          { cmdDesc = CommandDesc
                     { cmdName = "type"
                     , cmdUiDescription = "Get the type of the expression under (LINE,COL)"
                     , cmdFileExtensions = [".hs",".lhs"]
                     , cmdContexts = [CtxPoint]
                     , cmdAdditionalParams = []
                     }
          , cmdFunc = typeCmd
          }
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

checkCmd :: CommandFunc String
checkCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& RNil) -> do
      liftIO $ runGhcModCommand fileName (\f->GM.checkSyntax [f])
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcModPlugin.checkCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- ---------------------------------------------------------------------

-- TODO: Must define a directory to base the search from, to be able to resolve
-- the project root.
findCmd :: CommandFunc String
findCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdText "symbol" :& RNil) req of
    Left err -> return err
    Right (ParamText _symbol :& RNil) -> do
      -- liftIO $ runGhcModCommand (GM.findSymbol (T.unpack symbol))
      -- dir <- liftIO getCurrentDirectory
      -- return (IdeResponseOk (String $ T.pack dir))
      -- return (IdeResponseOk (String $ _symbol))
      return (IdeResponseOk "Placholder:Need to debug this in ghc-mod, returns 'does not exist (No such file or directory)'")
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcModPlugin.findCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- ---------------------------------------------------------------------

lintCmd :: CommandFunc String
lintCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& RNil) -> do
      liftIO $ runGhcModCommand fileName GM.lint
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcModPlugin.lintCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- ---------------------------------------------------------------------

infoCmd :: CommandFunc String
infoCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& IdText "expr" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamText expr :& RNil) -> do
      liftIO $ runGhcModCommand fileName (flip GM.info (GM.Expression (T.unpack expr)))
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcModPlugin.infoCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- ---------------------------------------------------------------------

typeCmd :: CommandFunc TypeInfo
typeCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos (r,c) :& RNil) -> do
      fmap (toTypeInfo . T.lines . T.pack) <$> liftIO (runGhcModCommand fileName (\f->GM.types f r c))
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
runGhcModCommand :: (ValidResponse a)
  => T.Text -- ^ The file name we'll operate on
  -> (FilePath -> GM.GmT (GM.GmOutT (GM.GmOutT IO)) a) -> IO (IdeResponse a)
runGhcModCommand fp cmd = do
  let (dir,f) = fileInfo fp
  let opts = GM.defaultOptions
  old <- getCurrentDirectory
  bracket (setCurrentDirectory dir)
          (\_ -> setCurrentDirectory old)
          (\_ -> do
            -- we need to get the root of our folder
            -- ghc-mod returns a new line at the end...
            root <- takeWhile (not . isSpace) <$> GM.runGmOutT opts GM.rootInfo
            setCurrentDirectory root
        -- s <- GHC.getSession
            (r,_l) <- GM.runGmOutT opts $ GM.runGhcModT opts $ do
            -- (r,_l) <- GM.runGhcModT opts $ do
                -- GHC.setSession s
                -- s <- GM.getSession
                -- GM.setSession s
                -- setTargets [fileName]
                cr <- cmd f
                -- s' <- GHC.getSession
                let s' = undefined
                return (cr,s')
            -- (Either GM.GhcModError String, GM.GhcModLog)
            case r of
              Left e -> return $ IdeResponseError (IdeError PluginError (T.pack $ "doCheck:got " ++ show e) Nothing)
              Right (checkResult,_s3) -> do
                -- GHC.setSession s3
                return $ (IdeResponseOk checkResult)
          )

{-
dispatcher = runGmlT $ forever $ do
    s <- getSession
    (r, s') <- runGhcModT $ do
      setSession s
      r <- checkSyntax
      s <- getSession
      return (r,s)
    setSession s'

-}

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
