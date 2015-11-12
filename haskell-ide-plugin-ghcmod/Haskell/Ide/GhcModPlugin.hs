{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Haskell.Ide.GhcModPlugin where

import           Control.Exception
import           Data.Vinyl
-- import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Text as T
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import qualified Language.Haskell.GhcMod as GM
import qualified Language.Haskell.GhcMod.Monad as GM
-- import           System.Directory

-- ---------------------------------------------------------------------

ghcmodDescriptor :: PluginDescriptor
ghcmodDescriptor = PluginDescriptor
  {
    pdCommands =
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

checkCmd :: CommandFunc
checkCmd _ctxs req = do
  case getParams (IdFile "file" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& RNil) -> do
      liftIO $ runGhcModCommand (GM.checkSyntax [T.unpack fileName])
    Right _ -> error $ "GhcModPlugin.checkCmd: ghc’s exhaustiveness checker is broken"

-- ---------------------------------------------------------------------

-- TODO: Must define a directory to base the search from, to be able to resolve
-- the project root.
findCmd :: CommandFunc
findCmd _ctxs req = do
  case getParams (IdText "symbol" :& RNil) req of
    Left err -> return err
    Right (ParamText _symbol :& RNil) -> do
      -- liftIO $ runGhcModCommand (GM.findSymbol (T.unpack symbol))
      -- dir <- liftIO getCurrentDirectory
      -- return (IdeResponseOk (String $ T.pack dir))
      -- return (IdeResponseOk (String $ _symbol))
      return (IdeResponseOk (String $ "Placholder:Need to debug this in ghc-mod, returns 'does not exist (No such file or directory)'"))
    Right x -> error $ "GhcModPlugin.findCmd: got unexpected param:" ++ show x

-- ---------------------------------------------------------------------

lintCmd :: CommandFunc
lintCmd _ctxs req = do
  case getParams (IdFile "file" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& RNil) -> do
      liftIO $ runGhcModCommand (GM.lint (T.unpack fileName))
    Right x -> error $ "GhcModPlugin.lintCmd: got unexpected file param:" ++ show x

-- ---------------------------------------------------------------------

infoCmd :: CommandFunc
infoCmd _ctxs req = do
  case getParams (IdFile "file" :& IdText "expr" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamText expr :& RNil) -> do
      liftIO $ runGhcModCommand (GM.info (T.unpack fileName) (GM.Expression (T.unpack expr)))
    Right x -> error $ "GhcModPlugin.infoCmd: got unexpected param:" ++ show x

-- ---------------------------------------------------------------------

typeCmd :: CommandFunc
typeCmd _ctxs req = do
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos (r,c) :& RNil) -> do
      liftIO $ runGhcModCommand (GM.types (T.unpack fileName) r c)
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcModPlugin.typesCmd: ghc’s exhaustiveness checker is broken" Nothing)

-- ---------------------------------------------------------------------

-- TODO: Need to thread the session through as in the commented out code below.
runGhcModCommand :: (ToJSON a) => GM.GmT (GM.GmOutT (GM.GmOutT IO)) a -> IO IdeResponse
runGhcModCommand cmd = do
  let opts = GM.defaultOptions
  -- s <- GHC.getSession
  (r,_l) <- GM.runGmOutT opts $ GM.runGhcModT opts $ do
  -- (r,_l) <- GM.runGhcModT opts $ do
      -- GHC.setSession s
      -- s <- GM.getSession
      -- GM.setSession s
      -- setTargets [fileName]
      cr <- cmd
      -- s' <- GHC.getSession
      let s' = undefined
      return (cr,s')
  -- (Either GM.GhcModError String, GM.GhcModLog)
  case r of
    Left e -> return $ IdeResponseError (IdeError PluginError (T.pack $ "doCheck:got " ++ show e) Nothing)
    Right (checkResult,_s3) -> do
      -- GHC.setSession s3
      return $ (IdeResponseOk (toJSON checkResult))

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

catchException :: (IO t) -> IO (Either String t)
catchException f = do
  res <- handle handler (f >>= \r -> return $ Right r)
  return res
  where
    handler:: SomeException -> IO (Either String t)
    handler e = return (Left (show e))
