{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Haskell.Ide.GhcModPlugin where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Either
import           Data.Function
import           Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Vinyl
import qualified Exception as G
import qualified GHC as G
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import qualified Language.Haskell.GhcMod as GM
import qualified Language.Haskell.GhcMod.Gap as GM
import qualified Language.Haskell.GhcMod.Monad as GM
import qualified Language.Haskell.GhcMod.Types as GM
import qualified Language.Haskell.GhcMod.Utils as GM
import qualified Name as G
import           System.Directory
import           System.FilePath

-- ---------------------------------------------------------------------

ghcmodDescriptor :: TaggedPluginDescriptor _
ghcmodDescriptor = PluginDescriptor
  {
    pdUIShortName = "ghc-mod"
  , pdUIOverview = "ghc-mod is a backend program to enrich Haskell programming \
\in editors. It strives to offer most of the features one has come to expect \
\from modern IDEs in any editor."
  , pdCommands =
         buildCommand checkCmd (Proxy :: Proxy "check") "check a file for GHC warnings and errors"
                       [".hs",".lhs"] (SCtxFile :& RNil) RNil

      :& buildCommand lintCmd (Proxy :: Proxy "lint")  "Check files using `hlint'"
                     [".hs",".lhs"] (SCtxFile :& RNil) RNil

      :& buildCommand findCmd (Proxy :: Proxy "find")  "List all modules that define SYMBOL"
                     [".hs",".lhs"] (SCtxProject :& RNil)
                     (  SParamDesc (Proxy :: Proxy "symbol") (Proxy :: Proxy "The SYMBOL to look up") SPtText SRequired
                     :& RNil)

      :& buildCommand infoCmd (Proxy :: Proxy "info") "Look up an identifier in the context of FILE (like ghci's `:info')"
                     [".hs",".lhs"] (SCtxFile :& RNil)
                     (  SParamDesc (Proxy :: Proxy "expr") (Proxy :: Proxy "The EXPR to provide info on") SPtText SRequired
                     :& RNil)

      :& buildCommand typeCmd (Proxy :: Proxy "type") "Get the type of the expression under (LINE,COL)"
                     [".hs",".lhs"] (SCtxPoint :& RNil) RNil

      :& RNil
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
      fmap T.pack <$> runGhcModCommand (GM.checkSyntax [(T.unpack fileName)])
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcModPlugin.checkCmd: ghc’s exhaustiveness checker is broken" Null)

-- ---------------------------------------------------------------------

-- | Extracted from ghc-mod
getGlobalSymbolTable :: GM.LightGhc [(GM.Symbol, [GM.ModuleString])]
getGlobalSymbolTable = do
  df  <- G.getSessionDynFlags
  let mods = GM.listVisibleModules df
  moduleInfos <- mapM G.getModuleInfo mods
  return $ collectModules
         $ extractBindings `concatMap` (moduleInfos `zip` mods)

-- | Extracted from ghc-mod
extractBindings :: (Maybe G.ModuleInfo, G.Module)
                -> [(GM.Symbol, GM.ModuleString)]
extractBindings (Nothing,  _)   = []
extractBindings (Just inf, mdl) =
  map (\name -> (G.getOccString name, modStr)) names
  where
    names  = G.modInfoExports inf
    modStr = GM.ModuleString $ GM.moduleNameString $ G.moduleName mdl

-- | Extracted from ghc-mod
collectModules :: [(GM.Symbol, GM.ModuleString)]
               -> [(GM.Symbol, [GM.ModuleString])]
collectModules = map tieup . groupBy ((==) `on` fst) . sort
  where
    tieup x = (head (map fst x), map snd x)

-- | Runs the find command from the given directory, for the given symbol
findCmd :: CommandFunc ModuleList
findCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "dir" :& IdText "symbol" :& RNil) req of
    Left err -> return err
    Right (ParamFile dirName :& ParamText symbol :& RNil) -> do
      runGhcModCommand
        (do -- adapted from ghc-mod find command, which launches the executable again
            symbolTable <- M.fromAscList <$> GM.runGmPkgGhc getGlobalSymbolTable
            let f = M.findWithDefault ([]::[GM.ModuleString]) (T.unpack symbol) symbolTable
            return $ ModuleList $ map (T.pack . GM.getModuleString) f
        )

      -- return (IdeResponseOk "Placholder:Need to debug this in ghc-mod, returns 'does not exist (No such file or directory)'")
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcModPlugin.findCmd: ghc’s exhaustiveness checker is broken" Null)

-- ---------------------------------------------------------------------

lintCmd :: CommandFunc T.Text
lintCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& RNil) -> do
      fmap T.pack <$> runGhcModCommand (GM.lint GM.defaultLintOpts (T.unpack fileName))
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcModPlugin.lintCmd: ghc’s exhaustiveness checker is broken" Null)

-- ---------------------------------------------------------------------

infoCmd :: CommandFunc T.Text
infoCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& IdText "expr" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamText expr :& RNil) -> do
      fmap T.pack <$> runGhcModCommand (GM.info (T.unpack fileName) (GM.Expression (T.unpack expr)))
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcModPlugin.infoCmd: ghc’s exhaustiveness checker is broken" Null)

-- ---------------------------------------------------------------------

typeCmd :: CommandFunc TypeInfo
typeCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos (r,c) :& RNil) -> do
      fmap (toTypeInfo . T.lines . T.pack) <$> runGhcModCommand (GM.types (T.unpack fileName) r c)
    Right _ -> return $ IdeResponseError (IdeError InternalError
      "GhcModPlugin.typesCmd: ghc’s exhaustiveness checker is broken" Null)


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


runGhcModCommand :: IdeM a
                 -> IdeM (IdeResponse a)
runGhcModCommand cmd =
  do (IdeResponseOk <$> cmd) `G.gcatch`
       \(e :: GM.GhcModError) ->
         return $
         IdeResponseFail $
         IdeError PluginError (T.pack $ "hie-ghc-mod: " ++ show e) Null
