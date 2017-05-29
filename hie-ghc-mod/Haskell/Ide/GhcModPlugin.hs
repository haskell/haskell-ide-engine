{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Haskell.Ide.GhcModPlugin where

import           Data.Aeson
import           Data.Either
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Vinyl
import qualified Exception as G
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import qualified GhcMod.Types as GM
import qualified GhcMod as GM

-- ---------------------------------------------------------------------

ghcmodDescriptor :: TaggedPluginDescriptor _
ghcmodDescriptor = PluginDescriptor
  {
    pdUIShortName = "ghc-mod"
  , pdUIOverview = ("ghc-mod is a backend program to enrich Haskell programming "
           <> "in editors. It strives to offer most of the features one has come to expect "
           <> "from modern IDEs in any editor.")
  , pdCommands =
         buildCommand checkCmd (Proxy :: Proxy "check") "check a file for GHC warnings and errors"
                       [".hs",".lhs"] (SCtxFile :& RNil) RNil SaveAll

      :& buildCommand lintCmd (Proxy :: Proxy "lint")  "Check files using `hlint'"
                     [".hs",".lhs"] (SCtxFile :& RNil) RNil SaveAll

      -- :& buildCommand findCmd (Proxy :: Proxy "find")  "List all modules that define SYMBOL"
      --                [".hs",".lhs"] (SCtxProject :& RNil)
      --                (  SParamDesc (Proxy :: Proxy "symbol") (Proxy :: Proxy "The SYMBOL to look up") SPtText SRequired
      --                :& RNil)

      :& buildCommand infoCmd (Proxy :: Proxy "info") "Look up an identifier in the context of FILE (like ghci's `:info')"
                     [".hs",".lhs"] (SCtxFile :& RNil)
                     (  SParamDesc (Proxy :: Proxy "expr") (Proxy :: Proxy "The EXPR to provide info on") SPtText SRequired
                     :& RNil) SaveNone

      :& buildCommand typeCmd (Proxy :: Proxy "type") "Get the type of the expression under (LINE,COL)"
                     [".hs",".lhs"] (SCtxPoint :& RNil) RNil SaveAll

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
      fmap T.pack <$> runGhcModCommand (GM.checkSyntax [T.unpack fileName])

-- ---------------------------------------------------------------------

-- Disabled until ghc-mod no longer needs to launch a separate executable
-- -- | Runs the find command from the given directory, for the given symbol
-- findCmd :: CommandFunc ModuleList
-- findCmd = CmdSync $ \_ctxs req -> do
--   case getParams (IdText "symbol" :& RNil) req of
--     Left err -> return err
--     Right (ParamText symbol :& RNil) -> do
--       runGhcModCommand $
--         (ModuleList . map (T.pack . GM.getModuleString)) <$> GM.findSymbol' (T.unpack symbol)


--       -- return (IdeResponseOk "Placholder:Need to debug this in ghc-mod, returns 'does not exist (No such file or directory)'")
--     Right _ -> return $ IdeResponseError (IdeError InternalError
--       "GhcModPlugin.findCmd: ghc’s exhaustiveness checker is broken" Null)

-- ---------------------------------------------------------------------

lintCmd :: CommandFunc T.Text
lintCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& RNil) -> do
      fmap T.pack <$> runGhcModCommand (GM.lint GM.defaultLintOpts (T.unpack fileName))

-- ---------------------------------------------------------------------

infoCmd :: CommandFunc T.Text
infoCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& IdText "expr" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamText expr :& RNil) -> do
      fmap T.pack <$> runGhcModCommand (GM.info (T.unpack fileName) (GM.Expression (T.unpack expr)))

-- ---------------------------------------------------------------------

typeCmd :: CommandFunc TypeInfo
typeCmd = CmdSync $ \_ctxs req ->
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile fileName :& ParamPos (Pos (Line l) (Col c)) :& RNil) -> do
      {-
      TODO: pass the bool through as a param, in the following call to 'GM.types'
      types :: IOish m
            => Bool         -- ^ Include constraints into type signature
            -> FilePath     -- ^ A target file.
            -> Int          -- ^ Line number.
            -> Int          -- ^ Column number.
            -> GhcModT m String

      -}
      fmap (toTypeInfo . T.lines . T.pack) <$> runGhcModCommand (GM.types False (T.unpack fileName) l c)




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
    return $ TypeResult (toPos (sl,sc)) (toPos (el,ec)) typ

-- ---------------------------------------------------------------------


runGhcModCommand :: IdeM a
                 -> IdeM (IdeResponse a)
runGhcModCommand cmd =
  do (IdeResponseOk <$> cmd) `G.gcatch`
       \(e :: GM.GhcModError) ->
         return $
         IdeResponseFail $
         IdeError PluginError (T.pack $ "hie-ghc-mod: " ++ show e) Null
