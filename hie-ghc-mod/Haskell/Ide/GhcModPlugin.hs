{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Haskell.Ide.GhcModPlugin where

import           Control.Monad
import           Data.Aeson
import           Data.Either
import           Data.Function
import           Data.List
import qualified Data.Map                            as Map
import           Data.Monoid
import qualified Data.Text                           as T
import qualified Data.Text.Read                      as T
import           Data.Vinyl
import qualified Exception                           as G
import           GHC
import qualified GhcMod                              as GM
import qualified GhcMod.Doc                          as GM
import qualified GhcMod.Gap                          as GM
import qualified GhcMod.Monad                        as GM
import qualified GhcMod.SrcUtils                     as GM
import qualified GhcMod.Types                        as GM
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import           Language.Haskell.Refact.Utils.Utils
import           Text.Parsec

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
                     [".hs",".lhs"] (SCtxPoint :& RNil)
                     (  SParamDesc (Proxy :: Proxy "include_constraints") (Proxy :: Proxy "Whether to include constraints in the type sig") SPtBool SRequired
                     :& RNil) SaveAll

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

type GhcModDiagnostics = [(FilePath,[Diagnostic])]

checkCmd :: CommandFunc GhcModDiagnostics
checkCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& RNil) req of
    Left err -> return err
    Right (ParamFile uri :& RNil) ->
      checkCmd' uri

checkCmd' :: Uri -> IdeM (IdeResponse GhcModDiagnostics)
checkCmd' uri =
  pluginGetFile "check: " uri $ \file -> do
    fmap parseGhcDiagnostics <$> runGhcModCommand (GM.checkSyntax [file])

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

setTypecheckedModule :: Uri -> IdeM (IdeResponse GhcModDiagnostics)
setTypecheckedModule uri = do
  pluginGetFile "setTypecheckedModule: " uri $ \fp ->
    runGhcModCommand $ do
      (diags', mtm) <- getTypecheckedModuleGhc fp
      let diags = parseGhcDiagnostics diags'
      case mtm of
        Nothing -> return diags
        Just tm -> do
          let cm = CachedModule tm return return
          modifyCachedModules (Map.insert uri cm)
          return diags

-- ---------------------------------------------------------------------

lintCmd :: CommandFunc T.Text
lintCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& RNil) req of
    Left err -> return err
    Right (ParamFile uri :& RNil) ->
      lintCmd' uri

lintCmd' :: Uri -> IdeM (IdeResponse T.Text)
lintCmd' uri =
  pluginGetFile "lint: " uri $ \file -> do
    fmap T.pack <$> runGhcModCommand (GM.lint GM.defaultLintOpts file)

-- ---------------------------------------------------------------------

infoCmd :: CommandFunc T.Text
infoCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& IdText "expr" :& RNil) req of
    Left err -> return err
    Right (ParamFile uri :& ParamText expr :& RNil) ->
      infoCmd' uri expr

infoCmd' :: Uri -> T.Text -> IdeM (IdeResponse T.Text)
infoCmd' uri expr =
  pluginGetFile "info: " uri $ \file -> do
    fmap T.pack <$> runGhcModCommand (GM.info file (GM.Expression (T.unpack expr)))

-- ---------------------------------------------------------------------

typeCmd :: CommandFunc TypeInfo
typeCmd = CmdSync $ \_ctxs req ->
  case getParams (IdBool "include_constraints" :& IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamBool bool :& ParamFile uri :& ParamPos pos :& RNil) -> do
      typeCmd' bool uri pos

typeCmd' :: Bool -> Uri -> Position -> IdeM (IdeResponse TypeInfo)
typeCmd' bool uri (Position l c) =
  pluginGetFile "type: " uri $ \file -> do
    fmap (toTypeInfo . T.lines . T.pack) <$> runGhcModCommand (GM.types bool file (l+1) (c+1))

-- ---------------------------------------------------------------------

newTypeCmd :: Bool -> Uri -> Position -> IdeM (IdeResponse [(Range, T.Text)])
newTypeCmd bool uri newPos = do
    mcm <- Map.lookup uri <$> getCachedModules
    case mcm of
      Nothing -> return $ IdeResponseOk []
      Just cm -> do
        let mOldPos = newPosToOld cm newPos
        case mOldPos of
          Nothing -> return $ IdeResponseOk []
          Just (Position l c) ->
            GM.unGmlT $ GM.withInteractiveContext $ do
              let tm = tcMod cm
              spanTypes' <- GM.collectSpansTypes bool tm (l+1,c+1)
              let spanTypes = sortBy (GM.cmp `on` fst) spanTypes'
              dflag        <- getSessionDynFlags
              st           <- GM.getStyle
              res <- forM spanTypes $ \(spn, t) -> do
                range' <- GM.GmlT $ srcLoc2Range spn
                let getNewRange (Range start end) = do
                      s' <- oldPosToNew cm start
                      e' <- oldPosToNew cm end
                      return $ Range s' e'
                case getNewRange <$> range' of
                  (Right (Just range)) -> return [(range , T.pack $ GM.pretty dflag st t)]
                  _ -> return []
              return $ IdeResponseOk $ concat res

getDynFlags :: IdeM DynFlags
getDynFlags = GM.unGmlT getSessionDynFlags

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

-- ---------------------------------------------------------------------
-- parsec parser for GHC error messages

type P = Parsec String ()

parseGhcDiagnostics :: String -> [(FilePath,[Diagnostic])]
parseGhcDiagnostics str =
  case parse diagnostics "inp" str of
    Left err -> error $ "parseGhcDiagnostics: got error" ++ show err
    Right ds -> ds

diagnostics :: P [(FilePath, [Diagnostic])]
diagnostics = (sepEndBy diagnostic (char '\n')) <* eof

diagnostic :: P (FilePath,[Diagnostic])
diagnostic = do
  fname <- many1 (noneOf ":")
  _ <- char ':'
  l <- number
  _ <- char ':'
  c <- number
  _ <- char ':'
  severity <- optionSeverity
  msglines <- sepEndBy (many1 (noneOf "\n\0")) (char '\0')
  let pos = (Position (l-1) (c-1))
  -- AZ:TODO: consider setting pprCols dflag value in the call, for better format on vscode
  return (fname,[Diagnostic (Range pos pos) (Just severity) Nothing (Just "ghcmod") (T.pack $ unlines msglines)] )

optionSeverity :: P DiagnosticSeverity
optionSeverity =
  (string "Warning:" >> return DsWarning)
  <|> (string "Error:" >> return DsError)
  <|> return DsError

number :: P Int
number = do
  s <- many1 digit
  return $ read s

-- ---------------------------------------------------------------------
