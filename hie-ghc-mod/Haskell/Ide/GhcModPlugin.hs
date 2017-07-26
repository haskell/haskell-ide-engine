{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Haskell.Ide.GhcModPlugin where

import           Data.Aeson
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Monoid
import           Control.Monad.IO.Class
import           Data.IORef
import qualified Data.Text                           as T
import qualified Data.Text.Read                      as T
import qualified Data.Map                            as Map
import qualified Data.Set                            as Set
import           Data.Vinyl
import qualified Exception                           as G
import           GHC
import qualified GhcMod                              as GM
import qualified GhcMod.Doc                          as GM
import qualified GhcMod.Gap                          as GM
import qualified GhcMod.Monad                        as GM
import qualified GhcMod.SrcUtils                     as GM
import qualified GhcMod.Utils                        as GM
import qualified GhcMod.Error                        as GM
import qualified GhcMod.Types                        as GM
import qualified GhcMod.DynFlags                     as GM
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.SemanticTypes
import           Text.Parsec
import Outputable (renderWithStyle)
import DynFlags
import HscTypes
import ErrUtils
import Bag
import System.FilePath

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
type Diagnostics = Map.Map Uri (Set.Set Diagnostic)

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

lspSev :: Severity -> DiagnosticSeverity
lspSev SevWarning = DsWarning
lspSev SevError = DsError
lspSev SevFatal = DsError
lspSev SevInfo = DsInfo
lspSev _ = DsInfo

type AdditionalErrs = [T.Text]

logDiag :: (FilePath -> FilePath) -> IORef AdditionalErrs -> IORef Diagnostics -> LogAction
logDiag rfm eref dref df _reason sev spn style msg = do
  eloc <- srcSpan2Loc rfm spn
  let msgTxt = T.pack $ renderWithStyle df msg style
  case eloc of
    Right (Location uri range) -> do
      let update = Map.insertWith' Set.union uri l
            where l = Set.singleton diag
          diag = Diagnostic range (Just $ lspSev sev) Nothing (Just "ghcmod") msgTxt
      modifyIORef' dref update
    Left _ -> do
      modifyIORef' eref (msgTxt:)
      return ()

unhelpfulSrcSpanErr :: T.Text -> IdeFailure
unhelpfulSrcSpanErr err =
  IdeRFail $
    IdeError PluginError
             ("Unhelpful SrcSpan" <> ": \"" <> err <> "\"")
             Null

srcErrToDiag :: MonadIO m
  => DynFlags
  -> (FilePath -> FilePath)
  -> SourceError -> m (Diagnostics, AdditionalErrs)
srcErrToDiag df rfm se = do
  debugm "in srcErrToDiag"
  let errMsgs = bagToList $ srcErrorMessages se
      processMsg err = do
        let sev = Just DsError
            unqual = errMsgContext err
            st = GM.mkErrStyle' df unqual
            msgTxt = T.pack $ renderWithStyle df (pprLocErrMsg err) st
        eloc <- srcSpan2Loc rfm $ errMsgSpan err
        case eloc of
          Right (Location uri range) ->
            return $ Right (uri, Diagnostic range sev Nothing (Just "ghcmod") msgTxt)
          Left _ -> return $ Left msgTxt
      processMsgs [] = return (Map.empty,[])
      processMsgs (x:xs) = do
        res <- processMsg x
        (m,es) <- processMsgs xs
        case res of
          Right (uri, diag) ->
            return (Map.insertWith' Set.union uri (Set.singleton diag) m, es)
          Left e -> return (m, e:es)
  processMsgs errMsgs

myLogger :: GM.IOish m
  => (FilePath -> FilePath)
  -> GM.GmlT m ()
  -> GM.GmlT m (Diagnostics, AdditionalErrs)
myLogger rfm action = do
  env <- getSession
  diagRef <- liftIO $ newIORef Map.empty
  errRef <- liftIO $ newIORef []
  ips <- map takeDirectory <$> GM.getMMappedFilePaths
  let setLogger df = df { log_action = logDiag rfm errRef diagRef }
      setIncludePath df = df { includePaths = ips ++ includePaths df }
      ghcErrRes msg = (Map.empty, [T.pack msg])
      handlers =
        [ GM.GHandler $ \ex ->
            srcErrToDiag (hsc_dflags env) rfm ex
        , GM.GHandler $ \ex ->
            return $ ghcErrRes $ GM.renderGm $ GM.ghcExceptionDoc ex
        , GM.GHandler $ \(ex :: GM.SomeException) ->
            return (Map.empty ,[T.pack (show ex)])
        ]
      action' = do
        GM.withDynFlags (setIncludePath . setLogger) action
        diags <- liftIO $ readIORef diagRef
        errs <- liftIO $ readIORef errRef
        return (diags,errs)
  GM.gcatches action' handlers

setTypecheckedModule :: Uri -> IdeM (IdeResponse (Diagnostics, AdditionalErrs))
setTypecheckedModule uri =
  pluginGetFile "setTypecheckedModule: " uri $ \fp -> do
    rfm <- GM.mkRevRedirMapFunc
    ((diags', errs), mtm) <- getTypecheckedModuleGhc (myLogger rfm) fp
    let diags = Map.insertWith' Set.union uri Set.empty diags'
    case mtm of
      Nothing -> do
        debugm $ "setTypecheckedModule: Didn't get typechecked module for: " ++ show fp
        return $ IdeResponseOk (diags,errs)
      Just tm -> do
        let cm = CachedModule tm rfm return return
        cacheModule uri cm
        return $ IdeResponseOk (diags,errs)

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
    mcm <- getCachedModule uri
    case mcm of
      Nothing -> return $ IdeResponseOk []
      Just cm -> do
        let mOldPos = newPosToOld cm newPos
        case mOldPos of
          Nothing -> return $ IdeResponseOk []
          Just pos ->
            GM.unGmlT $ GM.withInteractiveContext $ do
              let tm = tcMod cm
              spanTypes' <- GM.collectSpansTypes bool tm $ unPos pos
              let spanTypes = sortBy (GM.cmp `on` fst) spanTypes'
              dflag        <- getSessionDynFlags
              st           <- GM.getStyle
              let f (spn, t) = do
                    let range' = srcSpan2Range spn
                    case oldRangeToNew cm <$> range' of
                      (Right (Just range)) -> [(range , T.pack $ GM.pretty dflag st t)]
                      _ -> []
              return $ IdeResponseOk $ concatMap f spanTypes

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
    Left x -> error $  "parseGhcDiagnostics error " ++ show x ++ "\n\n on " ++ str
    Right r -> r

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
