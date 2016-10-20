{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Haskell.Ide.ApplyRefactPlugin where

import           Control.Arrow
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson hiding (Error)
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Vinyl
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import           Language.Haskell.HLint3 as Hlint
import           Refact.Apply
import           System.Directory
import           System.IO.Extra
import           Language.Haskell.Exts.SrcLoc

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------

applyRefactDescriptor :: TaggedPluginDescriptor _
applyRefactDescriptor = PluginDescriptor
  {
    pdUIShortName = "ApplyRefact"
  , pdUIOverview = "apply-refact applies refactorings specified by the refact package. It is currently integrated into hlint to enable the automatic application of suggestions."
    , pdCommands =

        buildCommand applyOneCmd (Proxy :: Proxy "applyOne") "Apply a single hint"
                   [".hs"] (SCtxPoint :& RNil) RNil SaveAll

      :& buildCommand applyAllCmd (Proxy :: Proxy "applyAll") "Apply all hints to the file"
                   [".hs"] (SCtxFile :& RNil) RNil SaveAll

      :& buildCommand lintCmd (Proxy :: Proxy "lint") "Run hlint on the file to generate hints"
                   [".hs"] (SCtxFile :& RNil) RNil SaveAll

      :& RNil
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

applyOneCmd :: CommandFunc HieDiff
applyOneCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& IdPos "start_pos" :& RNil) req of
    Left err -> return err
    Right (ParamFile file :& ParamPos pos :& RNil) -> do
      res <- liftIO $ applyHint (T.unpack file) (Just pos)
      logm $ "applyOneCmd:res=" ++ show res
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "applyOne: " ++ show err) Null)
        Right fs -> return (IdeResponseOk fs)


-- ---------------------------------------------------------------------

applyAllCmd :: CommandFunc HieDiff
applyAllCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& RNil) req of
    Left err -> return err
    Right (ParamFile file :& RNil) -> do
      res <- liftIO $ applyHint (T.unpack file) Nothing
      logm $ "applyAllCmd:res=" ++ show res
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "applyAll: " ++ show err) Null)
        Right fs -> return (IdeResponseOk fs)

-- ---------------------------------------------------------------------

lintCmd :: CommandFunc FileDiagnostics
lintCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdFile "file" :& RNil) req of
    Left err -> return err
    Right (ParamFile file :& RNil) -> do
      -- res <- liftIO $ applyHint (T.unpack file) Nothing
      res <- liftIO $ runEitherT $ runHlint (T.unpack file) []
      logm $ "lint:res=" ++ show res
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "lint: " ++ show err) Null)
        Right fs -> return (IdeResponseOk (FileDiagnostics ("file://" ++ T.unpack file) (map hintToDiagnostic fs)))

{-
-- | An idea suggest by a 'Hint'.
data Idea = Idea
    {ideaModule :: String -- ^ The module the idea applies to, may be @\"\"@ if the module cannot be determined or is a result of cross-module hints.
    ,ideaDecl :: String -- ^ The declaration the idea applies to, typically the function name, but may be a type name.
    ,ideaSeverity :: Severity -- ^ The severity of the idea, e.g. 'Warning'.
    ,ideaHint :: String -- ^ The name of the hint that generated the idea, e.g. @\"Use reverse\"@.
    ,ideaSpan :: SrcSpan -- ^ The source code the idea relates to.
    ,ideaFrom :: String -- ^ The contents of the source code the idea relates to.
    ,ideaTo :: Maybe String -- ^ The suggested replacement, or 'Nothing' for no replacement (e.g. on parse errors).
    ,ideaNote :: [Note] -- ^ Notes about the effect of applying the replacement.
    ,ideaRefactoring :: [Refactoring R.SrcSpan] -- ^ How to perform this idea
    }
    deriving (Eq,Ord)

-}

-- ---------------------------------------------------------------------

hintToDiagnostic :: Idea -> Diagnostic
hintToDiagnostic idea
  = Diagnostic
      { rangeDiagnostic    = ss2Range (ideaSpan idea)
      , severityDiagnostic = Just (severity2DisgnosticSeverity $ ideaSeverity idea)
      , codeDiagnostic     = Just (ideaHint idea)
      , sourceDiagnostic   = Just "hlint"
      , messageDiagnostic  = idea2Message idea
      }

-- ---------------------------------------------------------------------
{-

instance Show Idea where
    show = showEx id


showANSI :: IO (Idea -> String)
showANSI = do
    f <- hsColourConsole
    return $ showEx f

showEx :: (String -> String) -> Idea -> String
showEx tt Idea{..} = unlines $
    [showSrcLoc (getPointLoc ideaSpan) ++ ": " ++ (if ideaHint == "" then "" else show ideaSeverity ++ ": " ++ ideaHint)] ++
    f "Found" (Just ideaFrom) ++ f "Why not" ideaTo ++
    ["Note: " ++ n | let n = showNotes ideaNote, n /= ""]
    where
        f msg Nothing = []
        f msg (Just x) | null xs = [msg ++ " remove it."]
                       | otherwise = (msg ++ ":") : map ("  "++) xs
            where xs = lines $ tt x

-}

idea2Message :: Idea -> String
idea2Message idea = unlines $ [ideaDecl idea, ideaFrom idea] ++ maybeToList (ideaTo idea) ++ (map show $ ideaNote idea)

-- ---------------------------------------------------------------------

severity2DisgnosticSeverity :: Severity -> DiagnosticSeverity
severity2DisgnosticSeverity Ignore     = DsInfo
severity2DisgnosticSeverity Suggestion = DsHint
severity2DisgnosticSeverity Warning    = DsWarning
severity2DisgnosticSeverity Error      = DsError

-- ---------------------------------------------------------------------

ss2Range :: SrcSpan -> Range
ss2Range ss = Range ps pe
  where
    ps = Position (srcSpanStartLine ss) (srcSpanStartColumn ss)
    pe = Position (srcSpanEndLine ss)   (srcSpanEndColumn ss)

-- ---------------------------------------------------------------------

applyHint :: FilePath -> Maybe Pos -> IO (Either String HieDiff)
applyHint file mpos = do
  withTempFile $ \f -> do
    let optsf = "-o " ++ f
        opts = case mpos of
                 Nothing -> optsf
                 Just (Pos (Line l) (Col c)) -> optsf ++ " --pos " ++ show l ++ "," ++ show c
        hlintOpts = [file, "--quiet", "--refactor", "--refactor-options=" ++ opts ]
    runEitherT $ do
      ideas <- runHlint file hlintOpts
      liftIO $ logm $ "applyHint:ideas=" ++ show ideas
      let commands = map (show &&& ideaRefactoring) ideas
      appliedFile <- liftIO $ applyRefactorings (fmap unPos mpos) commands file
      diff <- liftIO $ makeDiffResult file (T.pack appliedFile)
      liftIO $ logm $ "applyHint:diff=" ++ show diff
      return diff


runHlint :: FilePath -> [String] -> EitherT String IO [Idea]
runHlint file args =
  do (flags,classify,hint) <- liftIO $ argsSettings args
     res <- bimapEitherT showParseError id $ EitherT $ parseModuleEx flags file Nothing
     pure $ applyHints classify hint [res]

showParseError :: Hlint.ParseError -> String
showParseError (Hlint.ParseError location message content) = unlines [show location, message, content]

makeDiffResult :: FilePath -> T.Text -> IO HieDiff
makeDiffResult orig new = do
  origText <- T.readFile orig
  let (HieDiff f _ d) = diffText (orig,origText) ("changed",new)
  f' <- liftIO $ makeRelativeToCurrentDirectory f
  -- return (HieDiff f' s' d)
  return (HieDiff f' "changed" d)
