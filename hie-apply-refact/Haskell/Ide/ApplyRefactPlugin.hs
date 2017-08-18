{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Haskell.Ide.ApplyRefactPlugin where

import           Control.Arrow
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson                        hiding (Error)
import           Data.Monoid                       ((<>))
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
import           GHC.Generics
import qualified GhcMod.Utils                      as GM
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Extension
import           Language.Haskell.HLint3           as Hlint
import           Refact.Apply
import           System.IO.Extra

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------

applyRefactDescriptor :: PluginDescriptor
applyRefactDescriptor = PluginDescriptor
  {
    pluginName = "ApplyRefact"
  , pluginDesc = "apply-refact applies refactorings specified by the refact package. It is currently integrated into hlint to enable the automatic application of suggestions."
  , pluginCommands =
      [ PluginCommand "applyOne" "Apply a single hint" applyOneCmd
      , PluginCommand "applyAll" "Apply all hints to the file" applyAllCmd
      , PluginCommand "lint" "Run hlint on the file to generate hints" lintCmd
      ]
  }

-- ---------------------------------------------------------------------

data ApplyOneParams = AOP
  { file      :: Uri
  , start_pos :: Position
  } deriving (Eq,Show,Generic,FromJSON,ToJSON)

applyOneCmd :: CommandFunc ApplyOneParams WorkspaceEdit
applyOneCmd = CmdSync $ \(AOP uri pos)-> do
  applyOneCmd' uri pos

applyOneCmd' :: Uri -> Position -> IdeM (IdeResponse WorkspaceEdit)
applyOneCmd' uri pos = pluginGetFile "applyOne: " uri $ \fp -> do
      revMapp <- GM.mkRevRedirMapFunc
      res <- GM.withMappedFile fp $ \file' -> liftIO $ applyHint file' (Just pos) revMapp
      logm $ "applyOneCmd:file=" ++ show fp
      logm $ "applyOneCmd:res=" ++ show res
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "applyOne: " ++ show err) Null)
        Right fs -> return (IdeResponseOk fs)


-- ---------------------------------------------------------------------

applyAllCmd :: CommandFunc Uri WorkspaceEdit
applyAllCmd = CmdSync $ \uri -> do
  applyAllCmd' uri

applyAllCmd' :: Uri -> IdeM (IdeResponse WorkspaceEdit)
applyAllCmd' uri = pluginGetFile "applyAll: " uri $ \fp -> do
      revMapp <- GM.mkRevRedirMapFunc
      res <- GM.withMappedFile fp $ \file' -> liftIO $ applyHint file' Nothing revMapp
      logm $ "applyAllCmd:res=" ++ show res
      case res of
        Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "applyAll: " ++ show err) Null)
        Right fs -> return (IdeResponseOk fs)

-- ---------------------------------------------------------------------

lintCmd :: CommandFunc Uri PublishDiagnosticsParams
lintCmd = CmdSync $ \uri -> do
  lintCmd' uri

lintCmd' :: Uri -> IdeM (IdeResponse PublishDiagnosticsParams)
lintCmd' uri = pluginGetFile "lintCmd: " uri $ \fp -> do
      res <- GM.withMappedFile fp $ \file' -> liftIO $ runEitherT $ runLintCmd file' []
      logm $ "lint:res=" ++ show res
      case res of
        Left diags ->
          return (IdeResponseOk (PublishDiagnosticsParams (filePathToUri fp) $ List diags))
        Right fs ->
          return $ IdeResponseOk $
            PublishDiagnosticsParams (filePathToUri fp)
              $ List (map hintToDiagnostic $ stripIgnores fs)

runLintCmd :: FilePath -> [String] -> EitherT [Diagnostic] IO [Idea]
runLintCmd fp args =
  do (flags,classify,hint) <- liftIO $ argsSettings args
     let myflags = flags { hseFlags = (hseFlags flags) { extensions = (EnableExtension TypeApplications:extensions (hseFlags flags))}}
     res <- bimapEitherT parseErrorToDiagnostic id $ EitherT $ parseModuleEx myflags fp Nothing
     pure $ applyHints classify hint [res]

parseErrorToDiagnostic :: Hlint.ParseError -> [Diagnostic]
parseErrorToDiagnostic (Hlint.ParseError l msg contents) =
  [Diagnostic
      { _range    = srcLoc2Range l
      , _severity = Just DsError
      , _code     = Just "parser"
      , _source   = Just "hlint"
      , _message  = T.unlines [T.pack msg,T.pack contents]
      }]

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

stripIgnores :: [Idea] -> [Idea]
stripIgnores ideas = filter notIgnored ideas
  where
    notIgnored idea = ideaSeverity idea /= Ignore

-- ---------------------------------------------------------------------

hintToDiagnostic :: Idea -> Diagnostic
hintToDiagnostic idea
  = Diagnostic
      { _range    = ss2Range (ideaSpan idea)
      , _severity = Just (severity2DisgnosticSeverity $ ideaSeverity idea)
      , _code     = Nothing
      , _source   = Just "hlint"
      , _message  = idea2Message idea
      }

-- ---------------------------------------------------------------------

idea2Message :: Idea -> T.Text
idea2Message idea = T.unlines $ [T.pack $ ideaHint idea, "Found:", ("  " <> T.pack (ideaFrom idea))]
                               <> toIdea <> map (T.pack . show) (ideaNote idea)
  where
    toIdea :: [T.Text]
    toIdea = case ideaTo idea of
      Nothing -> []
      Just i  -> [T.pack "Why not:", T.pack $ "  " ++ i]

-- ---------------------------------------------------------------------

severity2DisgnosticSeverity :: Severity -> DiagnosticSeverity
severity2DisgnosticSeverity Ignore     = DsInfo
severity2DisgnosticSeverity Suggestion = DsHint
severity2DisgnosticSeverity Warning    = DsWarning
severity2DisgnosticSeverity Error      = DsError

-- ---------------------------------------------------------------------

srcLoc2Range :: SrcLoc -> Range
srcLoc2Range (SrcLoc _ l c) = Range ps pe
  where
    ps = Position (l-1) (c-1)
    pe = Position (l-1) 100000

-- ---------------------------------------------------------------------

ss2Range :: SrcSpan -> Range
ss2Range ss = Range ps pe
  where
    ps = Position (srcSpanStartLine ss - 1) (srcSpanStartColumn ss - 1)
    pe = Position (srcSpanEndLine ss - 1)   (srcSpanEndColumn ss - 1)

-- ---------------------------------------------------------------------

applyHint :: FilePath -> Maybe Position -> (FilePath -> FilePath) -> IO (Either String WorkspaceEdit)
applyHint fp mpos fileMap = do
  withTempFile $ \f -> do
    let optsf = "-o " ++ f
        opts = case mpos of
                 Nothing -> optsf
                 Just (Position l c) -> optsf ++ " --pos " ++ show (l+1) ++ "," ++ show (c+1)
        hlintOpts = [fp, "--quiet", "--refactor", "--refactor-options=" ++ opts ]
    runEitherT $ do
      ideas <- runHlint fp hlintOpts
      liftIO $ logm $ "applyHint:ideas=" ++ show ideas
      let commands = map (show &&& ideaRefactoring) ideas
      appliedFile <- liftIO $ applyRefactorings (unPos <$> mpos) commands fp
      diff <- liftIO $ makeDiffResult fp (T.pack appliedFile) fileMap
      liftIO $ logm $ "applyHint:diff=" ++ show diff
      return diff


runHlint :: FilePath -> [String] -> EitherT String IO [Idea]
runHlint fp args =
  do (flags,classify,hint) <- liftIO $ argsSettings args
     let myflags = flags { hseFlags = (hseFlags flags) { extensions = (EnableExtension TypeApplications:extensions (hseFlags flags))}}
     res <- bimapEitherT showParseError id $ EitherT $ parseModuleEx myflags fp Nothing
     pure $ applyHints classify hint [res]

showParseError :: Hlint.ParseError -> String
showParseError (Hlint.ParseError location message content) =
  unlines [show location, message, content]

makeDiffResult :: FilePath -> T.Text -> (FilePath -> FilePath) -> IO WorkspaceEdit
makeDiffResult orig new fileMap = do
  origText <- T.readFile orig
  let fp' = fileMap orig
  fp <- GM.makeAbsolute' fp'
  return $ diffText (filePathToUri fp,origText) new
