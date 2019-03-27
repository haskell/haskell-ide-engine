{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Haskell.Ide.Engine.Plugin.ApplyRefact where

import           Control.Arrow
import           Control.Exception              ( IOException
                                                , try
                                                )
import           Control.Lens            hiding ( List )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Aeson                        hiding (Error)
import           Data.Maybe
import           Data.Monoid                       ((<>))
import qualified Data.Text                         as T
import           GHC.Generics
import qualified GhcMod.Utils                      as GM
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Extension
import           Language.Haskell.HLint3           as Hlint
import qualified Language.Haskell.LSP.Types        as LSP
import qualified Language.Haskell.LSP.Types.Lens   as LSP
import           Refact.Apply

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------

type HintTitle = T.Text

applyRefactDescriptor :: PluginId -> PluginDescriptor
applyRefactDescriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginName = "ApplyRefact"
  , pluginDesc = "apply-refact applies refactorings specified by the refact package. It is currently integrated into hlint to enable the automatic application of suggestions."
  , pluginCommands =
      [ PluginCommand "applyOne" "Apply a single hint" applyOneCmd
      , PluginCommand "applyAll" "Apply all hints to the file" applyAllCmd
      , PluginCommand "lint" "Run hlint on the file to generate hints" lintCmd
      ]
  , pluginCodeActionProvider = Just codeActionProvider
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  , pluginFormattingProvider = Nothing
  }

-- ---------------------------------------------------------------------

data ApplyOneParams = AOP
  { file      :: Uri
  , start_pos :: Position
  -- | There can be more than one hint suggested at the same position, so HintTitle is used to distinguish between them.
  , hintTitle :: HintTitle
  } deriving (Eq,Show,Generic,FromJSON,ToJSON)

data OneHint = OneHint
  { oneHintPos :: Position
  , oneHintTitle :: HintTitle
  } deriving (Eq, Show)

applyOneCmd :: CommandFunc ApplyOneParams WorkspaceEdit
applyOneCmd = CmdSync $ \(AOP uri pos title) -> do
  applyOneCmd' uri (OneHint pos title)

applyOneCmd' :: Uri -> OneHint -> IdeGhcM (IdeResult WorkspaceEdit)
applyOneCmd' uri oneHint = pluginGetFile "applyOne: " uri $ \fp -> do
      revMapp <- GM.mkRevRedirMapFunc
      res <- GM.withMappedFile fp $ \file' -> liftToGhc $ applyHint file' (Just oneHint) revMapp
      logm $ "applyOneCmd:file=" ++ show fp
      logm $ "applyOneCmd:res=" ++ show res
      case res of
        Left err -> return $ IdeResultFail (IdeError PluginError
                      (T.pack $ "applyOne: " ++ show err) Null)
        Right fs -> return (IdeResultOk fs)


-- ---------------------------------------------------------------------

applyAllCmd :: CommandFunc Uri WorkspaceEdit
applyAllCmd = CmdSync $ \uri -> do
  applyAllCmd' uri

applyAllCmd' :: Uri -> IdeGhcM (IdeResult WorkspaceEdit)
applyAllCmd' uri = pluginGetFile "applyAll: " uri $ \fp -> do
      revMapp <- GM.mkRevRedirMapFunc
      res <- GM.withMappedFile fp $ \file' -> liftToGhc $ applyHint file' Nothing revMapp
      logm $ "applyAllCmd:res=" ++ show res
      case res of
        Left err -> return $ IdeResultFail (IdeError PluginError
                      (T.pack $ "applyAll: " ++ show err) Null)
        Right fs -> return (IdeResultOk fs)

-- ---------------------------------------------------------------------

lintCmd :: CommandFunc Uri PublishDiagnosticsParams
lintCmd = CmdSync $ \uri -> do
  lintCmd' uri

-- AZ:TODO: Why is this in IdeGhcM?
lintCmd' :: Uri -> IdeGhcM (IdeResult PublishDiagnosticsParams)
lintCmd' uri = pluginGetFile "lintCmd: " uri $ \fp -> do
  eitherErrorResult <- GM.withMappedFile fp $ \file' -> 
    liftIO (try $ runExceptT $ runLintCmd file' [] :: IO (Either IOException (Either [Diagnostic] [Idea])))
  
  case eitherErrorResult of
    Left err ->
      return
        $ IdeResultFail (IdeError PluginError
        (T.pack $ "lintCmd: " ++ show err) Null)
    Right res -> case res of
      Left diags ->
        return
          (IdeResultOk
            (PublishDiagnosticsParams (filePathToUri fp) $ List diags)
          )
      Right fs ->
        return
          $ IdeResultOk
          $ PublishDiagnosticsParams (filePathToUri fp)
          $ List (map hintToDiagnostic $ stripIgnores fs)

runLintCmd :: FilePath -> [String] -> ExceptT [Diagnostic] IO [Idea]
runLintCmd fp args = do
  (flags,classify,hint) <- liftIO $ argsSettings args
  let myflags = flags { hseFlags = (hseFlags flags) { extensions = EnableExtension TypeApplications:extensions (hseFlags flags)}}
  res <- bimapExceptT parseErrorToDiagnostic id $ ExceptT $ parseModuleEx myflags fp Nothing
  pure $ applyHints classify hint [res]

parseErrorToDiagnostic :: Hlint.ParseError -> [Diagnostic]
parseErrorToDiagnostic (Hlint.ParseError l msg contents) =
  [Diagnostic
      { _range    = srcLoc2Range l
      , _severity = Just DsInfo -- Not displayed
      , _code     = Just "parser"
      , _source   = Just "hlint"
      , _message  = T.unlines [T.pack msg,T.pack contents]
      , _relatedInformation = Nothing
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

-- | Map over both failure and success.
bimapExceptT :: Functor m => (e -> f) -> (a -> b) -> ExceptT e m a -> ExceptT f m b
bimapExceptT f g (ExceptT m) = ExceptT (fmap h m) where
  h (Left e)  = Left (f e)
  h (Right a) = Right (g a)
{-# INLINE bimapExceptT #-}

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
      , _severity = Just (hintSeverityMap $ ideaSeverity idea)
      , _code     = Just (T.pack $ ideaHint idea)
      , _source   = Just "hlint"
      , _message  = idea2Message idea
      , _relatedInformation = Nothing
      }

-- ---------------------------------------------------------------------

idea2Message :: Idea -> T.Text
idea2Message idea = T.unlines $ [T.pack $ ideaHint idea, "Found:", "  " <> T.pack (ideaFrom idea)]
                               <> toIdea <> map (T.pack . show) (ideaNote idea)
  where
    toIdea :: [T.Text]
    toIdea = case ideaTo idea of
      Nothing -> []
      Just i  -> [T.pack "Why not:", T.pack $ "  " ++ i]

-- ---------------------------------------------------------------------
-- | Maps hlint severities to LSP severities
-- | We want to lower the severities so HLint errors and warnings
-- | don't mix with GHC errors and warnings:
-- | as per https://github.com/haskell/haskell-ide-engine/issues/375
hintSeverityMap :: Severity -> DiagnosticSeverity
hintSeverityMap Ignore     = DsInfo -- cannot really happen after stripIgnores
hintSeverityMap Suggestion = DsHint
hintSeverityMap Warning    = DsInfo
hintSeverityMap Error      = DsInfo

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

applyHint :: FilePath -> Maybe OneHint -> (FilePath -> FilePath) -> IdeM (Either String WorkspaceEdit)
applyHint fp mhint fileMap = do
  runExceptT $ do
    ideas <- getIdeas fp mhint
    let commands = map (show &&& ideaRefactoring) ideas
    liftIO $ logm $ "applyHint:apply=" ++ show commands
    -- set Nothing as "position" for "applyRefactorings" because
    -- applyRefactorings expects the provided position to be _within_ the scope
    -- of each refactoring it will apply.
    -- But "Idea"s returned by HLint pont to starting position of the expressions
    -- that contain refactorings, so they are often outside the refactorings' boundaries.
    -- Example:
    -- Given an expression "hlintTest = reid $ (myid ())"
    -- Hlint returns an idea at the position (1,13)
    -- That contains "Redundant brackets" refactoring at position (1,20):
    --
    -- [("src/App/Test.hs:5:13: Warning: Redundant bracket\nFound:\n  reid $ (myid ())\nWhy not:\n  reid $ myid ()\n",[Replace {rtype = Expr, pos = SrcSpan {startLine = 5, startCol = 20, endLine = 5, endCol = 29}, subts = [("x",SrcSpan {startLine = 5, startCol = 21, endLine = 5, endCol = 28})], orig = "x"}])]
    --
    -- If we provide "applyRefactorings" with "Just (1,13)" then
    -- the "Redundant bracket" hint will never be executed
    -- because SrcSpan (1,20,??,??) doesn't contain position (1,13).
    appliedFile <- liftIO $ applyRefactorings Nothing commands fp
    diff <- ExceptT $ Right <$> makeDiffResult fp (T.pack appliedFile) fileMap
    liftIO $ logm $ "applyHint:diff=" ++ show diff
    return diff

-- | Gets HLint ideas for
getIdeas :: MonadIO m => FilePath -> Maybe OneHint -> ExceptT String m [Idea]
getIdeas lintFile mhint = do
  let hOpts = hlintOpts lintFile (oneHintPos <$> mhint)
  ideas <- runHlint lintFile hOpts
  pure $ maybe ideas (`filterIdeas` ideas) mhint

-- | If we are only interested in applying a particular hint then
-- let's filter out all the irrelevant ideas
filterIdeas :: OneHint -> [Idea] -> [Idea]
filterIdeas (OneHint (Position l c) title) ideas =
  let
    title' = T.unpack title
    ideaPos = (srcSpanStartLine &&& srcSpanStartColumn) . ideaSpan
  in filter (\i -> ideaHint i == title' && ideaPos i == (l+1, c+1)) ideas

hlintOpts :: FilePath -> Maybe Position -> [String]
hlintOpts lintFile mpos =
  let
    posOpt (Position l c) = " --pos " ++ show (l+1) ++ "," ++ show (c+1)
    opts = maybe "" posOpt mpos
  in [lintFile, "--quiet", "--refactor", "--refactor-options=" ++ opts ]

runHlint :: MonadIO m => FilePath -> [String] -> ExceptT String m [Idea]
runHlint fp args =
  do (flags,classify,hint) <- liftIO $ argsSettings args
     let myflags = flags { hseFlags = (hseFlags flags) { extensions = EnableExtension TypeApplications:extensions (hseFlags flags)}}
     res <- bimapExceptT showParseError id $ ExceptT $ liftIO $ parseModuleEx myflags fp Nothing
     pure $ applyHints classify hint [res]

showParseError :: Hlint.ParseError -> String
showParseError (Hlint.ParseError location message content) =
  unlines [show location, message, content]

-- ---------------------------------------------------------------------

codeActionProvider :: CodeActionProvider
codeActionProvider plId docId _ context = IdeResultOk <$> hlintActions
  where

    hlintActions :: IdeM [LSP.CodeAction]
    hlintActions = catMaybes <$> mapM mkHlintAction (filter validCommand diags)

    -- |Some hints do not have an associated refactoring
    validCommand (LSP.Diagnostic _ _ (Just code) (Just "hlint") _ _) =
      case code of
        "Eta reduce" -> False
        _            -> True
    validCommand _ = False

    LSP.List diags = context ^. LSP.diagnostics

    mkHlintAction :: LSP.Diagnostic -> IdeM (Maybe LSP.CodeAction)
    mkHlintAction diag@(LSP.Diagnostic (LSP.Range start _) _s (Just code) (Just "hlint") m _) =
      Just . codeAction <$> mkLspCommand plId "applyOne" title (Just args)
     where
       codeAction cmd = LSP.CodeAction title (Just LSP.CodeActionRefactor) (Just (LSP.List [diag])) Nothing (Just cmd)
       title = "Apply hint:" <> head (T.lines m)
       -- need 'file', 'start_pos' and hint title (to distinguish between alternative suggestions at the same location)
       args = [toJSON (AOP (docId ^. LSP.uri) start code)]
    mkHlintAction (LSP.Diagnostic _r _s _c _source _m _) = return Nothing
