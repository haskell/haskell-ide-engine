{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}
module Haskell.Ide.Engine.Support.HieExtras
  ( getDynFlags
  , getTypeForName
  , getTypeForNameDetails
  , NameDetails(..)
  , getSymbolsAtPoint
  , getReferencesInDoc
  , getModule
  , findDef
  , findTypeDef
  , showName
  , safeTyThingId
  , VFS.PosPrefixInfo(..)
  , HarePoint(..)
  , customOptions
  -- , splitCaseCmd'
  -- , splitCaseCmd
  , getFormattingPlugin
  ) where

import           Data.Semigroup (Semigroup(..))
import           ConLike
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Exception (SomeException, catch)
import           Data.Aeson
import qualified Data.Aeson.Types                             as J
import           Data.IORef
import qualified Data.Map                                     as Map
import           Data.Maybe
import qualified Data.Text                                    as T
import           Data.Typeable
import           DataCon
import qualified DynFlags                                     as GHC
import           FastString
import           Finder
import           GHC                                          hiding (getContext)
import           GHC.Generics                                 (Generic)

import           Haskell.Ide.Engine.ArtifactMap
import           Haskell.Ide.Engine.Config
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           HscTypes
import qualified Language.Haskell.LSP.Types                   as J
import qualified Language.Haskell.LSP.VFS                     as VFS
import           Language.Haskell.Refact.Utils.MonadFunctions
import           Name
import           NameCache
import           Outputable                                   (Outputable)
import qualified Outputable                                   as GHC
import           Packages
import           SrcLoc
import           TcEnv
import           Type
import           Var
import           Module hiding (getModule)

-- ---------------------------------------------------------------------

getDynFlags :: GHC.TypecheckedModule -> DynFlags
getDynFlags = ms_hspp_opts . pm_mod_summary . tm_parsed_module

-- ---------------------------------------------------------------------

newtype NameMapData = NMD
  { inverseNameMap ::  Map.Map Name [SrcSpan]
  } deriving (Typeable)


invert :: (Ord v) => Map.Map k v -> Map.Map v [k]
invert m = Map.fromListWith (++) [(v,[k]) | (k,v) <- Map.toList m]

instance ModuleCache NameMapData where
  cacheDataProducer tm _ = pure $ NMD inm
    where nm  = initRdrNameMap tm
          inm = invert nm

data NameDetails
  = NameDetails Module OccName
  deriving (Eq)

nsJSON :: NameSpace -> Value
nsJSON ns
  | isVarNameSpace ns = String "v"
  | isDataConNameSpace ns = String "c"
  | isTcClsNameSpace ns  = String "t"
  | isTvNameSpace ns = String "z"
  | otherwise = error "namespace not recognized"

parseNs :: Value -> J.Parser NameSpace
parseNs (String "v") = pure Name.varName
parseNs (String "c") = pure dataName
parseNs (String "t") = pure tcClsName
parseNs (String "z") = pure tvName
parseNs _ = mempty

instance FromJSON NameDetails where
  parseJSON v@(Array _)
    = do
      [modname,modid,namesp,occname] <- parseJSON v
      mn  <- parseJSON modname
      mid <- parseJSON modid
      ns <- parseNs namesp
      occn <- parseJSON occname
      pure $ NameDetails (mkModule (stringToUnitId mid) (mkModuleName mn)) (mkOccName ns occn)
  parseJSON _ = mempty
instance ToJSON NameDetails where
  toJSON (NameDetails mdl occ) = toJSON [toJSON mname,toJSON mid,nsJSON ns,toJSON occs]
    where
      mname = moduleNameString $ moduleName mdl
      mid = unitIdString $ moduleUnitId mdl
      ns = occNameSpace occ
      occs = occNameString occ

safeTyThingId :: TyThing -> Maybe Id
safeTyThingId (AnId i)                    = Just i
safeTyThingId (AConLike (RealDataCon dc)) = Just $ dataConWrapId dc
safeTyThingId _                           = Nothing

-- ---------------------------------------------------------------------

getTypeForName :: Name -> IdeM (Maybe Type)
getTypeForName n = do
  hscEnvRef <- ghcSession <$> readMTS
  mhscEnv <- liftIO $ traverse readIORef hscEnvRef
  case mhscEnv of
    Nothing -> pure Nothing
    Just hscEnv -> liftIO $ getTypeForName_ hscEnv n

getTypeForNameDetails :: NameDetails -> IdeM (Maybe Type)
getTypeForNameDetails (NameDetails mdl occ) = do
  hscEnvRef <- ghcSession <$> readMTS
  mhscEnv <- liftIO $ traverse readIORef hscEnvRef
  case mhscEnv of
    Nothing -> pure Nothing
    Just hscEnv -> do
      nc <- liftIO $ readIORef $ hsc_NC hscEnv
      case lookupOrigNameCache (nsNames nc) mdl occ of
        Nothing -> pure Nothing
        Just n -> liftIO $ getTypeForName_ hscEnv n

getTypeForName_ :: HscEnv -> Name -> IO (Maybe Type)
getTypeForName_ hscEnv n = do
  mt <- (Just <$> lookupGlobal hscEnv n)
          `catch` \(_ :: SomeException) -> return Nothing
  pure $ fmap varType $ safeTyThingId =<< mt

-- ---------------------------------------------------------------------

getSymbolsAtPoint :: Position -> CachedInfo -> [(Range,Name)]
getSymbolsAtPoint pos info = maybe [] (`getArtifactsAtPos` locMap info) $ newPosToOld info pos

-- |Get a symbol from the given location map at the given location.
-- Retrieves the name and range of the symbol at the given location
-- from the cached location map.
symbolFromTypecheckedModule
  :: LocMap
  -> Position
  -> Maybe (Range, Name)
symbolFromTypecheckedModule lm pos =
  case getArtifactsAtPos pos lm of
    (x:_) -> pure x
    []    -> Nothing

-- ---------------------------------------------------------------------

-- | Find the references in the given doc, provided it has been
-- loaded.  If not, return the empty list.
getReferencesInDoc :: Uri -> Position -> IdeDeferM (IdeResult [J.DocumentHighlight])
getReferencesInDoc uri pos =
  pluginGetFile "getReferencesInDoc: " uri $ \file ->
    withCachedModuleAndData file (IdeResultOk []) $
      \tcMod info NMD{inverseNameMap} -> do
        let lm = locMap info
            pm = tm_parsed_module tcMod
            cfile = ml_hs_file $ ms_location $ pm_mod_summary pm
            mpos = newPosToOld info pos
        case mpos of
          Nothing -> return $ IdeResultOk []
          Just pos' -> return $ fmap concat $
            forM (getArtifactsAtPos pos' lm) $ \(_,name) -> do
                let usages = fromMaybe [] $ Map.lookup name inverseNameMap
                    defn = nameSrcSpan name
                    defnInSameFile =
                      (unpackFS <$> srcSpanFileName_maybe defn) == cfile
                    makeDocHighlight :: SrcSpan -> Maybe J.DocumentHighlight
                    makeDocHighlight spn = do
                      let kind = if spn == defn then J.HkWrite else J.HkRead
                      let
                        foo (Left _) = Nothing
                        foo (Right r) = Just r
                      r <- foo $ srcSpan2Range spn
                      r' <- oldRangeToNew info r
                      return $ J.DocumentHighlight r' (Just kind)
                    highlights
                      |    isVarOcc (occName name)
                        && defnInSameFile = mapMaybe makeDocHighlight (defn : usages)
                      | otherwise = mapMaybe makeDocHighlight usages
                return highlights

-- ---------------------------------------------------------------------

showName :: Outputable a => a -> T.Text
showName = T.pack . prettyprint
  where
    prettyprint x = GHC.renderWithStyle GHC.unsafeGlobalDynFlags (GHC.ppr x) style
    style = GHC.mkUserStyle GHC.unsafeGlobalDynFlags GHC.neverQualify GHC.AllTheWay

getModule :: DynFlags -> Name -> Maybe (Maybe T.Text,T.Text)
getModule df n = do
  m <- nameModule_maybe n
  let uid = moduleUnitId m
  let pkg = showName . packageName <$> lookupPackage df uid
  return (pkg, T.pack $ moduleNameString $ moduleName m)

-- ---------------------------------------------------------------------

-- | Return the type definition of the symbol at the given position.
-- Works for Datatypes, Newtypes and Type Definitions, as well as paremterized types.
-- Type Definitions can only be looked up, if the corresponding type is defined in the project.
-- Sum Types can also be searched.
findTypeDef :: Uri -> Position -> IdeDeferM (IdeResult [Location])
findTypeDef uri pos = pluginGetFile "findTypeDef: " uri $ \file ->
  withCachedInfo
    file
    (IdeResultOk []) -- Default result
    (\info -> do
      let rfm    = revMap info
          tmap   = typeMap info
          oldPos = newPosToOld info pos

          -- | Get SrcSpan of the name at the given position.
          -- If the old position is Nothing, e.g. there is no cached info about it,
          -- Nothing is returned.
          --
          -- Otherwise, searches for the Type of the given position
          -- and retrieves its SrcSpan.
          getTypeSrcSpanFromPosition
            :: Maybe Position -> ExceptT () IdeDeferM SrcSpan
          getTypeSrcSpanFromPosition maybeOldPosition = do
            oldPosition <- liftMaybe maybeOldPosition
            let tmapRes = getArtifactsAtPos oldPosition tmap
            case tmapRes of
              [] -> throwError ()
              a  -> do
                -- take last type since this is always the most accurate one
                tyCon <- liftMaybe $ tyConAppTyCon_maybe (snd $ last a)
                case nameSrcSpan (getName tyCon) of
                  UnhelpfulSpan _ -> throwError ()
                  realSpan        -> return realSpan

          liftMaybe :: Monad m => Maybe a -> ExceptT () m a
          liftMaybe val = liftEither $ case val of
            Nothing -> Left ()
            Just s  -> Right s

      runExceptT (getTypeSrcSpanFromPosition oldPos) >>= \case
        Left () -> return $ IdeResultOk []
        Right realSpan ->
          lift $ srcSpanToFileLocation "hare:findTypeDef" rfm realSpan
    )

-- | Return the definition
findDef :: Uri -> Position -> IdeDeferM (IdeResult [Location])
findDef uri pos = pluginGetFile "findDef: " uri $ \file ->
  withCachedInfo file (IdeResultOk []) (\info -> do
    let rfm = revMap info
        lm = locMap info
        mm = moduleMap info
        oldPos = newPosToOld info pos

    case (\x -> Just $ getArtifactsAtPos x mm) =<< oldPos of
      Just ((_,mn):_) -> gotoModule rfm mn
      _ -> case symbolFromTypecheckedModule lm =<< oldPos of
        Nothing -> return $ IdeResultOk []
        Just (_, n) ->
          case nameSrcSpan n of
            UnhelpfulSpan _ -> return $ IdeResultOk []
            realSpan   -> lift $ srcSpanToFileLocation "hare:findDef" rfm realSpan
  )

-- | Resolve the given SrcSpan to a Location in a file.
-- Takes the name of the invoking function for error display.
--
-- If the SrcSpan can not be resolved, an error will be returned.
srcSpanToFileLocation :: T.Text -> (FilePath -> FilePath) -> SrcSpan -> IdeM (IdeResult [Location])
srcSpanToFileLocation invoker rfm srcSpan = do
  -- Since we found a real SrcSpan, try to map it to real files
  res <- srcSpan2Loc rfm srcSpan
  case res of
    Right l@(J.Location luri range) ->
      case uriToFilePath luri of
        Nothing -> return $ IdeResultOk [l]
        Just fp -> ifCachedModule fp (IdeResultOk [l]) $ \(_ :: ParsedModule) info' ->
          case oldRangeToNew info' range of
            Just r  -> return $ IdeResultOk [J.Location luri r]
            Nothing -> return $ IdeResultOk [l]
    Left x -> do
      debugm (T.unpack invoker <> ": name srcspan not found/valid")
      pure (IdeResultFail
            (IdeError PluginError
                      (invoker <> ": \"" <> x <> "\"")
                      Null))

-- | Goto given module.
gotoModule :: (FilePath -> FilePath) -> ModuleName -> IdeDeferM (IdeResult [Location])
gotoModule rfm mn = do
  hscEnvRef <- ghcSession <$> readMTS
  mhscEnv <- liftIO $ traverse readIORef hscEnvRef
  case mhscEnv of
    Just env -> do
      fr <- liftIO $ do
        -- Flush cache or else we get temporary files
        flushFinderCaches env
        findImportedModule env mn Nothing
      case fr of
        Found (ModLocation (Just src) _ _) _ -> do
          fp <- reverseMapFile rfm src

          let r = Range (Position 0 0) (Position 0 0)
              loc = Location (filePathToUri fp) r
          return (IdeResultOk [loc])
        _ -> return (IdeResultOk [])
    Nothing -> return $ IdeResultFail
      (IdeError PluginError "Couldn't get hscEnv when finding import" Null)
-- ---------------------------------------------------------------------

data HarePoint =
  HP { hpFile :: Uri
     , hpPos  :: Position
     } deriving (Eq,Generic,Show)

customOptions :: Int -> J.Options
customOptions n = J.defaultOptions { J.fieldLabelModifier = J.camelTo2 '_' . drop n}

instance FromJSON HarePoint where
  parseJSON = genericParseJSON $ customOptions 2
instance ToJSON HarePoint where
  toJSON = genericToJSON $ customOptions 2

-- ---------------------------------------------------------------------

{-
runGhcModCommand :: IdeGhcM a
                 -> IdeGhcM (IdeResult a)
runGhcModCommand cmd =
  (IdeResultOk <$> cmd) `gcatch`
    \(e :: GM.GhcModError) ->
      return $
      IdeResultFail $
      IdeError PluginError (T.pack $ "hie-ghc-mod: " ++ show e) Null
      -}

-- ---------------------------------------------------------------------

{-
splitCaseCmd :: CommandFunc HarePoint WorkspaceEdit
splitCaseCmd = CmdSync $ \(HP uri pos) -> splitCaseCmd' uri pos

splitCaseCmd' :: Uri -> Position -> IdeGhcM (IdeResult WorkspaceEdit)
splitCaseCmd' uri newPos =
  pluginGetFile "splitCaseCmd: " uri $ \path -> do
    origText <- GM.withMappedFile path $ liftIO . T.readFile
    ifCachedModule path (IdeResultOk mempty) $ \tm info -> runGhcModCommand $
      case newPosToOld info newPos of
        Just oldPos -> do
          let (line, column) = unPos oldPos
          splitResult' <- GM.splits' path tm line column
          case splitResult' of
            Just splitResult -> do
              wEdit <- liftToGhc $ splitResultToWorkspaceEdit origText splitResult
              return $ oldToNewPositions info wEdit
            Nothing -> return mempty
        Nothing -> return mempty
  where

    -- | Transform all ranges in a WorkspaceEdit from old to new positions.
    oldToNewPositions :: CachedInfo -> WorkspaceEdit -> WorkspaceEdit
    oldToNewPositions info wsEdit =
      wsEdit
        & J.documentChanges %~ (>>= traverseOf (traverse . J.edits . traverse . J.range) (oldRangeToNew info))
        & J.changes %~ (>>= traverseOf (traverse . traverse . J.range) (oldRangeToNew info))

    -- | Given the range and text to replace, construct a 'WorkspaceEdit'
    -- by diffing the change against the current text.
    splitResultToWorkspaceEdit :: T.Text -> GM.SplitResult -> IdeM WorkspaceEdit
    splitResultToWorkspaceEdit originalText (GM.SplitResult replaceFromLine replaceFromCol replaceToLine replaceToCol replaceWith) =
      diffText (uri, originalText) newText IncludeDeletions
      where
        before = takeUntil (toPos (replaceFromLine, replaceFromCol)) originalText
        after = dropUntil (toPos (replaceToLine, replaceToCol)) originalText
        newText = before <> replaceWith <> after

    -- | Take the first part of text until the given position.
    -- Returns all characters before the position.
    takeUntil :: Position -> T.Text -> T.Text
    takeUntil (Position l c) txt =
      T.unlines takeLines <> takeCharacters
      where
        textLines = T.lines txt
        takeLines = take l textLines
        takeCharacters = T.take c (textLines !! c)

    -- | Drop the first part of text until the given position.
    -- Returns all characters after and including the position.
    dropUntil :: Position -> T.Text -> T.Text
    dropUntil (Position l c) txt = dropCharacters
      where
        textLines = T.lines txt
        dropLines = drop l textLines
        dropCharacters = T.drop c (T.unlines dropLines)
        -}

getFormattingPlugin :: Config -> IdePlugins -> Maybe (PluginDescriptor, FormattingProvider)
getFormattingPlugin config plugins = do
  let providerName = formattingProvider config
  fmtPlugin <- Map.lookup providerName (ipMap plugins)
  fmtProvider <- pluginFormattingProvider fmtPlugin
  return (fmtPlugin, fmtProvider)
