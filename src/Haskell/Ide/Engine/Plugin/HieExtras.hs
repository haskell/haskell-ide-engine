{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
module Haskell.Ide.Engine.Plugin.HieExtras
  ( getDynFlags
  , getSymbols
  , getCompletions
  , getTypeForName
  , getSymbolsAtPoint
  , getReferencesInDoc
  , getModule
  , findDef
  , showName
  ) where

import           ConLike
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Either
import           Data.IORef
import qualified Data.List                                    as List
import qualified Data.Map                                     as Map
import           Data.Maybe
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Text                                    as T
import           Data.Typeable
import           DataCon
import           Exception
import           FastString
import           GHC
import qualified GhcMod.Error                                 as GM
import qualified GhcMod.Gap                                   as GM
import qualified GhcMod.LightGhc                              as GM
import qualified GhcMod.Monad                                 as GM
import           Haskell.Ide.Engine.ArtifactMap
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Plugin.GhcMod            (setTypecheckedModule)
import qualified Haskell.Ide.Engine.Plugin.Fuzzy              as Fuzzy
import           HscTypes
import qualified Language.Haskell.LSP.Types                   as J
import           Language.Haskell.Refact.API                 (showGhc, showGhcQual, setGhcContext, hsNamessRdr)
import           Language.Haskell.Refact.Utils.MonadFunctions
import           Module                                       hiding (getModule)
import           Name
import           Outputable                                   (Outputable)
import qualified Outputable                                   as GHC
import qualified DynFlags                                     as GHC
import           Packages
import           SrcLoc
import           TcEnv
import           Var

getDynFlags :: Uri -> IdeM (IdeResponse DynFlags)
getDynFlags uri =
  pluginGetFile "getDynFlags: " uri $ \fp -> do
      mcm <- getCachedModule fp
      return $ case mcm of
        ModuleCached cm _ -> IdeResponseOk $ ms_hspp_opts $ pm_mod_summary $ tm_parsed_module $ tcMod cm
        _ -> IdeResponseFail $
          IdeError PluginError ("getDynFlags: \"" <> "module not loaded" <> "\"") Null

-- ---------------------------------------------------------------------

nonExistentCacheErr :: String -> IdeResponse a
nonExistentCacheErr meth =
  IdeResponseFail $
    IdeError PluginError
             (T.pack $ meth <> ": \"" <> "module not loaded" <> "\"")
             Null

someErr :: String -> String -> IdeResponse a
someErr meth err =
  IdeResponseFail $
    IdeError PluginError
             (T.pack $ meth <> ": " <> err)
             Null

-- ---------------------------------------------------------------------

data NameMapData = NMD
  { inverseNameMap ::  !(Map.Map Name [SrcSpan])
  } deriving (Typeable)

invert :: (Ord v) => Map.Map k v -> Map.Map v [k]
invert m = Map.fromListWith (++) [(v,[k]) | (k,v) <- Map.toList m]

instance ModuleCache NameMapData where
  cacheDataProducer cm = pure $ NMD inm
    where nm  = initRdrNameMap $ tcMod cm
          inm = invert nm

-- ---------------------------------------------------------------------

getSymbols :: Uri -> (IdeResponse [J.SymbolInformation] -> IdeM ()) -> IdeM ()
getSymbols uri callback =
  case uriToFilePath uri of 
    Nothing -> callback $ IdeResponseFail (IdeError PluginError ("getSymbols" <> "Couldn't resolve uri" <> getUri uri) Null)
    Just file -> do
      mcm <- getCachedModule file
      case mcm of
        ModuleCached cm _ -> respond cm
        ModuleLoading -> queueActionForModule file respond
        --TODO: add ModuleFailed
        _ -> callback $ IdeResponseOk []
  where respond cm = getSymbolsFromModule cm >>= callback
    
getSymbolsFromModule :: CachedModule -> IdeM (IdeResponse [J.SymbolInformation])
getSymbolsFromModule cm = do
  let tm = tcMod cm
      rfm = revMap cm
      hsMod = unLoc $ pm_parsed_source $ tm_parsed_module tm
      imports = hsmodImports hsMod
      imps  = concatMap (goImport . unLoc) imports
      decls = concatMap (go . unLoc) $ hsmodDecls hsMod
      s x = showName <$> x

      go :: HsDecl GM.GhcPs -> [(J.SymbolKind,Located T.Text,Maybe T.Text)]
      go (TyClD FamDecl { tcdFam = FamilyDecl { fdLName = n } }) = pure (J.SkClass, s n, Nothing)
      go (TyClD SynDecl { tcdLName = n }) = pure (J.SkClass, s n, Nothing)
      go (TyClD DataDecl { tcdLName = n, tcdDataDefn = HsDataDefn { dd_cons = cons } }) =
        (J.SkClass, s n, Nothing) : concatMap (processCon (unLoc $ s n) . unLoc) cons
      go (TyClD ClassDecl { tcdLName = n, tcdSigs = sigs, tcdATs = fams }) =
        (J.SkInterface, sn, Nothing) :
              concatMap (processSig (unLoc sn) . unLoc) sigs
          ++  concatMap (map setCnt . go . TyClD . FamDecl . unLoc) fams
        where sn = s n
              setCnt (k,n',_) = (k,n',Just (unLoc sn))
      go (ValD FunBind { fun_id = ln }) = pure (J.SkFunction, s ln, Nothing)
      go (ValD PatBind { pat_lhs = p }) =
        map (\n ->(J.SkMethod, s n, Nothing)) $ hsNamessRdr p
      go (ForD ForeignImport { fd_name = n }) = pure (J.SkFunction, s n, Nothing)
      go _ = []

      processSig :: T.Text
                -> Sig GM.GhcPs
                -> [(J.SymbolKind, Located T.Text, Maybe T.Text)]
      processSig cnt (ClassOpSig False names _) =
        map (\n ->(J.SkMethod,s n, Just cnt)) names
      processSig _ _ = []

      processCon :: T.Text
                -> ConDecl GM.GhcPs
                -> [(J.SymbolKind, Located T.Text, Maybe T.Text)]
      processCon cnt ConDeclGADT { con_names = names } =
        map (\n -> (J.SkConstructor, s n, Just cnt)) names
      processCon cnt ConDeclH98 { con_name = name, con_details = dets } =
        (J.SkConstructor, sn, Just cnt) : xs
        where
          sn = s name
          xs = case dets of
            RecCon (L _ rs) -> concatMap (map (f . rdrNameFieldOcc . unLoc)
                                        . cd_fld_names
                                        . unLoc) rs
                                where f ln = (J.SkField, s ln, Just (unLoc sn))
            _ -> []

      goImport :: ImportDecl GM.GhcPs -> [(J.SymbolKind, Located T.Text, Maybe T.Text)]
      goImport ImportDecl { ideclName = lmn, ideclAs = as, ideclHiding = meis } = a ++ xs
        where
          im = (J.SkModule, lsmn, Nothing)
          lsmn = s lmn
          smn = unLoc lsmn
          a = case as of
                    Just a' -> [(J.SkNamespace, lsmn, Just $ showName a')]
                    Nothing -> [im]
          xs = case meis of
                Just (False, eis) -> concatMap (f . unLoc) (unLoc eis)
                _ -> []
          f (IEVar n) = pure (J.SkFunction, s n, Just smn)
          f (IEThingAbs n) = pure (J.SkClass, s n, Just smn)
          f (IEThingAll n) = pure (J.SkClass, s n, Just smn)
          f (IEThingWith n _ vars fields) =
            let sn = s n in
            (J.SkClass, sn, Just smn) :
                map (\n' -> (J.SkFunction, s n', Just (unLoc sn))) vars
              ++ map (\f' -> (J.SkField   , s f', Just (unLoc sn))) fields
          f _ = []

      declsToSymbolInf :: (J.SymbolKind, Located T.Text, Maybe T.Text)
                      -> IdeM (Either T.Text J.SymbolInformation)
      declsToSymbolInf (kind, L l nameText, cnt) = do
        eloc <- srcSpan2Loc rfm l
        case eloc of
          Left x -> return $ Left x
          Right loc -> return $ Right $ J.SymbolInformation nameText kind loc cnt
  symInfs <- mapM declsToSymbolInf (imps ++ decls)
  return $ IdeResponseOk $ rights symInfs

-- ---------------------------------------------------------------------

data CompItem = CI
  { origName     :: Name
  , importedFrom :: T.Text
  , thingType    :: Maybe T.Text
  , label        :: T.Text
  } deriving (Show)

instance Eq CompItem where
  (CI n1 _ _ _) == (CI n2 _ _ _) = n1 == n2

instance Ord CompItem where
  compare (CI n1 _ _ _) (CI n2 _ _ _) = compare n1 n2

occNameToComKind :: OccName -> J.CompletionItemKind
occNameToComKind oc
  | isVarOcc  oc = J.CiFunction
  | isTcOcc   oc = J.CiClass
  | isDataOcc oc = J.CiConstructor
  | otherwise    = J.CiVariable

type HoogleQuery = T.Text

mkQuery :: T.Text -> T.Text -> HoogleQuery
mkQuery name importedFrom = name <> " module:" <> importedFrom
                                 <> " is:exact"

mkCompl :: CompItem -> J.CompletionItem
mkCompl CI{origName,importedFrom,thingType,label} =
  J.CompletionItem label kind (Just $ maybe "" (<>"\n") thingType <> importedFrom)
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing hoogleQuery
  where kind  = Just $ occNameToComKind $ occName origName
        hoogleQuery = Just $ toJSON $ mkQuery label importedFrom

mkModCompl :: T.Text -> J.CompletionItem
mkModCompl label =
  J.CompletionItem label (Just J.CiModule) Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing hoogleQuery
  where hoogleQuery = Just $ toJSON $ "module:" <> label

safeTyThingId :: TyThing -> Maybe Id
safeTyThingId (AnId i)                    = Just i
safeTyThingId (AConLike (RealDataCon dc)) = Just $ dataConWrapId dc
safeTyThingId _                           = Nothing

-- Associates a module's qualifier with its members
type QualCompls = Map.Map T.Text [CompItem]

data CachedCompletions = CC
  { allModNamesAsNS :: [T.Text]
  , unqualCompls :: [CompItem]
  , qualCompls :: QualCompls
  } deriving (Typeable)

instance ModuleCache CachedCompletions where
  cacheDataProducer cm = do
    let tm = tcMod cm
        parsedMod = tm_parsed_module tm
        curMod = moduleName $ ms_mod $ pm_mod_summary parsedMod
        Just (_,limports,_,_) = tm_renamed_source tm

        iDeclToModName :: ImportDecl name -> ModuleName
        iDeclToModName = unLoc . ideclName

        showModName :: ModuleName -> T.Text
        showModName = T.pack . moduleNameString

#if __GLASGOW_HASKELL__ >= 802
        asNamespace :: ImportDecl name -> ModuleName
        asNamespace imp = fromMaybe (iDeclToModName imp) (fmap GHC.unLoc $ ideclAs imp)
#else
        asNamespace :: ImportDecl name -> ModuleName
        asNamespace imp = fromMaybe (iDeclToModName imp) (ideclAs imp)
#endif
        -- Full canonical names of imported modules
        importDeclerations = map unLoc limports

        -- The given namespaces for the imported modules (ie. full name, or alias if used)
        allModNamesAsNS = map (showModName . asNamespace) importDeclerations

        typeEnv = md_types $ snd $ tm_internals_ tm
        toplevelVars = mapMaybe safeTyThingId $ typeEnvElts typeEnv
        varToCompl var = CI name (showModName curMod) typ label
          where
            typ = Just $ T.pack $ showGhc $ varType var
            name = Var.varName var
            label = T.pack $ showGhc name

        toplevelCompls = map varToCompl toplevelVars

        toCompItem :: ModuleName -> Name -> CompItem
        toCompItem mn n =
          CI n (showModName mn) Nothing (T.pack $ showGhc n)

        allImportsInfo :: [(Bool, T.Text, ModuleName, Maybe (Bool, [Name]))]
        allImportsInfo = map getImpInfo importDeclerations
          where
            getImpInfo imp =
              let modName = iDeclToModName imp
                  modQual = showModName (asNamespace imp)
                  isQual = ideclQualified imp
                  hasHiddsMembers =
                    case ideclHiding imp of
                      Nothing -> Nothing
                      Just (hasHiddens, L _ liens) ->
                        Just (hasHiddens, concatMap (ieNames . unLoc) liens)
              in (isQual, modQual, modName, hasHiddsMembers)

        getModCompls :: GhcMonad m => HscEnv -> m ([CompItem], QualCompls)
        getModCompls hscEnv = do
          (unquals, qualKVs) <- foldM (orgUnqualQual hscEnv) ([], []) allImportsInfo
          return (unquals, Map.fromList qualKVs)

        orgUnqualQual hscEnv (prevUnquals, prevQualKVs) (isQual, modQual, modName, hasHiddsMembers) =
          let
            ifUnqual xs = if isQual then prevUnquals else (prevUnquals ++ xs)
            setTypes = setComplsType hscEnv
          in
            case hasHiddsMembers of
              Just (False, members) -> do
                compls <- setTypes (map (toCompItem modName) members)
                return
                  ( ifUnqual compls
                  , (modQual, compls) : prevQualKVs
                  )
              Just (True , members) -> do
                let hiddens = map (toCompItem modName) members
                allCompls <- getComplsFromModName modName
                compls <- setTypes (allCompls List.\\ hiddens)
                return
                  ( ifUnqual compls
                  , (modQual, compls) : prevQualKVs
                  )
              Nothing -> do
                -- debugm $ "///////// Nothing " ++ (show modQual)
                compls <- setTypes =<< getComplsFromModName modName
                return
                  ( ifUnqual compls
                  , (modQual, compls) : prevQualKVs
                  )

        getComplsFromModName :: GhcMonad m
          => ModuleName -> m [CompItem]
        getComplsFromModName mn = do
          mminf <- getModuleInfo =<< findModule mn Nothing
          return $ case mminf of
            Nothing -> []
            Just minf -> map (toCompItem mn) $ modInfoExports minf

        setComplsType :: (Traversable t, MonadIO m)
          => HscEnv -> t CompItem -> m (t CompItem)
        setComplsType hscEnv xs =
          liftIO $ forM xs $ \ci@CI{origName} -> do
            mt <- (Just <$> lookupGlobal hscEnv origName)
                    `catch` \(_ :: SourceError) -> return Nothing
            let typ = do
                  t <- mt
                  tyid <- safeTyThingId t
                  return $ T.pack $ showGhc $ varType tyid
            return $ ci {thingType = typ}

    hscEnvRef <- ghcSession <$> readMTS
    hscEnv <- liftIO $ traverse readIORef hscEnvRef
    (unquals, quals) <- maybe
                          (pure ([], Map.empty))
                          (\env -> GM.runLightGhc env (getModCompls env))
                          hscEnv
    return $ CC
      { allModNamesAsNS = allModNamesAsNS
      , unqualCompls = toplevelCompls ++ unquals
      , qualCompls = quals
      }

getCompletions :: Uri -> (T.Text, T.Text) -> IdeM (IdeResponse [J.CompletionItem])
getCompletions uri (qualifier, ident) = pluginGetFile "getCompletions: " uri $ \file ->
  let handlers =
        [ GM.GHandler $ \(ex :: SomeException) ->
            return $ someErr "getCompletions" (show ex)
        ]
  in flip GM.gcatches handlers $ do
    -- debugm $ "got prefix" ++ show (qualifier, ident)
    let noCache = return $ nonExistentCacheErr "getCompletions"
        enteredQual = if T.null qualifier then "" else qualifier <> "."
        fullPrefix = enteredQual <> ident
    withCachedModuleAndData file noCache $
      \_ CC
        { allModNamesAsNS
        , unqualCompls
        , qualCompls
        } -> do
          let
            filtModNameCompls = map mkModCompl
              $ mapMaybe (T.stripPrefix enteredQual)
              $ Fuzzy.simpleFilter fullPrefix allModNamesAsNS

            filtCompls = Fuzzy.filterBy label ident compls
              where
                compls = if T.null qualifier
                  then unqualCompls
                  else Map.findWithDefault [] qualifier qualCompls

          return $ IdeResponseOk $ filtModNameCompls ++ map mkCompl filtCompls

-- ---------------------------------------------------------------------

getTypeForName :: Name -> IdeM (Maybe Type)
getTypeForName n = do
  hscEnvRef <- ghcSession <$> readMTS
  mhscEnv <- liftIO $ traverse readIORef hscEnvRef
  case mhscEnv of
    Nothing -> return Nothing
    Just hscEnv -> do
      mt <- liftIO $ (Just <$> lookupGlobal hscEnv n)
                        `catch` \(_ :: SomeException) -> return Nothing
      return $ fmap varType $ safeTyThingId =<< mt

-- ---------------------------------------------------------------------

getSymbolsAtPoint :: Uri -> Position -> IdeM (IdeResponse [(Range, Name)])
getSymbolsAtPoint uri pos = pluginGetFile "getSymbolsAtPoint: " uri $ \file -> do
  let noCache = return $ IdeResponseOk [] -- Processing hover, no symbols available, not an error
  withCachedModule file noCache $
    return . IdeResponseOk . getSymbolsAtPointPure pos

getSymbolsAtPointPure :: Position -> CachedModule -> [(Range,Name)]
getSymbolsAtPointPure pos cm = maybe [] (`getArtifactsAtPos` locMap cm) $ newPosToOld cm pos

symbolFromTypecheckedModule
  :: LocMap
  -> Position
  -> Maybe (Range, Name)
symbolFromTypecheckedModule lm pos =
  case getArtifactsAtPos pos lm of
    (x:_) -> pure x
    []    -> Nothing

-- ---------------------------------------------------------------------

getReferencesInDoc :: Uri -> Position -> IdeM (IdeResponse [J.DocumentHighlight])
getReferencesInDoc uri pos = pluginGetFile "getReferencesInDoc: " uri $ \file -> do
  let noCache = return $ IdeResponseOk [] -- Processing doc highlights request, no symbols available, not an error
  withCachedModuleAndData file noCache $
    \cm NMD{inverseNameMap} -> runExceptT $ do
      let lm = locMap cm
          pm = tm_parsed_module $ tcMod cm
          cfile = ml_hs_file $ ms_location $ pm_mod_summary pm
          mpos = newPosToOld cm pos
      case mpos of
        Nothing -> return []
        Just pos' -> fmap concat $
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
                    r' <- oldRangeToNew cm r
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
#if __GLASGOW_HASKELL__ >= 802
    style = (GHC.mkUserStyle GHC.unsafeGlobalDynFlags GHC.neverQualify GHC.AllTheWay)
#else
    style = (GHC.mkUserStyle GHC.neverQualify GHC.AllTheWay)
#endif

getModule :: DynFlags -> Name -> Maybe (Maybe T.Text,T.Text)
getModule df n = do
  m <- nameModule_maybe n
  let uid = moduleUnitId m
  let pkg = showName . packageName <$> lookupPackage df uid
  return (pkg, T.pack $ moduleNameString $ moduleName m)

-- ---------------------------------------------------------------------

getNewNames :: GhcMonad m => Name -> m [Name]
getNewNames old = do
  let eqModules (Module pk1 mn1) (Module pk2 mn2) = mn1 == mn2 && pk1 == pk2
  gnames <- GHC.getNamesInScope
  let clientModule = GHC.nameModule old
  let clientInscopes = filter (\n -> eqModules clientModule (GHC.nameModule n)) gnames
  let newNames = filter (\n -> showGhcQual n == showGhcQual old) clientInscopes
  return newNames

findDef :: Uri -> Position -> IdeGhcM (IdeResponse [Location])
findDef uri pos = pluginGetFile "findDef: " uri $ \file -> do
  let noCache = return $ nonExistentCacheErr "hare:findDef"
  withCachedModule file noCache $
    \cm -> do
      let rfm = revMap cm
          lm = locMap cm
      case symbolFromTypecheckedModule lm =<< newPosToOld cm pos of
        Nothing -> return $ IdeResponseOk []
        Just pn -> do
          let n = snd pn
          case nameSrcSpan n of
            UnhelpfulSpan _ -> return $ IdeResponseOk []
            realSpan   -> do
              res <- srcSpan2Loc rfm realSpan
              case res of
                Right l@(J.Location luri range) ->
                  case oldRangeToNew cm range of
                    Just r  -> return $ IdeResponseOk [J.Location luri r]
                    Nothing -> return $ IdeResponseOk [l]
                Left x -> do
                  let failure = pure (IdeResponseFail
                                        (IdeError PluginError
                                                  ("hare:findDef" <> ": \"" <> x <> "\"")
                                                  Null))
                  case nameModule_maybe n of
                    Just m -> do
                      let mName = moduleName m
                      b <- GM.unGmlT $ isLoaded mName
                      if b then do
                        mLoc <- GM.unGmlT $ ms_location <$> getModSummary mName
                        case ml_hs_file mLoc of
                          Just fp -> do
                            cfp <- reverseMapFile rfm fp
                            mcm' <- getCachedModule cfp
                            rcm' <- case mcm' of
                              ModuleCached cmdl stale -> do
                                debugm "module already in cache in findDef"
                                return $ ModuleCached cmdl stale
                              _ -> do
                                debugm "setting cached module in findDef"
                                _ <- setTypecheckedModule $ filePathToUri cfp
                                getCachedModule cfp
                            case rcm' of
                              ModuleCached cm' _ -> do
                                let modSum = pm_mod_summary $ tm_parsed_module $ tcMod cm'
                                    rfm'   = revMap cm'
                                newNames <- GM.unGmlT $ do
                                  setGhcContext modSum
                                  getNewNames n
                                eithers <- mapM (srcSpan2Loc rfm' . nameSrcSpan) newNames
                                case rights eithers of
                                  (l:_) -> return $ IdeResponseOk [l]
                                  []    -> failure
                              _ ->
                                return
                                  $ IdeResponseFail
                                  $ IdeError PluginError ("hare:findDef: failed to load module for " <> T.pack cfp) Null
                          Nothing -> failure
                        else failure
                    Nothing -> failure
