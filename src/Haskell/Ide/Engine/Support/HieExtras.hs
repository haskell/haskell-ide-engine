{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}
module Haskell.Ide.Engine.Support.HieExtras
  ( getDynFlags
  , WithSnippets(..)
  , getCompletions
  , getTypeForName
  , getSymbolsAtPoint
  , getReferencesInDoc
  , getModule
  , findDef
  , findTypeDef
  , showName
  , safeTyThingId
  , PosPrefixInfo(..)
  , HarePoint(..)
  , customOptions
  , runGhcModCommand
  , splitCaseCmd'
  , splitCaseCmd
  , getFormattingPlugin
  ) where

import           ConLike
import           Control.Lens.Operators                       ( (^?), (?~), (&) )
import           Control.Lens.Prism                           ( _Just )
import           Control.Lens.Setter ((%~))
import           Control.Lens.Traversal (traverseOf)
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Aeson
import qualified Data.Aeson.Types                             as J
import           Data.Char
import           Data.IORef
import qualified Data.List                                    as List
import qualified Data.Map                                     as Map
import           Data.Maybe
import           Data.Monoid                                  ( (<>) )
import qualified Data.Text                                    as T
import qualified Data.Text.IO                                 as T
import           Data.Typeable
import           DataCon
import qualified DynFlags                                     as GHC
import           Exception
import           FastString
import           Finder
import           GHC                                          hiding (getContext)
import           GHC.Generics                                 (Generic)
import qualified GhcMod.Error                                 as GM
import qualified GhcMod.Exe.CaseSplit                         as GM
import qualified GhcMod.Gap                                   as GM
import qualified GhcMod.LightGhc                              as GM
import qualified GhcMod.Utils                                 as GM
import           Haskell.Ide.Engine.ArtifactMap
import           Haskell.Ide.Engine.Config
import           Haskell.Ide.Engine.Context
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import qualified Haskell.Ide.Engine.Support.Fuzzy              as Fuzzy
import           HscTypes
import qualified Language.Haskell.LSP.Types                   as J
import qualified Language.Haskell.LSP.Types.Lens              as J
import           Language.Haskell.Refact.API                 (showGhc)
import           Language.Haskell.Refact.Utils.MonadFunctions
import           Name
import           Outputable                                   (Outputable)
import qualified Outputable                                   as GHC
import           Packages
import           SrcLoc
import           TcEnv
import           Type
import           Var

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

-- ---------------------------------------------------------------------

data CompItem = CI
  { origName     :: Name
  , importedFrom :: T.Text
  , thingType    :: Maybe Type
  , label        :: T.Text
  }

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
  J.CompletionItem label kind (Just $ maybe "" (<>"\n") typeText <> importedFrom)
    Nothing Nothing Nothing Nothing Nothing (Just insertText) (Just J.Snippet)
    Nothing Nothing Nothing Nothing hoogleQuery
  where kind = Just $ occNameToComKind $ occName origName
        hoogleQuery = Just $ toJSON $ mkQuery label importedFrom
        argTypes = maybe [] getArgs thingType
        insertText
          | [] <- argTypes = label
          | otherwise = label <> " " <> argText
        argText :: T.Text
        argText =  mconcat $ List.intersperse " " $ zipWith snippet [1..] argTypes
        stripForall t
          | T.isPrefixOf "forall" t =
            -- We drop 2 to remove the '.' and the space after it
            T.drop 2 (T.dropWhile (/= '.') t)
          | otherwise               = t
        snippet :: Int -> Type -> T.Text
        snippet i t = T.pack $ "${" <> show i <> ":" <> showGhc t <> "}"
        typeText
          | Just t <- thingType = Just . stripForall $ T.pack (showGhc t)
          | otherwise = Nothing
        getArgs :: Type -> [Type]
        getArgs t
          | isPredTy t = []
          | isDictTy t = []
          | isForAllTy t = getArgs $ snd (splitForAllTys t)
          | isFunTy t =
            let (args, ret) = splitFunTys t
              in if isForAllTy ret
                  then getArgs ret
                  else filter (not . isDictTy) args
          | isPiTy t = getArgs $ snd (splitPiTys t)
          | isCoercionTy t = maybe [] (getArgs . snd) (splitCoercionType_maybe t)
          | otherwise = []

mkModCompl :: T.Text -> J.CompletionItem
mkModCompl label =
  J.CompletionItem label (Just J.CiModule) Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing hoogleQuery
  where hoogleQuery = Just $ toJSON $ "module:" <> label

mkExtCompl :: T.Text -> J.CompletionItem
mkExtCompl label =
  J.CompletionItem label (Just J.CiKeyword) Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing

mkPragmaCompl :: T.Text -> T.Text -> J.CompletionItem
mkPragmaCompl label insertText =
  J.CompletionItem label (Just J.CiKeyword) Nothing
    Nothing Nothing Nothing Nothing Nothing (Just insertText) (Just J.Snippet)
    Nothing Nothing Nothing Nothing Nothing

safeTyThingId :: TyThing -> Maybe Id
safeTyThingId (AnId i)                    = Just i
safeTyThingId (AConLike (RealDataCon dc)) = Just $ dataConWrapId dc
safeTyThingId _                           = Nothing

-- Associates a module's qualifier with its members
type QualCompls = Map.Map T.Text [CompItem]

-- | Describes the line at the current cursor position
data PosPrefixInfo = PosPrefixInfo
  { fullLine :: T.Text
    -- ^ The full contents of the line the cursor is at

  , prefixModule :: T.Text
    -- ^ If any, the module name that was typed right before the cursor position.
    --  For example, if the user has typed "Data.Maybe.from", then this property
    --  will be "Data.Maybe"

  , prefixText :: T.Text
    -- ^ The word right before the cursor position, after removing the module part.
    -- For example if the user has typed "Data.Maybe.from",
    -- then this property will be "from"
  , cursorPos :: J.Position
    -- ^ The cursor position
  }

data CachedCompletions = CC
  { allModNamesAsNS :: [T.Text]
  , unqualCompls :: [CompItem]
  , qualCompls :: QualCompls
  , importableModules :: [T.Text]
  , cachedExtensions :: [T.Text]
  } deriving (Typeable)

instance ModuleCache CachedCompletions where
  cacheDataProducer tm _ = do
    let parsedMod = tm_parsed_module tm
        curMod = moduleName $ ms_mod $ pm_mod_summary parsedMod
        Just (_,limports,_,_) = tm_renamed_source tm

        iDeclToModName :: ImportDecl name -> ModuleName
        iDeclToModName = unLoc . ideclName

        showModName :: ModuleName -> T.Text
        showModName = T.pack . moduleNameString

        asNamespace :: ImportDecl name -> ModuleName
        asNamespace imp = maybe (iDeclToModName imp) GHC.unLoc (ideclAs imp)
        -- Full canonical names of imported modules
        importDeclerations = map unLoc limports

        -- The list of all importable Modules from all packages
        moduleNames = map showModName (GM.listVisibleModuleNames (getDynFlags tm))

        -- The given namespaces for the imported modules (ie. full name, or alias if used)
        allModNamesAsNS = map (showModName . asNamespace) importDeclerations

        -- The supported languages and extensions
        languagesAndExts = map T.pack GHC.supportedLanguagesAndExtensions

        typeEnv = md_types $ snd $ tm_internals_ tm
        toplevelVars = mapMaybe safeTyThingId $ typeEnvElts typeEnv
        varToCompl var = CI name (showModName curMod) typ label
          where
            typ = Just $ varType var
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
          return (unquals, Map.fromListWith (++) qualKVs)

        orgUnqualQual hscEnv (prevUnquals, prevQualKVs) (isQual, modQual, modName, hasHiddsMembers) =
          let
            ifUnqual xs = if isQual then prevUnquals else prevUnquals ++ xs
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
                  return $ varType tyid
            return $ ci { thingType = typ }

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
      , importableModules = moduleNames
      , cachedExtensions = languagesAndExts
      }

newtype WithSnippets = WithSnippets Bool

-- | Returns the cached completions for the given module and position.
getCompletions :: Uri -> PosPrefixInfo -> WithSnippets -> IdeM (IdeResult [J.CompletionItem])
getCompletions uri prefixInfo (WithSnippets withSnippets) =
  pluginGetFile "getCompletions: " uri $ \file -> do
    let snippetLens = (^? J.textDocument
                        . _Just
                        . J.completion
                        . _Just
                        . J.completionItem
                        . _Just
                        . J.snippetSupport
                        . _Just)
    supportsSnippets <- fromMaybe False . snippetLens <$> getClientCapabilities
    let toggleSnippets x
          | withSnippets && supportsSnippets = x
          | otherwise = x { J._insertTextFormat = Just J.PlainText
                          , J._insertText       = Nothing
                          }

        PosPrefixInfo { fullLine, prefixModule, prefixText } = prefixInfo
    debugm $ "got prefix" ++ show (prefixModule, prefixText)
    let enteredQual = if T.null prefixModule then "" else prefixModule <> "."
        fullPrefix  = enteredQual <> prefixText

    ifCachedModuleAndData file (IdeResultOk [])
      $ \tm CachedInfo { newPosToOld } CC { allModNamesAsNS, unqualCompls, qualCompls, importableModules, cachedExtensions } ->
          let
            -- default to value context if no explicit context
            context = fromMaybe ValueContext $ getContext pos (tm_parsed_module tm)

            {- correct the position by moving 'foo :: Int -> String ->    '
                                                                         ^
               to                             'foo :: Int -> String ->    '
                                                                   ^
            -}
            pos =
              let newPos = cursorPos prefixInfo
                  Position l c = fromMaybe newPos (newPosToOld newPos)
                  typeStuff = [isSpace, (`elem` (">-." :: String))]
                  stripTypeStuff = T.dropWhileEnd (\x -> any (\f -> f x) typeStuff)
                  -- if oldPos points to
                  -- foo -> bar -> baz
                  --    ^
                  -- Then only take the line up to there, discard '-> bar -> baz'
                  partialLine = T.take c fullLine
                  -- drop characters used when writing incomplete type sigs
                  -- like '-> '
                  d = T.length fullLine - T.length (stripTypeStuff partialLine)
              in Position l (c - d)

            filtModNameCompls =
              map mkModCompl
                $ mapMaybe (T.stripPrefix enteredQual)
                $ Fuzzy.simpleFilter fullPrefix allModNamesAsNS

            filtCompls = Fuzzy.filterBy label prefixText ctxCompls
              where
                isTypeCompl = isTcOcc . occName . origName
                -- completions specific to the current context
                ctxCompls = case context of
                              TypeContext -> filter isTypeCompl compls
                              ValueContext -> filter (not . isTypeCompl) compls
                compls = if T.null prefixModule
                  then unqualCompls
                  else Map.findWithDefault [] prefixModule qualCompls

            mkImportCompl label = (J.detail ?~ label) . mkModCompl $ fromMaybe
              ""
              (T.stripPrefix enteredQual label)

            filtListWith f list =
              [ f label
              | label <- Fuzzy.simpleFilter fullPrefix list
              , enteredQual `T.isPrefixOf` label
              ]

            filtListWithSnippet f list suffix =
              [ toggleSnippets (f label (snippet <> suffix))
              | (snippet, label) <- list
              , Fuzzy.test fullPrefix label
              ]

            filtImportCompls = filtListWith mkImportCompl importableModules
            filtPragmaCompls = filtListWithSnippet mkPragmaCompl validPragmas
            filtOptsCompls   = filtListWith mkExtCompl

            stripLeading :: Char -> String -> String
            stripLeading _ [] = []
            stripLeading c (s:ss)
              | s == c = ss
              | otherwise = s:ss

            result
              | "import " `T.isPrefixOf` fullLine
              = filtImportCompls
              | "{-# language" `T.isPrefixOf` T.toLower fullLine
              = filtOptsCompls cachedExtensions
              | "{-# options_ghc" `T.isPrefixOf` T.toLower fullLine
              = filtOptsCompls (map (T.pack . stripLeading '-') $ GHC.flagsForCompletion False)
              | "{-# " `T.isPrefixOf` fullLine
              = filtPragmaCompls (pragmaSuffix fullLine)
              | otherwise
              = filtModNameCompls ++ map (toggleSnippets . mkCompl . stripAutoGenerated) filtCompls
          in
            return $ IdeResultOk result
 where
  validPragmas :: [(T.Text, T.Text)]
  validPragmas =
    [ ("LANGUAGE ${1:extension}"        , "LANGUAGE")
    , ("OPTIONS_GHC -${1:option}"       , "OPTIONS_GHC")
    , ("INLINE ${1:function}"           , "INLINE")
    , ("NOINLINE ${1:function}"         , "NOINLINE")
    , ("INLINABLE ${1:function}"        , "INLINABLE")
    , ("WARNING ${1:message}"           , "WARNING")
    , ("DEPRECATED ${1:message}"        , "DEPRECATED")
    , ("ANN ${1:annotation}"            , "ANN")
    , ("RULES"                          , "RULES")
    , ("SPECIALIZE ${1:function}"       , "SPECIALIZE")
    , ("SPECIALIZE INLINE ${1:function}", "SPECIALIZE INLINE")
    ]

  pragmaSuffix :: T.Text -> T.Text
  pragmaSuffix fullLine
    |  "}" `T.isSuffixOf` fullLine = mempty
    | otherwise = " #-}"

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
  mHscEnv <- liftIO $ traverse readIORef hscEnvRef
  case mHscEnv of
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

runGhcModCommand :: IdeGhcM a
                 -> IdeGhcM (IdeResult a)
runGhcModCommand cmd =
  (IdeResultOk <$> cmd) `gcatch`
    \(e :: GM.GhcModError) ->
      return $
      IdeResultFail $
      IdeError PluginError (T.pack $ "hie-ghc-mod: " ++ show e) Null

-- ---------------------------------------------------------------------

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

-- ---------------------------------------------------------------------

-- | Under certain circumstance GHC generates some extra stuff that we
-- don't want in the autocompleted symbols
stripAutoGenerated :: CompItem -> CompItem
stripAutoGenerated ci =
    ci {label = stripPrefix (label ci)}
    {- When e.g. DuplicateRecordFields is enabled, compiler generates
    names like "$sel:accessor:One" and "$sel:accessor:Two" to disambiguate record selectors
    https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/DuplicateRecordFields#Implementation
    -}

-- TODO: Turn this into an alex lexer that discards prefixes as if they were whitespace.

stripPrefix :: T.Text -> T.Text
stripPrefix name = T.takeWhile (/=':') $ go prefixes
  where
    go [] = name
    go (p:ps)
      | T.isPrefixOf p name = T.drop (T.length p) name
      | otherwise = go ps

-- | Prefixes that can occur in a GHC OccName
prefixes :: [T.Text]
prefixes =
  [
    -- long ones
    "$con2tag_"
  , "$tag2con_"
  , "$maxtag_"

  -- four chars
  , "$sel:"
  , "$tc'"

  -- three chars
  , "$dm"
  , "$co"
  , "$tc"
  , "$cp"
  , "$fx"

  -- two chars
  , "$W"
  , "$w"
  , "$m"
  , "$b"
  , "$c"
  , "$d"
  , "$i"
  , "$s"
  , "$f"
  , "$r"
  , "C:"
  , "N:"
  , "D:"
  , "$p"
  , "$L"
  , "$f"
  , "$t"
  , "$c"
  , "$m"
  ]

-- ---------------------------------------------------------------------

getFormattingPlugin :: Config -> IdePlugins -> Maybe (PluginDescriptor, FormattingProvider)
getFormattingPlugin config plugins = do 
  let providerName = formattingProvider config
  fmtPlugin <- Map.lookup providerName (ipMap plugins)
  fmtProvider <- pluginFormattingProvider fmtPlugin
  return (fmtPlugin, fmtProvider)
