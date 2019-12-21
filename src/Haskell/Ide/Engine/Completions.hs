{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings      #-}
module Haskell.Ide.Engine.Completions
  ( WithSnippets(..)
  , getCompletions
  , resolveCompletion
  )
where

import           Control.Lens.Operators         ( (.~)
                                                , (^.)
                                                , (^?)
                                                , (?~)
                                                , (&)
                                                )
import           Control.Lens.Prism             ( _Just )

import           Data.Aeson
import qualified Data.Aeson.Types              as J
import           Data.Char
import qualified Data.List                     as List
import qualified Data.Text                     as T
import qualified Data.Map                      as Map
import           Data.Maybe
#if __GLASGOW_HASKELL__ < 808
import           Data.Semigroup (Semigroup(..))
#endif
import           Data.Typeable
import           GHC.Generics                   ( Generic )


import           HscTypes
import qualified DynFlags                      as GHC
import           GHC                     hiding ( getContext )
import           RdrName
import           Name
import           TcRnTypes
import           Type
import           Var
import           Packages (listVisibleModuleNames)


-- import           Language.Haskell.Refact.API    ( showGhc )

import qualified Language.Haskell.LSP.Types    as J
import qualified Language.Haskell.LSP.Types.Capabilities
                                               as J
import qualified Language.Haskell.LSP.Types.Lens
                                               as J
import qualified Haskell.Ide.Engine.Support.Fuzzy
                                               as Fuzzy
import qualified Haskell.Ide.Engine.Support.Hoogle
                                               as Hoogle
import qualified Language.Haskell.LSP.VFS      as VFS

import           Haskell.Ide.Engine.Support.HieExtras
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Context

import           Language.Haskell.GHC.ExactPrint.Utils

-- ---------------------------------------------------------------------

data CompItem = CI
  { origName     :: Name           -- ^ Original name, such as Maybe, //, or find.
  , importedFrom :: T.Text         -- ^ From where this item is imported from.
  , thingType    :: Maybe Type     -- ^ Available type information.
  , label        :: T.Text         -- ^ Label to display to the user.
  , isInfix      :: Maybe Backtick -- ^ Did the completion happen
                                   -- in the context of an infix notation.
  }

data Backtick = Surrounded | LeftSide

instance Eq CompItem where
  ci1 == ci2 = origName ci1 == origName ci2

instance Ord CompItem where
  compare ci1 ci2 = origName ci1 `compare` origName ci2

occNameToComKind :: OccName -> J.CompletionItemKind
occNameToComKind oc
  | isVarOcc  oc = J.CiFunction
  | isTcOcc   oc = J.CiClass
  | isDataOcc oc = J.CiConstructor
  | otherwise    = J.CiVariable

type HoogleQuery = T.Text
data CompItemResolveData
  = CompItemResolveData
  { nameDetails :: Maybe NameDetails
  , hoogleQuery :: HoogleQuery
  } deriving (Eq,Generic)

instance FromJSON CompItemResolveData where
  parseJSON = genericParseJSON $ customOptions 0
instance ToJSON CompItemResolveData where
  toJSON = genericToJSON $ customOptions 0

resolveCompletion :: WithSnippets -> J.CompletionItem -> IdeM J.CompletionItem
resolveCompletion withSnippets origCompl =
  case fromJSON <$> origCompl ^. J.xdata of
    Just (J.Success compdata) -> do
      mdocs <- Hoogle.info $ hoogleQuery compdata
      let docText = case mdocs of
            Right x -> Just x
            _ -> Nothing
          markup = J.MarkupContent J.MkMarkdown <$> docText
          docs = J.CompletionDocMarkup <$> markup
      (detail,insert) <- case nameDetails compdata of
        Nothing -> pure (Nothing,Nothing)
        Just nd -> do
          mtyp <- getTypeForNameDetails nd
          case mtyp of
            Nothing -> pure (Nothing, Nothing)
            Just typ -> do
              let label = origCompl ^. J.label
                  insertText = label <> " " <> getArgText typ
                  det = Just . stripForall $ T.pack (showGhc typ) <> "\n"
              pure (det,Just insertText)
      let compl = origCompl & J.documentation .~ docs
                         & J.insertText .~ insert
                         & J.insertTextFormat ?~ J.Snippet
                         & J.detail .~ (detail <> origCompl ^. J.detail)
      toggleSnippets <$> getClientCapabilities <*> pure withSnippets <*> pure compl
    Just (J.Error err) -> do
      debugm $ "resolveCompletion: Decoding data failed because of: " ++ err
      pure origCompl
    _ -> pure origCompl

mkQuery :: T.Text -> T.Text -> HoogleQuery
mkQuery name importedFrom = name <> " module:" <> importedFrom
                                 <> " is:exact"

mkCompl :: CompItem -> J.CompletionItem
mkCompl CI{origName,importedFrom,thingType,label,isInfix} =
  J.CompletionItem label kind (Just $ maybe "" (<>"\n") typeText <> importedFrom)
    Nothing Nothing Nothing Nothing Nothing (Just insertText) (Just J.Snippet)
    Nothing Nothing Nothing Nothing resolveData
  where kind = Just $ occNameToComKind $ occName origName
        resolveData = Just $ toJSON $ CompItemResolveData nameDets hoogleQuery
        hoogleQuery = mkQuery label importedFrom
        insertText = case isInfix of
            Nothing -> case getArgText <$> thingType of
                            Nothing -> label
                            Just argText -> label <> " " <> argText
            Just LeftSide -> label <> "`"

            Just Surrounded -> label
        typeText
          | Just t <- thingType = Just . stripForall $ T.pack (showGhc t)
          | otherwise = Nothing
        nameDets =
          case (thingType, nameModule_maybe origName) of
            (Just _,_) -> Nothing
            (Nothing, Nothing) -> Nothing
            (Nothing, Just mdl) -> Just (NameDetails mdl (nameOccName origName))

stripForall :: T.Text -> T.Text
stripForall t
  | T.isPrefixOf "forall" t =
    -- We drop 2 to remove the '.' and the space after it
    T.drop 2 (T.dropWhile (/= '.') t)
  | otherwise               = t

getArgText :: Type -> T.Text
getArgText typ = argText
  where
    argTypes = getArgs typ
    argText :: T.Text
    argText =  mconcat $ List.intersperse " " $ zipWith snippet [1..] argTypes
    snippet :: Int -> Type -> T.Text
    snippet i t = T.pack $ "${" <> show i <> ":" <> showGhc t <> "}"
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
    Nothing Nothing Nothing Nothing (Just $ toJSON resolveData)
  where hoogleQuery = "module:" <> label
        resolveData = Just $ CompItemResolveData Nothing hoogleQuery

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

-- Associates a module's qualifier with its members
newtype QualCompls = QualCompls { getQualCompls :: Map.Map T.Text [CompItem] }

instance Semigroup QualCompls where
  (QualCompls a) <> (QualCompls b) = QualCompls $ Map.unionWith (++) a b

instance Monoid QualCompls where
  mempty = QualCompls Map.empty
  mappend = (<>)

data CachedCompletions = CC
  { allModNamesAsNS :: [T.Text] -- ^ All module names in scope.
                                -- Prelude is a single module
  , unqualCompls :: [CompItem]  -- ^ All Possible completion items
  , qualCompls :: QualCompls    -- ^ Completion items associated to
                                -- to a specific module name.
  , importableModules :: [T.Text] -- ^ All modules that may be imported.
  } deriving (Typeable)

-- The supported languages and extensions
languagesAndExts :: [T.Text]
languagesAndExts = map T.pack GHC.supportedLanguagesAndExtensions

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
        moduleNames = map showModName (listVisibleModuleNames (getDynFlags tm))

        -- The given namespaces for the imported modules (ie. full name, or alias if used)
        allModNamesAsNS = map (showModName . asNamespace) importDeclerations

        typeEnv = tcg_type_env $ fst $ tm_internals_ tm
        rdrEnv = tcg_rdr_env $ fst $ tm_internals_ tm
        rdrElts = globalRdrEnvElts rdrEnv

        getCompls :: [GlobalRdrElt] -> ([CompItem],QualCompls)
        getCompls = foldMap getComplsForOne

        getComplsForOne :: GlobalRdrElt -> ([CompItem],QualCompls)
        getComplsForOne (GRE n _ True _) =
          case lookupTypeEnv typeEnv n of
            Just tt -> case safeTyThingId tt of
              Just var -> ([varToCompl var],mempty)
              Nothing -> ([toCompItem curMod n],mempty)
            Nothing -> ([toCompItem curMod n],mempty)
        getComplsForOne (GRE n _ False prov) =
          flip foldMap (map is_decl prov) $ \spec ->
            let unqual
                  | is_qual spec = []
                  | otherwise = compItem
                qual
                  | is_qual spec = Map.singleton asMod compItem
                  | otherwise = Map.fromList [(asMod,compItem),(origMod,compItem)]
                compItem = [toCompItem (is_mod spec) n]
                asMod = showModName (is_as spec)
                origMod = showModName (is_mod spec)
            in (unqual,QualCompls qual)

        varToCompl :: Var -> CompItem
        varToCompl var = CI name (showModName curMod) typ label Nothing
          where
            typ = Just $ varType var
            name = Var.varName var
            label = T.pack $ showGhc name

        toCompItem :: ModuleName -> Name -> CompItem
        toCompItem mn n =
          CI n (showModName mn) Nothing (T.pack $ showGhc n) Nothing

        (unquals,quals) = getCompls rdrElts
    return $ CC
      { allModNamesAsNS = allModNamesAsNS
      , unqualCompls = unquals
      , qualCompls = quals
      , importableModules = moduleNames
      }

newtype WithSnippets = WithSnippets Bool

toggleSnippets :: J.ClientCapabilities -> WithSnippets -> J.CompletionItem -> J.CompletionItem
toggleSnippets clientCaps (WithSnippets with) x
  | with && supported = x
  | otherwise = x { J._insertTextFormat = Just J.PlainText
                  , J._insertText       = Nothing
                  }
  where supported = fromMaybe False (clientCaps ^? J.textDocument
          . _Just
          . J.completion
          . _Just
          . J.completionItem
          . _Just
          . J.snippetSupport
          . _Just)

-- | Returns the cached completions for the given module and position.
getCompletions :: Uri -> VFS.PosPrefixInfo -> WithSnippets -> IdeM (IdeResult [J.CompletionItem])
getCompletions uri prefixInfo withSnippets =
  pluginGetFile "getCompletions: " uri $ \file -> do
    caps <- getClientCapabilities

    let VFS.PosPrefixInfo { VFS.fullLine, VFS.prefixModule, VFS.prefixText } = prefixInfo
    debugm $ "got prefix" ++ show (prefixModule, prefixText)
    let enteredQual = if T.null prefixModule then "" else prefixModule <> "."
        fullPrefix  = enteredQual <> prefixText

    ifCachedModuleAndData file (IdeResultOk [])
      $ \tm CachedInfo { newPosToOld } CC { allModNamesAsNS, unqualCompls, qualCompls, importableModules } ->
          let
            -- default to value context if no explicit context
            context = fromMaybe ValueContext $ getContext pos (tm_parsed_module tm)

            {- correct the position by moving 'foo :: Int -> String ->    '
                                                                         ^
               to                             'foo :: Int -> String ->    '
                                                                   ^
            -}
            pos =
              let newPos = VFS.cursorPos prefixInfo
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
                ctxCompls' = case context of
                              TypeContext -> filter isTypeCompl compls
                              ValueContext -> filter (not . isTypeCompl) compls
                              _ -> filter (not . isTypeCompl) compls
                -- Add whether the text to insert has backticks
                ctxCompls = map (\comp -> comp { isInfix = infixCompls }) ctxCompls'

                infixCompls :: Maybe Backtick
                infixCompls = isUsedAsInfix fullLine prefixModule prefixText (VFS.cursorPos prefixInfo)

                compls = if T.null prefixModule
                  then unqualCompls
                  else Map.findWithDefault [] prefixModule $ getQualCompls qualCompls


            mkImportCompl label = (J.detail ?~ label) . mkModCompl $ fromMaybe
              ""
              (T.stripPrefix enteredQual label)

            filtListWith f list =
              [ f label
              | label <- Fuzzy.simpleFilter fullPrefix list
              , enteredQual `T.isPrefixOf` label
              ]

            filtListWithSnippet f list suffix =
              [ toggleSnippets caps withSnippets (f label (snippet <> suffix))
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
              = filtOptsCompls languagesAndExts
              | "{-# options_ghc" `T.isPrefixOf` T.toLower fullLine
              = filtOptsCompls (map (T.pack . stripLeading '-') $ GHC.flagsForCompletion False)
              | "{-# " `T.isPrefixOf` fullLine
              = filtPragmaCompls (pragmaSuffix fullLine)
              | otherwise
              = filtModNameCompls ++ map (toggleSnippets caps withSnippets
                                            . mkCompl . stripAutoGenerated) filtCompls
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
-- helper functions for infix backticks
-- ---------------------------------------------------------------------

hasTrailingBacktick :: T.Text -> Position -> Bool
hasTrailingBacktick line pos
    | T.length line > cursorIndex = (line `T.index` cursorIndex) == '`'
    | otherwise = False
    where cursorIndex = pos ^. J.character

isUsedAsInfix :: T.Text -> T.Text -> T.Text -> Position -> Maybe Backtick
isUsedAsInfix line prefixMod prefixText pos
    | hasClosingBacktick && hasOpeningBacktick = Just Surrounded
    | hasOpeningBacktick = Just LeftSide
    | otherwise = Nothing
  where
    hasOpeningBacktick = openingBacktick line prefixMod prefixText pos
    hasClosingBacktick = hasTrailingBacktick line pos

openingBacktick :: T.Text -> T.Text -> T.Text -> Position -> Bool
openingBacktick line prefixModule prefixText pos
  | backtickIndex < 0 = False
  | otherwise = (line `T.index` backtickIndex) == '`'
    where
    column = pos ^. J.character

    backtickIndex :: Int
    backtickIndex =
      let
          prefixLength = T.length prefixText
          moduleLength = if prefixModule == ""
                    then 0
                    else T.length prefixModule + 1 {- Because of "." -}
      in
        -- Points to the first letter of either the module or prefix text
        column - (prefixLength + moduleLength) - 1


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
