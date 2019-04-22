{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
module Haskell.Ide.Engine.Plugin.Haddock where

import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map                                     as Map
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Text                                    as T
import           Data.IORef
import Data.Function
import Data.Maybe
import Data.List
import           GHC
import qualified GhcMod.LightGhc                              as GM
import qualified GhcMod.Monad                                 as GM
import           GhcMonad
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import qualified Haskell.Ide.Engine.Plugin.Hoogle             as Hoogle
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Support.HieExtras
import           HscTypes
import qualified Language.Haskell.LSP.Types as J
import           Name
import           Packages
import           System.Directory
import           System.FilePath


import Documentation.Haddock
import Documentation.Haddock.Types

haddockDescriptor :: PluginId -> PluginDescriptor
haddockDescriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginName = "Haddock"
  , pluginDesc = "Fishy documentation"
  , pluginCommands = []
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Just hoverProvider
  , pluginSymbolProvider = Nothing
  , pluginFormattingProvider = Nothing
  }


lookupHaddock :: DynFlags -> UnitId -> Maybe [FilePath]
lookupHaddock df ui = haddockInterfaces <$> lookupPackage df ui

lookupHtmls :: DynFlags -> UnitId -> Maybe [FilePath]
lookupHtmls df ui = haddockHTMLs <$> lookupPackage df ui

lookupDocHtmlForModule :: DynFlags -> Module -> IO (Maybe FilePath)
lookupDocHtmlForModule =
  lookupHtmlForModule (\pkgDocDir modDocName -> pkgDocDir </> modDocName <.> "html")

lookupSrcHtmlForModule :: DynFlags -> Module -> IO (Maybe FilePath)
lookupSrcHtmlForModule =
  lookupHtmlForModule (\pkgDocDir modDocName -> pkgDocDir </> "src" </> modDocName <.> "html")

lookupHtmlForModule :: (FilePath -> FilePath -> FilePath) -> DynFlags -> Module -> IO (Maybe FilePath)
lookupHtmlForModule mkDocPath df m = do
  let mfp = go <$> (listToMaybe =<< lookupHtmls df ui)
  exists <- maybe (pure False) doesFileExist mfp
  return $ if exists then mfp else Nothing
  where
    go pkgDocDir = mkDocPath pkgDocDir mn
    ui = moduleUnitId m
    mn = map (\x -> if x == '.' then '-' else x) mns
    mns = moduleNameString $ moduleName m

nameCacheFromGhcMonad :: GhcMonad m => NameCacheAccessor m
nameCacheFromGhcMonad = ( read_from_session , write_to_session )
  where
    read_from_session = do
       ref <- withSession (return . hsc_NC)
       liftIO $ readIORef ref
    write_to_session nc' = do
       ref <- withSession (return . hsc_NC)
       liftIO $ writeIORef ref nc'

runInLightGhc :: GM.LightGhc a -> IdeM a
runInLightGhc a = do
  hscEnvRef <- ghcSession <$> readMTS
  mhscEnv <- liftIO $ traverse readIORef hscEnvRef
  case mhscEnv of
    Nothing -> error "Ghc Session not initialized"
    Just env -> GM.runLightGhc env a

nameCacheFromIdeM :: NameCacheAccessor IdeM
nameCacheFromIdeM = ( read_from_session , write_to_session )
  where
    read_from_session = runInLightGhc $ fst nameCacheFromGhcMonad
    write_to_session = runInLightGhc . snd nameCacheFromGhcMonad

getDocsForName :: DynFlags -> Name -> IdeM (Maybe T.Text)
getDocsForName df name = do
  let mfs = nameModule_maybe name >>=
              lookupHaddock df . moduleUnitId
  mf <- case mfs of
    Nothing -> pure Nothing
    Just fs -> liftIO $ listToMaybe <$> filterM doesFileExist fs
  case mf of
    Nothing -> return Nothing
    Just f -> do
      ehi <- readInterfaceFile nameCacheFromIdeM f
      case ehi of
        Left message -> do
          debugm $ "Haddock docs couldn't be loaded as readInterfaceFile failed with: " ++ message
          return Nothing
        Right hi -> do
          let res = do -- @Maybe
                mdl <- nameModule_maybe name
                lmdl <- Map.lookup name (ifLinkEnv hi)
                insiface <- find ((mdl ==) . instMod) $ ifInstalledIfaces hi
                doc <- Map.lookup name $ instDocMap insiface
                return (renderDocs doc, mdl, lmdl)
          case res of
            Nothing -> return Nothing
            Just (doc, mdl, lmdl) -> do
              mdoch <- liftIO $ lookupDocHtmlForModule df lmdl
              msrch <- liftIO $ lookupSrcHtmlForModule df mdl
              let selector
                    | isValName name = "v:"
                    | otherwise = "t:"
              return $ Just $ T.concat
                [ doc
                , maybe "" (\x ->"\n\n[Documentation](file://"<>T.pack x<>"#"<>selector<>showName name<>")") mdoch
                , maybe "" (\x ->"\n\n[Source](file://"<>T.pack x<>"#"<>showName name<>")") msrch
                ]

getDocsWithType :: DynFlags -> Name -> IdeM (Maybe T.Text)
getDocsWithType df name = do
  mdocs <- getDocsForName df name
  mtyp <- getTypeForName name
  return $ case (mdocs,mtyp) of
    (Nothing, Nothing) ->
      Nothing
    (Just docs, Just typ) -> Just $
      prettyprintType name typ <> "\n" <> docs
    (Just docs, Nothing) -> Just docs
    (Nothing, Just typ) -> Just $ prettyprintType name typ

prettyprintType :: Name -> Type -> T.Text
prettyprintType n t = T.unlines
  [ "```haskell"
  , showName n <> " :: " <> showName t
  , "```\n"
  ]

renderDocs :: MDoc Name -> T.Text
renderDocs = markup renderMarkDown . _doc

renderMarkDown :: DocMarkup Name T.Text
renderMarkDown =
  Markup { markupEmpty = ""
         , markupString = T.pack
         , markupParagraph = (<> "\n\n")
         , markupAppend = mappend
         , markupIdentifier = surround "`" . T.pack . getOccString
         , markupIdentifierUnchecked = T.pack . occNameString . snd
         , markupModule = surround "**" . T.pack
         , markupWarning = surround "*"
         , markupEmphasis = surround "*"
         , markupBold = surround "**"
         , markupMonospaced = surround "`"
         , markupUnorderedList = T.unlines . map ("- "<>)
         , markupOrderedList =
             T.unlines . zipWith (\i n -> T.pack (show (i :: Int)) <> ". " <> n) [1..]
         , markupDefList = T.unlines . map (\(a, b) -> a <> " :: " <> b)
         , markupCodeBlock = \x -> "\n```haskell\n" <> removeInner x <> "\n```\n"
         , markupHyperlink = \h ->
             T.pack $ maybe
               (hyperlinkUrl h)
               (\l -> "["<>l<>"]("<>hyperlinkUrl h<>")")
               (hyperlinkLabel h)
         , markupAName = T.pack
         , markupPic = const ""
         , markupMathInline = T.pack
         , markupMathDisplay = T.pack
         , markupProperty = \s -> T.unlines
             ["\n```haskell"
             ,"prop> " <> removeInner (T.pack s)
             ,"\n```\n"]
         , markupExample = T.unlines . map (\e -> T.pack $ unlines $
             ["\n```haskell"
             ,"> " <> exampleExpression e
             ] ++ exampleResult e ++
             ["\n```\n"])
         , markupHeader = \h ->
             T.replicate (headerLevel h) "#" <> " " <> headerTitle h <> "\n"
#if __GLASGOW_HASKELL__ >= 804
         , markupTable = mempty
#endif
         }
    where surround c x = c <> T.replace c "" x <> c
          removeInner x = T.replace "```" "" $ T.replace "```haskell" "" x

hoverProvider :: HoverProvider
hoverProvider doc pos = pluginGetFile "haddock:hoverProvider" doc $ \fp ->
  ifCachedModule fp (IdeResultOk mempty) $ \tm info -> runIdeResultT $ do
    let df = getDynFlags tm
        names = mapMaybe pickName $ groupBy f $ sortBy f' $ getSymbolsAtPoint pos info
    docs <- forM names $ \(_,name) -> do
      let sname = showName name
      case getModule df name of
        Nothing -> return $ "`" <> sname <> "` *local*"
        (Just (pkg,mdl)) -> do
          let mname = "`"<> sname <> "`\n\n"
          let minfo = maybe "" (<>" ") pkg <> mdl
          mdocu' <- lift $ getDocsWithType df name
          mdocu <- case mdocu' of
            Just _ -> return mdocu'
            -- Hoogle as fallback
            Nothing -> lift $ Hoogle.getDocsForName sname pkg mdl
          return $ case mdocu of
            Nothing -> mname <> minfo
            Just docu -> docu <> "\n\n" <> minfo
    return [J.Hover (J.HoverContents $ J.MarkupContent J.MkMarkdown (T.intercalate J.sectionSeparator docs)) Nothing]
  where
    pickName [] = Nothing
    pickName [x] = Just x
    pickName xs@(x:_) = case find (isJust . nameModule_maybe . snd) xs of
      Nothing -> Just x
      Just a -> Just a
    f = (==) `on` (showName . snd)
    f' = compare `on` (showName . snd)
