{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Haskell.Ide.HaddockPlugin where

import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map                                     as Map
import           Data.Monoid
import qualified Data.Text                                    as T
import           Data.IORef
import           System.Directory
import           System.FilePath
import           GHC
import           GhcMonad
import qualified GhcMod.Monad                                 as GM
import           Haskell.Ide.Engine.MonadTypes
import           HscTypes
import           Name
import           Packages

import           Haskell.Ide.HaRePlugin

import Documentation.Haddock
import Documentation.Haddock.Types


lookupHaddock :: DynFlags -> UnitId -> Maybe [FilePath]
lookupHaddock df ui = haddockInterfaces <$> lookupPackage df ui

lookupHtmls :: DynFlags -> UnitId -> Maybe [FilePath]
lookupHtmls df ui = haddockHTMLs <$> lookupPackage df ui

lookupDocHtmlForModule :: DynFlags -> Module -> IO (Maybe FilePath)
lookupDocHtmlForModule df m = do
  let fp = go =<< lookupHtmls df ui
  ex <- maybe (pure False) doesFileExist fp
  if ex then
    return fp
  else
    return Nothing
  where
    go [] = Nothing
    go (x:_) = Just $ x</>mn<.>"html"
    ui = moduleUnitId m
    mn = map (\x -> if x == '.' then '-' else x) mns
    mns = moduleNameString $ moduleName m

lookupSrcHtmlForModule :: DynFlags -> Module -> IO (Maybe FilePath)
lookupSrcHtmlForModule df m = do
  let fp = go =<< lookupHtmls df ui
  ex <- maybe (pure False) doesFileExist fp
  if ex then
    return fp
  else
    return Nothing
  where
    go [] = Nothing
    go (x:_) = Just $ x</>"src"</>mn<.>"html"
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


getDocsForName :: DynFlags -> Name -> IdeM (Maybe T.Text)
getDocsForName df name = do
  let mfs = nameModule_maybe name >>=
              lookupHaddock df . moduleUnitId
  mf <- case mfs of
    Nothing -> pure Nothing
    Just fs -> liftIO $ do
      fs' <- filterM doesFileExist fs
      case fs' of
        [] -> pure Nothing
        (x:_) -> pure $ Just x
  case mf of
    Nothing -> return Nothing
    Just f -> do
      ehi <- GM.unGmlT $ readInterfaceFile nameCacheFromGhcMonad f
      case ehi of
        Left _ -> return Nothing
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
              return $ Just $ T.intercalate "\n\n"
                [ doc
                , maybe "" (\x ->"[Documentation](file://"<>T.pack x<>"#"<>selector<>showName name<>")") mdoch
                , maybe "" (\x ->"[Source](file://"<>T.pack x<>"#"<>showName name<>")") msrch
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
prettyprintType n t = T.unlines $
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
         , markupCodeBlock = \x -> "\n```haskell\n" <> x <> "```"
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
             ,"prop> " <> T.pack s
             ,"```\n"]
         , markupExample = T.unlines . map (\e -> T.pack $ unlines $
             ["\n```haskell"
             ,"> " <> exampleExpression e
             ] ++ exampleResult e ++
             ["```\n"])
         , markupHeader = \h ->
             T.replicate (headerLevel h) "#" <> " " <> headerTitle h <> "\n"
         }
    where surround c x = c <> x <> c
