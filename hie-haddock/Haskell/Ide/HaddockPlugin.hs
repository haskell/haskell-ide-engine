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
import           GHC
import           GhcMonad
import qualified GhcMod.Monad                                 as GM
import           Haskell.Ide.Engine.MonadTypes
import           HscTypes
import           Name
import           Packages

import Documentation.Haddock
import Documentation.Haddock.Types

lookupHaddock :: DynFlags -> UnitId -> Maybe [FilePath]
lookupHaddock df ui =
  case find ((ui ==) . unitId) $ listPackageConfigMap df of
    Nothing -> Nothing
    Just pc -> Just $ haddockInterfaces pc

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
  let mf = do
        m <- nameModule_maybe name
        fs <- lookupHaddock df (moduleUnitId m)
        case fs of
          [] -> Nothing
          (x:_) -> return x
  case mf of
    Nothing -> return Nothing
    Just f -> do
      ehi <- GM.unGmlT $ readInterfaceFile nameCacheFromGhcMonad f
      case ehi of
        Left _ -> return Nothing
        Right hi -> do
          let docs = do
                mdl <- Map.lookup name $ ifLinkEnv hi
                insiface <- find ((mdl == ) . instMod ) $ ifInstalledIfaces hi
                Map.lookup name $ instDocMap insiface
          return (renderDocs <$> docs)

renderDocs :: MDoc Name -> T.Text
renderDocs = markup renderMarkDown . _doc

renderMarkDown :: DocMarkup Name T.Text
renderMarkDown =
  Markup { markupEmpty = ""
         , markupString = T.pack
         , markupParagraph = surround "\n"
         , markupAppend = mappend
         , markupIdentifier = T.pack . getOccString
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
         , markupProperty = T.pack
         , markupExample = const ""
         , markupHeader = const ""
         }
    where surround c x = c <> x <> c
