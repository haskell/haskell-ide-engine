{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Haskell.Ide.HooglePlugin where

import           Data.Aeson
import           Data.Monoid
import           Data.Maybe
--import           Data.List (intercalate)
import qualified Data.Text as T
import           Data.Vinyl
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.ExtensibleState
import           Control.Monad.IO.Class
import           Hoogle
import           System.Directory
import qualified GhcMod as GM
import qualified GhcMod.Monad.Env as GM
import qualified GhcMod.Types as GM
import           System.FilePath
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

-- ---------------------------------------------------------------------

hoogleDescriptor :: TaggedPluginDescriptor _
hoogleDescriptor = PluginDescriptor
  {
    pdUIShortName = "hoogle"
  , pdUIOverview = ("Hoogle is a Haskell API search engine, which allows you to search "
           <> "many standard Haskell libraries by either function name, or by approximate "
           <> "type signature. ")
  , pdCommands =
         buildCommand infoCmd (Proxy :: Proxy "info") "Look up the documentation for an identifier in the hoogle database"
                     [] (SCtxNone :& RNil)
                     (  SParamDesc (Proxy :: Proxy "expr") (Proxy :: Proxy "The identifier to lookup") SPtText SRequired
                     :& RNil) SaveNone
      :& buildCommand lookupCmd (Proxy :: Proxy "lookup") "Search the hoogle database with a string"
                     [] (SCtxNone :& RNil)
                     (  SParamDesc (Proxy :: Proxy "term") (Proxy :: Proxy "The term to search for in the hoogle database") SPtText SRequired
                     :& RNil) SaveNone
      :& RNil
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

newtype HoogleDb = HoogleDb (Maybe FilePath)

instance ExtensionClass HoogleDb where
  initialValue = HoogleDb Nothing

getHoogleDb :: IdeM FilePath
getHoogleDb = do
  HoogleDb mdb <- get
  case mdb of
    Nothing -> do
      db <- getHoogleDbLoc
      put $ HoogleDb $ Just db
      return db
    Just db -> return db

getHoogleDbLoc :: IdeM FilePath
getHoogleDbLoc = do
  cradle <- GM.gmCradle <$> GM.gmeAsk
  let mfp =
        case GM.cradleProject cradle of
          GM.StackProject s ->
            case reverse $ splitPath $ GM.seLocalPkgDb s of
              "pkgdb":ghcver:resolver:arch:_install:xs ->
                Just $ joinPath $ reverse xs ++ ["hoogle",arch,resolver,ghcver,"database.hoo"]
              _ -> Nothing
          _ -> Just "hiehoogledb.hoo"
  case mfp of
    Nothing -> liftIO defaultDatabaseLocation
    Just fp -> do
      exists <- liftIO $ doesFileExist fp
      if exists then
        return fp
      else
        liftIO defaultDatabaseLocation


infoCmd :: CommandFunc (Maybe [J.MarkedString])
infoCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdText "expr" :& RNil) req of
    Left err -> return err
    Right (ParamText expr :& RNil) ->
      infoCmd' expr

infoCmd' :: T.Text -> IdeM (IdeResponse (Maybe [J.MarkedString]))
infoCmd' expr = do
  db <- getHoogleDb
  liftIO $ runHoogleQuery db expr $ \res ->
      if null res then
          IdeResponseOk Nothing
      else
          IdeResponseOk $ Just $ renderTarget $ head res

renderTarget :: Target -> [J.MarkedString]
renderTarget t = [J.CodeString $ J.LanguageString "haskell" $ unHTML $ T.pack $ targetItem t]
              ++ [J.PlainString $ T.pack $ unwords mdl | not $ null mdl]
              ++ [renderDocs $ targetDocs t]
              ++ [J.PlainString $ T.pack $ curry annotate "More info" $ targetURL t]
  where mdl = map annotate $ catMaybes [targetPackage t, targetModule t]
        annotate (thing,url) = "["<>thing++"]"++"("++url++")"
        unHTML = T.replace "<0>" "" . innerText . parseTags
        renderDocs = J.PlainString . T.concat . map htmlToMarkDown . parseTree . T.pack
        htmlToMarkDown :: TagTree T.Text -> T.Text
        htmlToMarkDown (TagLeaf x) = fromMaybe "" $ maybeTagText x
        htmlToMarkDown (TagBranch "i" _ tree) = "*" <> T.concat (map htmlToMarkDown tree) <> "*"
        htmlToMarkDown (TagBranch "b" _ tree) = "**" <> T.concat (map htmlToMarkDown tree) <> "**"
        htmlToMarkDown (TagBranch "a" _ tree) = "`" <> T.concat (map htmlToMarkDown tree) <> "`"
        htmlToMarkDown (TagBranch "tt" _ tree) = "`" <> innerText (flattenTree tree) <> "`"
        htmlToMarkDown (TagBranch "pre" _ tree) = "```haskell\n" <> T.concat (map htmlToMarkDown tree) <> "```"
        htmlToMarkDown (TagBranch _ _ tree) = T.concat $ map htmlToMarkDown tree

------------------------------------------------------------------------

lookupCmd :: CommandFunc [T.Text]
lookupCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdText "term" :& RNil) req of
    Left err -> return err
    Right (ParamText term :& RNil) ->
      lookupCmd' 10 term

lookupCmd' :: Int -> T.Text -> IdeM (IdeResponse [T.Text])
lookupCmd' n term = do
  db <- getHoogleDb
  liftIO $ runHoogleQuery db term
    (IdeResponseOk . map (T.pack . targetResultDisplay False) . take n)

------------------------------------------------------------------------

runHoogleQuery :: FilePath -> T.Text -> ([Target] -> IdeResponse a) -> IO (IdeResponse a)
runHoogleQuery db quer f = do
        dbExists <- doesFileExist db
        if dbExists then do
            res <- searchHoogle db quer
            return (f res)
        else
            return $ IdeResponseFail hoogleDbError

searchHoogle :: FilePath -> T.Text -> IO [Target]
searchHoogle dbf quer = withDatabase dbf (return . flip searchDatabase (T.unpack quer))

hoogleDbError :: IdeError
hoogleDbError = IdeError PluginError "Hoogle database not found. Run hoogle generate to generate" Null
