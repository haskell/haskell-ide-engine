{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Hoogle where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Bifunctor
import           Data.Maybe
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Text                          as T
import Data.List
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.MonadFunctions
import           Hoogle
import           System.Directory
import           System.Environment
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree

data HoogleError = NoDb | NoResults deriving (Eq,Ord,Show)

newtype HoogleDb = HoogleDb (Maybe FilePath)

hoogleErrorToIdeError :: HoogleError -> IdeError
hoogleErrorToIdeError NoResults =
  IdeError PluginError "No results found" Null
hoogleErrorToIdeError NoDb =
  IdeError PluginError "Hoogle database not found. Run hoogle generate to generate" Null

instance ExtensionClass HoogleDb where
  initialValue = HoogleDb Nothing

initializeHoogleDb :: IdeGhcM (Maybe FilePath)
initializeHoogleDb = do
  explicitDbLocation <- liftIO $ lookupEnv "HIE_HOOGLE_DATABASE"
  db' <- maybe (liftIO defaultDatabaseLocation) pure explicitDbLocation
  db <- liftIO $ makeAbsolute db'
  exists <- liftIO $ doesFileExist db
  if exists then do
    put $ HoogleDb $ Just db
    return $ Just db
  else
    return Nothing

info :: T.Text -> IdeM (Either HoogleError T.Text)
info expr = do
  HoogleDb mdb <- get
  liftIO $ runHoogleQuery mdb expr $ \res ->
      if null res then
        Left NoResults
      else
        return $ T.pack $ targetInfo $ head res

infoFancyRender :: T.Text -> IdeM (Either HoogleError T.Text)
infoFancyRender expr = do
  HoogleDb mdb <- get
  liftIO $ runHoogleQuery mdb expr $ \res ->
      if null res then
        Left NoResults
      else
        return $ renderTarget $ head res

renderTarget :: Target -> T.Text
renderTarget t = T.intercalate "\n\n" $
     ["```haskell\n" <> unHTML (T.pack $ targetItem t) <> "```"]
  ++ [T.pack $ unwords mdl | not $ null mdl]
  ++ [renderDocs $ targetDocs t]
  ++ [T.pack $ curry annotate "More info" $ targetURL t]
  where mdl = map annotate $ catMaybes [targetPackage t, targetModule t]
        annotate (thing,url) = "["<>thing++"]"++"("++url++")"
        unHTML = T.replace "<0>" "" . innerText . parseTags
        renderDocs = T.concat . map htmlToMarkDown . parseTree . T.pack
        htmlToMarkDown :: TagTree T.Text -> T.Text
        htmlToMarkDown (TagLeaf x) = fromMaybe "" $ maybeTagText x
        htmlToMarkDown (TagBranch "i" _ tree) = "*" <> T.concat (map htmlToMarkDown tree) <> "*"
        htmlToMarkDown (TagBranch "b" _ tree) = "**" <> T.concat (map htmlToMarkDown tree) <> "**"
        htmlToMarkDown (TagBranch "a" _ tree) = "`" <> T.concat (map htmlToMarkDown tree) <> "`"
        htmlToMarkDown (TagBranch "li" _ tree) = "- " <> T.concat (map htmlToMarkDown tree)
        htmlToMarkDown (TagBranch "tt" _ tree) = "`" <> innerText (flattenTree tree) <> "`"
        htmlToMarkDown (TagBranch "pre" _ tree) = "```haskell\n" <> T.concat (map htmlToMarkDown tree) <> "```"
        htmlToMarkDown (TagBranch _ _ tree) = T.concat $ map htmlToMarkDown tree

------------------------------------------------------------------------

lookup :: Int -> T.Text -> IdeM (Either HoogleError [T.Text])
lookup n term = do
  HoogleDb mdb <- get
  liftIO $ runHoogleQuery mdb term
    (Right . map (T.pack . targetResultDisplay False) . take n)

searchModules :: T.Text -> IdeM [T.Text]
searchModules = fmap (nub . take 5) . searchTargets (fmap (T.pack . fst) . targetModule)

searchPackages :: T.Text -> IdeM [T.Text]
searchPackages = fmap (nub . take 5) . searchTargets (fmap (T.pack . fst) . targetPackage)

searchTargets :: (Target -> Maybe a) -> T.Text -> IdeM [a]
searchTargets f term = do
  HoogleDb mdb <- get
  res <- liftIO $ runHoogleQuery mdb term (Right . mapMaybe f . take 10)
  case bimap hoogleErrorToIdeError id res of
    Left _ -> return []
    Right xs -> return xs

------------------------------------------------------------------------

runHoogleQuery :: Maybe FilePath -> T.Text -> ([Target] -> Either HoogleError a) -> IO (Either HoogleError a)
runHoogleQuery Nothing _ _ = return $ Left NoDb
runHoogleQuery (Just db) quer f = do
  res <- withDatabase db (return . flip searchDatabase (T.unpack quer))
  return (f res)


------------------------------------------------------------------------

docRules :: Maybe T.Text -> T.Text -> T.Text
docRules (Just "base") "GHC.Base"    = "Prelude"
docRules (Just "base") "GHC.Enum"    = "Prelude"
docRules (Just "base") "GHC.Num"     = "Prelude"
docRules (Just "base") "GHC.Real"    = "Prelude"
docRules (Just "base") "GHC.Float"   = "Prelude"
docRules (Just "base") "GHC.Show"    = "Prelude"
docRules (Just "containers") modName =
  fromMaybe modName $ T.stripSuffix ".Base" modName
docRules _ modName = modName

getDocsForName :: T.Text -> Maybe T.Text -> T.Text -> IdeM (Maybe T.Text)
getDocsForName name pkg modName' = do
  let modName = docRules pkg modName'
      query = name
           <> maybe "" (T.append " package:") pkg
           <> " module:" <> modName
           <> " is:exact"
  debugm $ "hoogle query: " ++ T.unpack query
  res <- infoFancyRender query
  case res of
    Right x -> return $ Just x
    Left _ -> return Nothing
