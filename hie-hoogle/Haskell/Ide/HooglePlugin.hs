{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.HooglePlugin where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Bifunctor
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                          as T
import qualified GhcMod                             as GM
import qualified GhcMod.ModuleLoader                as GM
import qualified GhcMod.Monad.Env                   as GM
import qualified GhcMod.Types                       as GM
import           Haskell.Ide.Engine.MonadTypes
import           Hoogle
import           System.Directory
import           System.FilePath
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree

-- ---------------------------------------------------------------------

hoogleDescriptor :: PluginDescriptor
hoogleDescriptor = PluginDescriptor
  {
    pluginName = "hoogle"
  , pluginDesc =
         "Hoogle is a Haskell API search engine, which allows you to search "
      <> "many standard Haskell libraries by either function name, or by approximate "
      <> "type signature. "
  , pluginCommands =
      [ PluginCommand "info" "Look up the documentation for an identifier in the hoogle database" infoCmd
      , PluginCommand "lookup" "Search the hoogle database with a string" lookupCmd
      ]
  }

-- ---------------------------------------------------------------------

data HoogleError = NoDb | NoResults deriving (Eq,Ord,Show)

newtype HoogleDb = HoogleDb (Maybe FilePath)

hoogleErrorToIdeError :: HoogleError -> IdeFailure
hoogleErrorToIdeError NoResults =
  IdeRErr $ IdeError PluginError "No results found" Null
hoogleErrorToIdeError NoDb =
  IdeRErr $ IdeError PluginError "Hoogle database not found. Run hoogle generate to generate" Null

instance GM.ExtensionClass HoogleDb where
  initialValue = HoogleDb Nothing

initializeHoogleDb :: IdeM (Maybe FilePath)
initializeHoogleDb = do
  db' <- getHoogleDbLoc
  db <- liftIO $ makeAbsolute db'
  exists <- liftIO $ doesFileExist db
  if exists then do
    GM.put $ HoogleDb $ Just db
    return $ Just db
  else
    return Nothing

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


infoCmd :: CommandFunc T.Text T.Text
infoCmd = CmdSync $ \expr -> do
  _ <- initializeHoogleDb
  bimap hoogleErrorToIdeError id <$> infoCmd' expr

infoCmd' :: T.Text -> IdeM (Either HoogleError T.Text)
infoCmd' expr = do
  HoogleDb mdb <- GM.get
  liftIO $ runHoogleQuery mdb expr $ \res ->
      if null res then
        Left NoResults
      else
        return $ T.pack $ targetInfo $ head res

infoCmdFancyRender :: T.Text -> IdeM (Either HoogleError T.Text)
infoCmdFancyRender expr = do
  HoogleDb mdb <- GM.get
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

lookupCmd :: CommandFunc T.Text [T.Text]
lookupCmd = CmdSync $ \term -> do
      _ <- initializeHoogleDb
      bimap hoogleErrorToIdeError id <$> lookupCmd' 10 term

lookupCmd' :: Int -> T.Text -> IdeM (Either HoogleError [T.Text])
lookupCmd' n term = do
  HoogleDb mdb <- GM.get
  liftIO $ runHoogleQuery mdb term
    (Right . map (T.pack . targetResultDisplay False) . take n)

------------------------------------------------------------------------

runHoogleQuery :: Maybe FilePath -> T.Text -> ([Target] -> Either HoogleError a) -> IO (Either HoogleError a)
runHoogleQuery Nothing _ _ = return $ Left NoDb
runHoogleQuery (Just db) quer f = do
  res <- searchHoogle db quer
  return (f res)

searchHoogle :: FilePath -> T.Text -> IO [Target]
searchHoogle dbf quer = withDatabase dbf (return . flip searchDatabase (T.unpack quer))

