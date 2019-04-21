{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Plugin.Hoogle where

import           Control.Monad.IO.Class
import           Control.Monad (join)
import           Control.Exception
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

-- ---------------------------------------------------------------------

hoogleDescriptor :: PluginId -> PluginDescriptor
hoogleDescriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginName = "hoogle"
  , pluginDesc =
         "Hoogle is a Haskell API search engine, which allows you to search "
      <> "many standard Haskell libraries by either function name, or by approximate "
      <> "type signature. "
  , pluginCommands =
      [ PluginCommand "info" "Look up the documentation for an identifier in the hoogle database" infoCmd
      , PluginCommand "lookup" "Search the hoogle database with a string" lookupCmd
      ]
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  , pluginFormattingProvider = Nothing
  }

-- ---------------------------------------------------------------------

data HoogleError 
  = NoDb
  | DbFail T.Text
  | NoResults 
  deriving (Eq,Ord,Show)

newtype HoogleDb = HoogleDb (Maybe FilePath)

-- | Convert Hoogle Error's to Ide Error's.
-- Can be used to present errors to the client.
hoogleErrorToIdeError :: HoogleError -> IdeError
hoogleErrorToIdeError NoResults =
  IdeError PluginError "No results found" Null
hoogleErrorToIdeError NoDb =
  IdeError PluginError "Hoogle database not found. Run hoogle generate to generate" Null
hoogleErrorToIdeError (DbFail msg) =
  IdeError PluginError ("Hoogle failed with following error: " <> msg) Null

instance ExtensionClass HoogleDb where
  initialValue = HoogleDb Nothing

-- | Initialise the Hoogle Database.
-- Search for the Hoogle Database and set it in the global config if found.
-- Looks first into custom hoogle database locations, then in the default location.
-- Note, that the FilePath must be an absolute path, otherwise Hoogle can not
-- find the database.
--
-- If no hoogle database has been found, Nothing is returned
-- and we will have no access to the hoogle database.
-- However, it is still safe to use the hoogle API,
-- e.g. either error or default values are returned.
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

infoCmd :: CommandFunc T.Text T.Text
infoCmd = CmdSync $ \expr -> do
  res <- liftToGhc $ bimap hoogleErrorToIdeError id <$> infoCmd' expr
  return $ case res of
    Left err -> IdeResultFail err
    Right x -> IdeResultOk x

infoCmd' :: T.Text -> IdeM (Either HoogleError T.Text)
infoCmd' expr = do
  HoogleDb mdb <- get
  liftIO $ runHoogleQuery mdb expr $ \res ->
      if null res then
        Left NoResults
      else
        return $ T.pack $ targetInfo $ head res

-- | Command to get the prettified documentation of an hoogle identifier.
-- Identifier should be understandable for hoogle.
-- If documentation can be found for it, the result will be rendered
-- in markdown for the lsp-client. If multiple results have been found,
-- only the first result will be shown.
--
-- If no result can be found for the identifier, a hoogle error is returned
-- that can be shown to the client by converting it
-- to an IdeError with 'hoogleErrorToIdeError'.
infoCmdFancyRender :: T.Text -> IdeM (Either HoogleError T.Text)
infoCmdFancyRender expr = do
  HoogleDb mdb <- get
  liftIO $ runHoogleQuery mdb expr $ \res ->
      if null res then
        Left NoResults
      else
        return $ renderTarget $ head res

-- | Render the target in valid markdown.
-- Transform haddock documentation into markdown.
renderTarget :: Target -> T.Text
-- renderTarget t = T.intercalate "\n\n" $
renderTarget t = T.intercalate "\n" $
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

-- | Search for modules that satisfy the given search text.
-- Will return at most five, unique results.
--
-- If an error occurs, such as no hoogle database has been found,
-- or the search term has no match, an empty list will be returned.
searchModules :: T.Text -> IdeM [T.Text]
searchModules = fmap (nub . take 5) . searchTargets (fmap (T.pack . fst) . targetModule)

-- | Search for packages that satisfy the given search text.
-- Will return at most five, unique results.
--
-- If an error occurs, such as no hoogle database has been found,
-- or the search term has no match, an empty list will be returned.
searchPackages :: T.Text -> IdeM [T.Text]
searchPackages = fmap (nub . take 5) . searchTargets (fmap (T.pack . fst) . targetPackage)

-- | Search for Targets that fit to the given Text and satisfy the given predicate.
-- Limits the amount of matches to at most ten.
-- Applies the predicate to the first ten matches. May also return zero matches,
-- although there are matches, if none of the first ten matches
-- satisfies the predicate.
--
-- If an error occurs, such as no hoogle database has been found,
-- or the search term has no match, an empty list will be returned.
searchTargets :: (Target -> Maybe a) -> T.Text -> IdeM [a]
searchTargets f term = do
  HoogleDb mdb <- get
  res <- liftIO $ runHoogleQuery mdb term (Right . mapMaybe f . take 10)
  case bimap hoogleErrorToIdeError id res of
    Left _ -> return []
    Right xs -> return xs

------------------------------------------------------------------------

-- | Lookup the given Text in the local Hoogle database.
-- Is limited to collect at most ten matches.
-- May fail with a HoogleError that can be shown to the user.
lookupCmd :: CommandFunc T.Text [T.Text]
lookupCmd = CmdSync $ \term -> do
  res <- liftToGhc $ bimap hoogleErrorToIdeError id <$> lookupCmd' 10 term
  return $ case res of
    Left err -> IdeResultFail err
    Right x -> IdeResultOk x

-- | Lookup the given Text in the local Hoogle database.
-- Takes the first `n` matches.
-- May fail with a HoogleError that can be shown to the user.
lookupCmd' :: Int -> T.Text -> IdeM (Either HoogleError [T.Text])
lookupCmd' n term = do
  HoogleDb mdb <- get
  liftIO $ runHoogleQuery mdb term
    (Right . map (T.pack . targetResultDisplay False) . take n)

------------------------------------------------------------------------

-- | Run a query for Hoogle on the given Hoogle database.
-- If no Database is given, no search is executed.
-- If the Database cannot be found at the given location, an IOException will be thrown.
-- Note, that the database file must be an absolute path.
-- The target may be of the form: 'take', 'take :: Int -> [a] -> [a]', 'Data.List'.
-- In general, it is very similar to the Web Api.
-- Found targets can be consumed with the given callback function.
-- You can limit the amount of results, by taking only the first ten results.
-- Example call:
--
-- @
--   runHoogleQuery
--    (Just "/home/user/.hoogle/default-haskell-5.0.17.hoo")
--    (Data.Text.pack "take :: Int -> [a] -> [a]")
--    (Right . Prelude.take 10)
-- @
-- This limits the results to ten and looks for a function `take` that has the given signature.
--
-- HoogleError's can be translated to IdeErrors with @hoogleErrorToIdeError@
-- and shown to the client.
runHoogleQuery :: Maybe FilePath -> T.Text -> ([Target] -> Either HoogleError a) -> IO (Either HoogleError a)
runHoogleQuery Nothing _ _ = return $ Left NoDb
runHoogleQuery (Just db) quer f = do
  res <- try (searchHoogle db quer) :: IO (Either ErrorCall [Target])
  return . join $ bimap (DbFail . T.pack . show) f res


-- | Run a query for Hoogle on the given Hoogle database.
-- If the database can not be found, an IOException is thrown.
-- The target may be of the form: `take`, `take :: Int -> [a] -> [a]`
searchHoogle :: FilePath -> T.Text -> IO [Target]
searchHoogle dbf quer = withDatabase dbf (return . flip searchDatabase (T.unpack quer))

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

-- | Get the Documentation for a given identifier in a given module.
-- May also specify the according package, to avoid name clashes.
-- Results is a prettified Text that can be sent and shown to the client.
--
-- Might fail, if the identifier can not be found.
getDocsForName :: T.Text -- ^ Identifier within a module.
               -> Maybe T.Text -- ^ Optional package name to avoid name clashes.
               -> T.Text -- ^ Name of the module to search in.
               -> IdeM (Maybe T.Text) -- ^ Prettified hoogle documentation of target.
getDocsForName name pkg modName' = do
  let modName = docRules pkg modName'
      query = name
           <> maybe "" (T.append " package:") pkg
           <> " module:" <> modName
           <> " is:exact"
  debugm $ "hoogle query: " ++ T.unpack query
  res <- infoCmdFancyRender query
  case res of
    Right x -> return $ Just x
    Left _ -> return Nothing
