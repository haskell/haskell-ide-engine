{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Haskell.Ide.HooglePlugin where

import           Data.Aeson
import           Data.Monoid
import           Data.Maybe
import qualified Data.Text as T
import           Data.Vinyl
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Control.Monad.IO.Class
import           Hoogle
import           System.Directory

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

infoCmd :: CommandFunc T.Text
infoCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdText "expr" :& RNil) req of
    Left err -> return err
    Right (ParamText expr :& RNil) -> liftIO $ do
        db <- defaultDatabaseLocation
        dbExists <- doesFileExist db
        if dbExists then do
            res <- searchHoogle db expr
            if null res then
                return $ IdeResponseOk "No results found"
            else do
                let Target{..} = head res
                    packageModule = unwords $ map fst $ catMaybes [targetPackage, targetModule]
                return $ IdeResponseOk $ T.pack $ unlines $ [unHTML targetItem, packageModule, unHTML targetDocs]
        else
            return $ IdeResponseFail hoogleDbError

-- ---------------------------------------------------------------------

lookupCmd :: CommandFunc [T.Text]
lookupCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdText "term" :& RNil) req of
    Left err -> return err
    Right (ParamText term :& RNil) -> liftIO $ do
        db <- defaultDatabaseLocation
        dbExists <- doesFileExist db
        if dbExists then do
            res <- take 10 <$> searchHoogle db term
            if null res then
                return $ IdeResponseOk []
            else do
                return $ IdeResponseOk $ map disp res
        else
            return $ IdeResponseFail hoogleDbError

searchHoogle :: FilePath -> T.Text -> IO [Target]
searchHoogle dbf quer = withDatabase dbf (return . flip searchDatabase (T.unpack quer))

hoogleDbError :: IdeError
hoogleDbError = IdeError PluginError "Hoogle database not found. Run hoogle generate to generate" Null

disp :: Target -> T.Text
disp Target{..} = T.pack $ unHTML . unwords $ fmap fst (maybeToList targetModule) ++ [targetItem]
