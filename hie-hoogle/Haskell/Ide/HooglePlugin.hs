{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Haskell.Ide.HooglePlugin where

import           Data.Aeson
import           Data.Either
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Vinyl
import qualified Exception as G
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.SemanticTypes
import           Control.Monad.IO.Class
import           System.Process

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
      IdeResponseOk . T.pack <$> readProcess "hoogle" ["--info", T.unpack expr] ""

-- ---------------------------------------------------------------------

lookupCmd :: CommandFunc T.Text
lookupCmd = CmdSync $ \_ctxs req -> do
  case getParams (IdText "term" :& RNil) req of
    Left err -> return err
    Right (ParamText term :& RNil) -> liftIO $ do
      IdeResponseOk . T.pack <$> readProcess "hoogle" ["-q", T.unpack term, "--count=10"] ""
