{-# LANGUAGE OverloadedStrings #-}
-- | A data structure to define a plugin.
-- Allows description of a plugin and the commands it provides

module Haskell.Ide.Engine.PluginDescriptor
  ( runPluginCommand
  , pluginDescToIdePlugins
  ) where

import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.List
import qualified Data.Map                        as Map
import           Data.Monoid
import qualified Data.Text                       as T
import           Haskell.Ide.Engine.IdeFunctions
import           Haskell.Ide.Engine.MonadTypes

pluginDescToIdePlugins :: [(PluginId,PluginDescriptor)] -> IdePlugins
pluginDescToIdePlugins = IdePlugins . foldr (uncurry Map.insert . f) Map.empty
  where f = fmap pluginCommands

-- | Runs a plugin command given a PluginId, CommandName and
-- arguments in the form of a JSON object.
-- Needs a callback to handle asynchronous commands.
-- Use an MVar to get the result in a synchronous fashion
-- @
--   mv  <- liftIO $ newEmptyMVar
--   runPluginCommand plugin command args (putMVar mv)
--   res <- liftIO $ takeMVar mv
-- @
runPluginCommand :: PluginId -> CommandName -> Value -> (IdeResponse Value -> IO ()) -> IdeM ()
runPluginCommand p com arg callback = do
  let ret = liftIO . callback
  (IdePlugins m) <- getPlugins
  case Map.lookup p m of
    Nothing -> ret $
      IdeResponseFail $ IdeError UnknownPlugin ("Plugin " <> p <> " doesn't exist") Null
    Just xs -> case find ((com ==) . commandName) xs of
      Nothing -> ret $ IdeResponseFail $
        IdeError UnknownCommand ("Command " <> com <> " isn't defined for plugin " <> p <> ". Legal commands are: " <> T.pack(show $ map commandName xs)) Null
      Just (PluginCommand _ _ cf) -> case fromJSON arg of
        Error err -> ret $ IdeResponseFail $
          IdeError ParameterError ("error while parsing args for " <> com <> " in plugin " <> p <> ": " <> T.pack err) Null
        Success a -> case cf of
          CmdSync f -> do
            res <- f a
            ret $ fmap toJSON res
          CmdAsync f ->
            f (callback . fmap toJSON) a
