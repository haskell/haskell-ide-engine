{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings    #-}
-- | A data structure to define a plugin.
-- Allows description of a plugin and the commands it provides

module Haskell.Ide.Engine.PluginDescriptor
  ( runPluginCommand
  , mkIdePlugins
  , DynamicJSON
  , dynToJSON
  , fromDynJSON
  , toDynJSON
  ) where

import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.List
import qualified Data.Map                        as Map
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Text                       as T
import qualified Data.ConstrainedDynamic         as CD
import           Data.Typeable
import           Haskell.Ide.Engine.IdeFunctions
import           Haskell.Ide.Engine.MonadTypes

mkIdePlugins :: [PluginDescriptor] -> IdePlugins
mkIdePlugins = IdePlugins . foldr (uncurry Map.insert . f) Map.empty
  where f p = (pluginId p, p) --(pluginCommands p, pluginCodeActionProvider fmap (\x -> (pluginCommands x, pluginCodeActionProvider x))

type DynamicJSON = CD.ConstrainedDynamic ToJSON

dynToJSON :: DynamicJSON -> Value
dynToJSON x = CD.applyClassFn x toJSON

fromDynJSON :: (Typeable a, ToJSON a) => DynamicJSON -> Maybe a
fromDynJSON = CD.fromDynamic

toDynJSON :: (Typeable a, ToJSON a) => a -> DynamicJSON
toDynJSON = CD.toDyn

-- | Runs a plugin command given a PluginId, CommandName and
-- arguments in the form of a JSON object.
runPluginCommand :: CommandId -> Value -> IdeGhcM (IdeResult DynamicJSON)
runPluginCommand cmdId@(CommandId pId com) arg = do
  IdePlugins plugins <- lift . lift $ getPlugins
  case Map.lookup pId plugins of
    Nothing -> return $
      IdeResultFail $ IdeError UnknownPlugin ("Plugin " <> pId <> " doesn't exist") Null
    Just PluginDescriptor { pluginCommands = cmds } -> case find ((cmdId ==) . commandId) cmds of
      Nothing -> return $ IdeResultFail $
        IdeError UnknownCommand ("Command " <> com <> " isn't defined for plugin " <> pId <> ". Legal commands are: " <> T.pack(show $ map commandId cmds)) Null
      Just (PluginCommand _ _ (CmdSync f)) -> case fromJSON arg of
        Error err -> return $ IdeResultFail $
          IdeError ParameterError ("error while parsing args for " <> com <> " in plugin " <> pId <> ": " <> T.pack err) Null
        Success a -> do
            res <- f a
            return $ fmap toDynJSON res
