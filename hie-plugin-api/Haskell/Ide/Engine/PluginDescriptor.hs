{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings    #-}
-- | A data structure to define a plugin.
-- Allows description of a plugin and the commands it provides

module Haskell.Ide.Engine.PluginDescriptor
  ( runPluginCommand
  , pluginDescToIdePlugins
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

-- ---------------------------------------------------------------------

pluginDescToIdePlugins :: [PluginDescriptor] -> IdePlugins
pluginDescToIdePlugins plugins = IdePlugins $ Map.fromList $ map (\p -> (pluginId p, p)) plugins

type DynamicJSON = CD.ConstrainedDynamic ToJSON

dynToJSON :: DynamicJSON -> Value
dynToJSON x = CD.applyClassFn x toJSON

fromDynJSON :: (Typeable a, ToJSON a) => DynamicJSON -> Maybe a
fromDynJSON = CD.fromDynamic

toDynJSON :: (Typeable a, ToJSON a) => a -> DynamicJSON
toDynJSON = CD.toDyn

-- | Runs a plugin command given a PluginId, CommandName and
-- arguments in the form of a JSON object.
runPluginCommand :: PluginId -> CommandName -> Value
                 -> IdeGhcM (IdeResult DynamicJSON)
runPluginCommand p com arg = do
  (IdePlugins m) <- lift $ lift $ lift getPlugins
  case Map.lookup p m of
    Nothing -> return $
      IdeResultFail $ IdeError UnknownPlugin ("Plugin " <> p <> " doesn't exist") Null
    Just (PluginDescriptor { pluginCommands = xs }) -> case find ((com ==) . commandName) xs of
      Nothing -> return $ IdeResultFail $
        IdeError UnknownCommand ("Command " <> com <> " isn't defined for plugin " <> p <> ". Legal commands are: " <> T.pack(show $ map commandName xs)) Null
      Just (PluginCommand _ _ (CmdSync f)) -> case fromJSON arg of
        Error err -> return $ IdeResultFail $
          IdeError ParameterError ("error while parsing args for " <> com <> " in plugin " <> p <> ": " <> T.pack err) Null
        Success a -> do
            res <- f a
            return $ fmap toDynJSON res
