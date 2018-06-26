{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- For MonadLogger IO instance
module Haskell.Ide.Engine.MonadFunctions
  (
  -- * Logging functions
    logm
  , debugm
  , warningm
  , errorm
  , ExtensionClass(..)
  , put
  , modify
  , remove
  , gets
  , get
  ) where

import Control.Monad.IO.Class
import System.Log.Logger
import Data.Typeable
import Data.Dynamic
import qualified Data.Map as Map

import Haskell.Ide.Engine.MultiThreadState
import Haskell.Ide.Engine.PluginsIdeMonads

-- ---------------------------------------------------------------------

logm :: MonadIO m => String -> m ()
logm s = liftIO $ infoM "hie" s

debugm :: MonadIO m => String -> m ()
debugm s = liftIO $ debugM "hie" s

warningm :: MonadIO m => String -> m ()
warningm s = liftIO $ warningM "hie" s

errorm :: MonadIO m => String -> m ()
errorm s = liftIO $ warningM "hie" s

-- ---------------------------------------------------------------------
-- Extensible state, based on
-- http://xmonad.org/xmonad-docs/xmonad/XMonad-Core.html#t:ExtensionClass
--
-- | Every module must make the data it wants to store
-- an instance of this class.
--
-- Minimal complete definition: initialValue
class Typeable a => ExtensionClass a where
    -- | Defines an initial value for the state extension
    initialValue :: a

-- ---------------------------------------------------------------------

-- Based on the one in xmonad-contrib, original header below
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.ExtensibleState
-- Copyright   :  (c) Daniel Schoepe 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  daniel.schoepe@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Module for storing custom mutable state in xmonad.
--
-----------------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- $usage
--
-- To utilize this feature in a plugin, create a data type
-- and make it an instance of ExtensionClass. You can then use
-- the functions from this module for storing and retrieving your data:
--
-- > {-# LANGUAGE DeriveDataTypeable #-}
-- > import qualified Haskell.Ide.Engine.ExtensibleState as XS
-- >
-- > data ListStorage = ListStorage [Integer] deriving Typeable
-- > instance ExtensionClass ListStorage where
-- >   initialValue = ListStorage []
-- >
-- > .. XS.put (ListStorage [23,42])
--
-- To retrieve the stored value call:
--
-- > .. XS.get
--
-- If the type can't be inferred from the usage of the retrieved data, you
-- have to add an explicit type signature:
--
-- > .. XS.get :: X ListStorage
--
-- > data ListStorage = ListStorage [Integer] deriving (Typeable,Read,Show)
-- >
-- > instance ExtensionClass ListStorage where
-- >   initialValue = ListStorage []
--
-- A module should not try to store common datatypes(e.g. a list of Integers)
-- without a custom data type as a wrapper to avoid collisions with other modules
-- trying to store the same data type without a wrapper.
--

-- | Modify the map of state extensions by applying the given function.
modifyStateExts :: MonadMTState IdeState m => (Map.Map TypeRep Dynamic -> Map.Map TypeRep Dynamic) -> m ()
modifyStateExts f = modifyMTS $ \st -> st { extensibleState = f (extensibleState st) }

-- | Apply a function to a stored value of the matching type or the initial value if there
-- is none.
modify :: (MonadMTState IdeState m, ExtensionClass a) => (a -> a) -> m ()
modify f = put . f =<< get

-- | Add a value to the extensible state field. A previously stored value with the same
-- type will be overwritten. (More precisely: A value whose string representation of its type
-- is equal to the new one's)
put :: (MonadMTState IdeState m, ExtensionClass a) => a -> m ()
put v = modifyStateExts . Map.insert (typeOf v) . toDyn $ v

-- | Try to retrieve a value of the requested type, return an initial value if there is no such value.
get :: forall a m. (MonadMTState IdeState m, ExtensionClass a) => m a
get = do
  mc <- readMTS
  let v = (Map.lookup (typeRep (Proxy :: Proxy a)) . extensibleState) mc
  case v of
    Just dyn -> return $ fromDyn dyn initialValue
    _        -> return initialValue

gets :: (MonadMTState IdeState m, ExtensionClass a) => (a -> b) -> m b
gets = flip fmap get

-- | Remove the value from the extensible state field that has the same type as the supplied argument
remove :: (MonadMTState IdeState m, ExtensionClass a) => proxy a -> m ()
remove wit = modifyStateExts $ Map.delete (typeRep $ wit)

-- ---------------------------------------------------------------------
