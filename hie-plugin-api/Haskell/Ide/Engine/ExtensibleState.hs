{-# LANGUAGE ScopedTypeVariables #-}
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

module Haskell.Ide.Engine.ExtensibleState (
                              -- * Usage
                              -- $usage
                              put
                              , modify
                              , remove
                              , get
                              , gets
                              ) where

import qualified Control.Monad.State.Strict    as State
import           Control.Monad.Trans.Class
import           Data.Dynamic
import qualified Data.Map                      as M
import           Haskell.Ide.Engine.MonadTypes

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
modifyStateExts :: (M.Map TypeRep Dynamic
                   -> M.Map TypeRep Dynamic)
                -> IdeM ()
modifyStateExts f = lift $ lift $ State.modify $ \st -> st { extensibleState = f (extensibleState st) }

-- | Apply a function to a stored value of the matching type or the initial value if there
-- is none.
modify :: ExtensionClass a => (a -> a) -> IdeM ()
modify f = put . f =<< get

-- | Add a value to the extensible state field. A previously stored value with the same
-- type will be overwritten. (More precisely: A value whose string representation of its type
-- is equal to the new one's)
put :: ExtensionClass a => a -> IdeM ()
put v = modifyStateExts . M.insert (typeOf $ v) . toDyn $ v

-- | Try to retrieve a value of the requested type, return an initial value if there is no such value.
get :: forall a. ExtensionClass a => IdeM a
get =
  do v <-
       lift $
       lift $
       State.gets $ M.lookup (typeRep $ (Proxy :: Proxy a)) . extensibleState
     case v of
       Just dyn -> return $ fromDyn dyn initialValue
       _        -> return initialValue

gets :: ExtensionClass a => (a -> b) -> IdeM b
gets = flip fmap get

-- | Remove the value from the extensible state field that has the same type as the supplied argument
remove :: ExtensionClass a => proxy a -> IdeM ()
remove wit = modifyStateExts $ M.delete (typeRep $ wit)
