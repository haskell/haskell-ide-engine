{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Haskell.Ide.Engine.Transport.JsonHttp.Undecidable where

import Data.Singletons.Prelude
import Haskell.Ide.Engine.PluginDescriptor

data ContextMappingFun :: (TyFun AcceptedContext [ParamDescType]) -> *

type instance Apply ContextMappingFun a = ContextMapping a

type family CommandParams cxts params :: [ParamDescType] where
  CommandParams cxts params = ConcatMap ContextMappingFun cxts :++ params
