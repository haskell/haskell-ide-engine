-- | Functions to work with and generate LocMaps
-- which allow fast queries for all symbols at a
-- point in the source.
-- Uses `Data.IntervalMap.FingerTree` under the hood

module Haskell.Ide.Engine.LocMap
  ( genLocMap
  , getNamesAtPos
  ) where

import           Data.Maybe
import           GHC                            (TypecheckedModule)
import           Haskell.Ide.Engine.PluginTypes

import qualified Data.Generics                  as SYB
import qualified Data.IntervalMap.FingerTree    as IM

import qualified GHC
import qualified Var

import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils

-- ---------------------------------------------------------------------

-- | Generates a LocMap from a TypecheckedModule,
-- which allows fast queries for all the symbols
-- located at a particular point in the source
genLocMap :: TypecheckedModule -> LocMap
genLocMap tm = names
  where
    typechecked = GHC.tm_typechecked_source tm
    renamed = fromJust $ GHC.tm_renamed_source tm

    rspToInt = uncurry IM.Interval . unpackRealSrcSpan

    names  = IM.union names2 $ SYB.everything IM.union (IM.empty `SYB.mkQ` hsRecFieldT) typechecked
    names2 = SYB.everything IM.union (IM.empty `SYB.mkQ`  fieldOcc
                                               `SYB.extQ` hsRecFieldN
                                               `SYB.extQ` checker) renamed

    checker (GHC.L (GHC.RealSrcSpan r) x) = IM.singleton (rspToInt r) x
    checker _                             = IM.empty

    fieldOcc :: GHC.FieldOcc GHC.Name -> LocMap
    fieldOcc (GHC.FieldOcc (GHC.L (GHC.RealSrcSpan r) _) n) = IM.singleton (rspToInt r) n
    fieldOcc _ = IM.empty

    hsRecFieldN :: GHC.LHsExpr GHC.Name -> LocMap
    hsRecFieldN (GHC.L _ (GHC.HsRecFld (GHC.Unambiguous (GHC.L (GHC.RealSrcSpan r) _) n) )) = IM.singleton (rspToInt r) n
    hsRecFieldN _ = IM.empty

    hsRecFieldT :: GHC.LHsExpr GHC.Id -> LocMap
    hsRecFieldT (GHC.L _ (GHC.HsRecFld (GHC.Ambiguous (GHC.L (GHC.RealSrcSpan r) _) n) )) = IM.singleton (rspToInt r) (Var.varName n)
    hsRecFieldT _ = IM.empty

-- | Seaches for all the symbols at a point in the
-- given LocMap
getNamesAtPos :: Position -> LocMap -> [(Range, GHC.Name)]
getNamesAtPos p im = map f $ IM.search p im
  where f (IM.Interval a b, x) = (Range a b, x)
