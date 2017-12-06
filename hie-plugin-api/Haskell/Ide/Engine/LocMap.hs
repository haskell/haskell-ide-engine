-- | Functions to work with and generate LocMaps
-- which allow fast queries for all symbols at a
-- point in the source.
-- Uses `Data.IntervalMap.FingerTree` under the hood

module Haskell.Ide.Engine.LocMap
  (
    getArtifactsAtPos
  ) where

import           Haskell.Ide.Engine.PluginTypes

import qualified Data.IntervalMap.FingerTree    as IM
import qualified GhcMod.ModuleLoader            as GM

import           Haskell.Ide.Engine.PluginUtils

-- ---------------------------------------------------------------------
-- | Seaches for all the symbols at a point in the
-- given LocMap
getArtifactsAtPos :: Position -> IM.IntervalMap GM.Pos a -> [(Range, a)]
getArtifactsAtPos p im = map f $ IM.search (position2pos p) im
  where f (IM.Interval a b, x) = (Range (pos2position a) (pos2position b), x)
