{-# LANGUAGE CPP #-}
module Haskell.Ide.Engine.ArtifactMap where

import Data.Maybe
import qualified Data.IntervalMap.FingerTree       as IM
import qualified Data.Generics                     as SYB

import GhcMod.SrcUtils

import qualified GHC
import           GHC                               (TypecheckedModule)
import qualified SrcLoc                            as GHC
import qualified Var
import qualified GhcMod.Gap                        as GM

import           Language.Haskell.LSP.Types

-- ---------------------------------------------------------------------

type SourceMap a = IM.IntervalMap Position a
type LocMap = SourceMap GHC.Name
type TypeMap = SourceMap GHC.Type


genTypeMap :: GHC.GhcMonad m => TypecheckedModule -> m TypeMap
genTypeMap tm = do
    ts <- collectAllSpansTypes True tm
    return $ foldr go IM.empty ts
  where
    go (GHC.RealSrcSpan spn, typ) im =
      IM.insert (uncurry IM.Interval $ unpackRealSrcSpan spn) typ im
    go _ im = im

-- | Generates a LocMap from a TypecheckedModule,
-- which allows fast queries for all the symbols
-- located at a particular point in the source
genLocMap :: TypecheckedModule -> LocMap
genLocMap tm = names
  where
    typechecked = GHC.tm_typechecked_source tm
    renamed = fromJust $ GHC.tm_renamed_source tm

    rspToInt = uncurry IM.Interval . unpackRealSrcSpan

#if __GLASGOW_HASKELL__ > 710
    names  = IM.union names2 $ SYB.everything IM.union (IM.empty `SYB.mkQ` hsRecFieldT) typechecked
#else
    names = names2
#endif
    names2 = SYB.everything IM.union (IM.empty
#if __GLASGOW_HASKELL__ > 710
                                               `SYB.mkQ`  fieldOcc
                                               `SYB.extQ` hsRecFieldN
                                               `SYB.extQ` checker) renamed
#else
                                               `SYB.mkQ` checker) renamed
#endif

    checker (GHC.L (GHC.RealSrcSpan r) x) = IM.singleton (rspToInt r) x
    checker _                             = IM.empty

#if __GLASGOW_HASKELL__ > 710
    fieldOcc :: GHC.FieldOcc GM.GhcRn -> LocMap
    fieldOcc (GHC.FieldOcc (GHC.L (GHC.RealSrcSpan r) _) n) = IM.singleton (rspToInt r) n
    fieldOcc _ = IM.empty

    hsRecFieldN :: GHC.LHsExpr GM.GhcRn -> LocMap
    hsRecFieldN (GHC.L _ (GHC.HsRecFld (GHC.Unambiguous (GHC.L (GHC.RealSrcSpan r) _) n) )) = IM.singleton (rspToInt r) n
    hsRecFieldN _ = IM.empty

    hsRecFieldT :: GHC.LHsExpr GM.GhcTc -> LocMap
    hsRecFieldT (GHC.L _ (GHC.HsRecFld (GHC.Ambiguous (GHC.L (GHC.RealSrcSpan r) _) n) )) = IM.singleton (rspToInt r) (Var.varName n)
    hsRecFieldT _ = IM.empty
#endif

-- -- | Seaches for all the symbols at a point in the
-- -- given LocMap
-- getNamesAtPos :: Position -> LocMap -> [((Position,Position), GM.GhcRn)]
-- getNamesAtPos p im = map f $ IM.search p im

getArtifactsAtPos :: Position -> SourceMap a -> [(Range, a)]
getArtifactsAtPos p im = map f $ IM.search p im
  where f (IM.Interval a b, x) = (Range a b, x)

unpackRealSrcSpan :: GHC.RealSrcSpan -> (Position, Position)
unpackRealSrcSpan rspan =
  (toPos (l1,c1),toPos (l2,c2))
  where s  = GHC.realSrcSpanStart rspan
        l1 = GHC.srcLocLine s
        c1 = GHC.srcLocCol s
        e  = GHC.realSrcSpanEnd rspan
        l2 = GHC.srcLocLine e
        c2 = GHC.srcLocCol e

-- | Converts to one based tuple
unPos :: Position -> (Int,Int)
unPos (Position l c) = (l+1,c+1)

-- | Converts from one based tuple
toPos :: (Int,Int) -> Position
toPos (l,c) = Position (l-1) (c-1)

-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
