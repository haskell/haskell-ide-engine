    module Haskell.Ide.Engine.ArtifactMap where

import           Data.Maybe
import qualified Data.IntervalMap.FingerTree       as IM
import qualified Data.Generics                     as SYB

import qualified GHC
import           GHC                               (TypecheckedModule)
import qualified SrcLoc                            as GHC
import qualified Var
import Haskell.Ide.Engine.GhcCompat

import           Language.Haskell.LSP.Types

-- ---------------------------------------------------------------------

type SourceMap a = IM.IntervalMap Position a
type LocMap      = SourceMap GHC.Name
type TypeMap     = SourceMap GHC.Type
type ModuleMap   = SourceMap GHC.ModuleName
type DefMap      = SourceMap GHC.RdrName

-- TODO: Maybe look at using ranges instead of positions

-- ---------------------------------------------------------------------

genIntervalMap :: [(Position,Position,a)] -> SourceMap a
genIntervalMap ts = foldr go IM.empty ts
  where
    go (l,h,x) im = IM.insert (IM.Interval l h) x im

-- ---------------------------------------------------------------------

-- | Generates a LocMap from a TypecheckedModule,
-- which allows fast queries for all the symbols
-- located at a particular point in the source
genLocMap :: TypecheckedModule -> LocMap
genLocMap tm = names
  where
    typechecked = GHC.tm_typechecked_source tm
    renamed = fromJust $ GHC.tm_renamed_source tm


    names  = IM.union names2 $ SYB.everything IM.union (IM.empty `SYB.mkQ` hsRecFieldT) typechecked
    names2 = SYB.everything IM.union (IM.empty
                                               `SYB.mkQ`  fieldOcc
                                               `SYB.extQ` hsRecFieldN
                                               `SYB.extQ` checker) renamed

    checker (GHC.L (GHC.RealSrcSpan r) x) = IM.singleton (rspToInt r) x
    checker _                             = IM.empty

    fieldOcc :: GHC.FieldOcc GhcRn -> LocMap
    fieldOcc (FieldOccCompat n (GHC.L (GHC.RealSrcSpan r) _)) = IM.singleton (rspToInt r) n
    fieldOcc _ = IM.empty

    hsRecFieldN :: GHC.LHsExpr GhcRn -> LocMap
    hsRecFieldN (GHC.L _ (HsRecFldCompat (UnambiguousCompat n (GHC.L (GHC.RealSrcSpan r) _)) )) = IM.singleton (rspToInt r) n
    hsRecFieldN _ = IM.empty

    hsRecFieldT :: GHC.LHsExpr GhcTc -> LocMap
    hsRecFieldT (GHC.L _ (HsRecFldCompat (AmbiguousCompat n (GHC.L (GHC.RealSrcSpan r) _)) )) = IM.singleton (rspToInt r) (Var.varName n)
    hsRecFieldT _ = IM.empty

-- | Generates a ModuleMap of imported and exported modules names,
-- and the locations that they were imported/exported at.
genImportMap :: TypecheckedModule -> ModuleMap
genImportMap tm = moduleMap
  where
    (_, lImports, mlies, _) = fromJust $ GHC.tm_renamed_source tm

    lies = map fst $ fromMaybe [] mlies

    moduleMap :: ModuleMap
    moduleMap = foldl goImp IM.empty lImports `IM.union` foldl goExp IM.empty lies

    goImp :: ModuleMap -> GHC.LImportDecl a -> ModuleMap
    goImp acc (GHC.L (GHC.RealSrcSpan r) i) = IM.insert (rspToInt r) (GHC.unLoc $ GHC.ideclName i) acc
    goImp acc _ = acc

    goExp :: ModuleMap -> GHC.LIE name -> ModuleMap
    goExp acc (GHC.L (GHC.RealSrcSpan r) (IEModuleContentsCompat lmn)) =
      IM.insert (rspToInt r) (GHC.unLoc lmn) acc
    goExp acc _ = acc

-- | Generates a map of function definitions and types
-- i.e. top-level bindings and their `where` clauses
genDefMap :: TypecheckedModule -> DefMap
genDefMap tm = mconcat $ map (go . GHC.unLoc) decls
  where
    go :: GHC.HsDecl GhcPs -> DefMap
    -- Type signatures
    go (SigDCompat (TypeSigCompat lns _)) =
      foldl IM.union mempty $ fmap go' lns
      where go' (GHC.L (GHC.RealSrcSpan r) n) = IM.singleton (rspToInt r) n
            go' _ = mempty
    -- Definitions
    go (ValDCompat (FunBindCompat (GHC.L (GHC.RealSrcSpan r) n) (GHC.MG { GHC.mg_alts = llms }))) =
      IM.insert (rspToInt r) n wheres
      where
        wheres = mconcat $ fmap (gomatch . GHC.unLoc) (GHC.unLoc llms)

        gomatch (MatchCompat lbs) = golbs (GHC.unLoc lbs)

        golbs (HsValBindsCompat (ValBindsCompat lhsbs lsigs)) =
          foldl (\acc x -> IM.union acc (go $ ValDCompat $ GHC.unLoc x)) mempty lhsbs
            `mappend` foldl IM.union mempty (fmap (go . SigDCompat . GHC.unLoc) lsigs)
        golbs _ = mempty
    go _ = mempty
    decls = GHC.hsmodDecls $ GHC.unLoc $ GHC.pm_parsed_source $ GHC.tm_parsed_module tm

-- | Converts a RealSrcSpan to an interval for an IntervalMap.
rspToInt :: GHC.RealSrcSpan -> IM.Interval Position
rspToInt = uncurry IM.Interval . unpackRealSrcSpan

-- -- | Seaches for all the symbols at a point in the
-- -- given LocMap
-- getNamesAtPos :: Position -> LocMap -> [((Position,Position), GhcRn)]
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
