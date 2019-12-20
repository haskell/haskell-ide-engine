-- Copyright 2017 Google Inc.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -w #-}

-- | Module trying to expose a unified (or at least simplified) view of the GHC
-- AST changes across multiple compiler versions.
module Haskell.Ide.Engine.GhcCompat where

import Control.Arrow ((&&&))
import qualified Digraph

#if __GLASGOW_HASKELL__ >= 804
import qualified EnumSet as ES
import qualified HsExtension as GHC
#else
import qualified Data.IntSet as ES
#endif

import CmdLineParser

#if __GLASGOW_HASKELL__ >= 800
import Module (UnitId, unitIdString)
import qualified Bag
#else
import Module (Module, packageKeyString, modulePackageKey)
#endif

#if __GLASGOW_HASKELL__ < 802
import HsDecls (hs_instds)
#endif

#if __GLASGOW_HASKELL__ < 800
import GHC (PackageKey)
import SrcLoc (combineSrcSpans)
#endif

import HsBinds (HsBindLR(..), Sig(..), LHsBinds, abe_mono, abe_poly)
import HsDecls (ConDecl(..), TyClDecl(ClassDecl, DataDecl, SynDecl))
import HsExpr (HsExpr(..), HsRecordBinds)
import qualified HsTypes
import HsTypes (HsType(HsTyVar), LHsType)
import Id (Id)
import Name (Name)
import RdrName (RdrName)
import Outputable (Outputable)
import SrcLoc (Located, GenLocated(L), unLoc, getLoc)
import qualified GHC
import GHC hiding (GhcPs, GhcRn, GhcTc)

#if __GLASGOW_HASKELL__ < 804
type GhcPs = RdrName
type GhcRn = Name
type GhcTc = Id
type IdP a = a
#else
type GhcPs = GHC.GhcPs
type GhcRn = GHC.GhcRn
type GhcTc = GHC.GhcTc
#endif

#if __GLASGOW_HASKELL__ >= 800
showPackageName :: UnitId -> String
showPackageName = unitIdString
#else
showPackageName :: PackageKey -> String
showPackageName = packageKeyString
-- | Backfilling.
moduleUnitId :: Module -> PackageKey
moduleUnitId = modulePackageKey
#endif

-- | In GHC before 8.0.1 less things had Located wrappers, so no-op there.
-- Drops the Located on newer GHCs.
#if __GLASGOW_HASKELL__ >= 800
mayUnLoc :: Located a -> a
mayUnLoc = unLoc
#else
mayUnLoc :: a -> a
mayUnLoc = id
#endif

#if __GLASGOW_HASKELL__ < 802
-- | Backfilling.
hsGroupInstDecls = hs_instds
#endif

pattern RecordConCompat :: Located Id -> HsRecordBinds GhcTc -> HsExpr GhcTc
pattern RecordConCompat lConId recBinds <-
#if __GLASGOW_HASKELL__ >= 806
    RecordCon _ lConId recBinds
#elif __GLASGOW_HASKELL__ >= 800
    RecordCon lConId _ _ recBinds
#else
    RecordCon lConId _ recBinds
#endif

pattern DataDeclCompat locName binders defn <-
#if __GLASGOW_HASKELL__ >= 806
    DataDecl _ locName binders _ defn
#elif __GLASGOW_HASKELL__ >= 802
    DataDecl locName binders _ defn _ _
#elif __GLASGOW_HASKELL__ >= 800
    DataDecl locName binders defn _ _
#else
    DataDecl locName binders defn _
#endif

pattern SynDeclCompat locName binders <-
#if __GLASGOW_HASKELL__ >= 806
    SynDecl _ locName binders _ _
#elif __GLASGOW_HASKELL__ >= 802
    SynDecl locName binders _ _ _
#else
    SynDecl locName binders _ _
#endif

pattern FunBindCompat funId funMatches <-
#if __GLASGOW_HASKELL__ >= 806
    FunBind _ funId funMatches _ _
#elif __GLASGOW_HASKELL__ >= 800
    FunBind funId funMatches _ _ _
#else
    FunBind funId _ funMatches _ _ _
#endif

pattern TypeSigCompat names ty <-
#if __GLASGOW_HASKELL__ >= 806
    TypeSig _ names ty
#elif __GLASGOW_HASKELL__ >= 800
    TypeSig names ty
#else
    TypeSig names ty _
#endif



#if __GLASGOW_HASKELL__ >= 800
namesFromHsIbWc :: HsTypes.LHsSigWcType GhcRn -> [Name]
namesFromHsIbSig :: HsTypes.LHsSigType GhcRn -> [Name]
namesFromHsWC :: HsTypes.LHsWcType GhcRn -> [Name]
-- | Monomorphising type so uniplate is happier.
#if __GLASGOW_HASKELL__ >= 808
namesFromHsIbSig = HsTypes.hsib_ext
#elif __GLASGOW_HASKELL__ >= 806
namesFromHsIbSig = hsib_vars . HsTypes.hsib_ext
#else
namesFromHsIbSig = HsTypes.hsib_vars
#endif

#if __GLASGOW_HASKELL__ <= 804
namesFromHsWC = HsTypes.hswc_wcs
#else
namesFromHsWC = HsTypes.hswc_ext
#endif

namesFromHsIbWc =
    -- No, can't use the above introduced names, because the types resolve
    -- differently here. Type-level functions FTW.
#if __GLASGOW_HASKELL__ <= 800
    HsTypes.hsib_vars
#elif __GLASGOW_HASKELL__ <= 804
    HsTypes.hswc_wcs
#else
    HsTypes.hswc_ext
#endif
#endif

data ClsSigBound = forall a. Outputable a => ClsSigBound ![Located Name] a

clsSigBound (TypeSigCompat ns ty) = Just (ClsSigBound ns ty)
#if __GLASGOW_HASKELL__ >= 806
clsSigBound (ClassOpSig _ _ ns ty)
#elif __GLASGOW_HASKELL__ >= 800
clsSigBound (ClassOpSig _ ns ty)
#endif
  = Just (ClsSigBound ns ty)
-- TODO(robinpalotai): PatSynSig
clsSigBound _ = Nothing

pattern ClassDeclCompat locName binders sigs <-
#if __GLASGOW_HASKELL__ >= 806
    ClassDecl _ _ locName binders _ _ sigs _ _ _ _
#elif __GLASGOW_HASKELL__ >= 802
    ClassDecl _ locName binders _ _ sigs _ _ _ _ _
#else
    ClassDecl _ locName binders _ sigs _ _ _ _ _
#endif

#if __GLASGOW_HASKELL__ >= 806
conDeclNames (ConDeclH98 { con_name = conName })  = [conName]
conDeclNames (ConDeclGADT { con_names = conNames }) = conNames
#elif __GLASGOW_HASKELL__ >= 800
conDeclNames (ConDeclH98 conName _ _ _ _) = [conName]
conDeclNames (ConDeclGADT conNames _ _) = conNames
#else
conDeclNames (ConDecl conNames _ _ _ _ _ _ _) = conNames
#endif

data AbsBindsKind = NormalAbs | SigAbs
    deriving (Eq)

#if __GLASGOW_HASKELL__ >= 804
maybeAbsBinds :: HsBindLR a b
              -> Maybe (LHsBinds a, [(IdP a, Maybe (IdP a))], AbsBindsKind)
#else
maybeAbsBinds :: HsBindLR a b
              -> Maybe (LHsBinds a, [(a, Maybe a)], AbsBindsKind)
#endif
maybeAbsBinds abs@(AbsBinds { abs_exports = exports,  abs_binds = binds}) =
    let ids = map (abe_poly &&& (Just . abe_mono)) exports
        binds_type =
#if __GLASGOW_HASKELL__ >= 804
          if abs_sig abs then SigAbs else NormalAbs
#else
          NormalAbs
#endif
    in Just $! (binds, ids, binds_type)
#if __GLASGOW_HASKELL__ >= 800 && __GLASGOW_HASKELL__ < 804
maybeAbsBinds (AbsBindsSig _ _ poly _ _ bind) =
    let binds = Bag.unitBag bind
        ids = [(poly, Nothing)]
    in Just $! (binds, ids, SigAbs)
#endif
maybeAbsBinds _ = Nothing

pattern AbsBindsCompat binds ids abskind <-
    (maybeAbsBinds -> Just (binds, ids, abskind))

-- | Represents various spans of 'instance' declarations separately.
data SplitInstType = SplitInstType
    { onlyClass :: !Name
    , classAndInstance :: !(LHsType GhcRn)
      -- ^ The location is properly set to the span of 'Cls Inst'
    }



#if __GLASGOW_HASKELL__ >= 800
mySplitInstanceType :: HsTypes.LHsSigType GhcRn -> Maybe SplitInstType
mySplitInstanceType ty = do
    let (_, body) = HsTypes.splitLHsForAllTy (HsTypes.hsSigType ty)
    clsName <- HsTypes.getLHsInstDeclClass_maybe ty
    Just $! SplitInstType
        { onlyClass = unLoc clsName
        , classAndInstance = body
        }
#else
mySplitInstanceType :: LHsType Name -> Maybe SplitInstType
mySplitInstanceType ty = do
    (_, _, L clsL clsName, instLTys) <- HsTypes.splitLHsInstDeclTy_maybe ty
    let clsInstTy = HsTypes.mkHsAppTys (L clsL (HsTypes.HsTyVar clsName))
                                       instLTys
        combinedLoc = foldr (combineSrcSpans . getLoc) clsL instLTys
    Just $! SplitInstType
        { onlyClass = clsName
        , classAndInstance = L combinedLoc clsInstTy
        }
#endif

#if __GLASGOW_HASKELL__ >= 806
hsTypeVarName :: HsType GhcRn -> Maybe (Located Name)
hsTypeVarName (HsTyVar _ _ n) = Just $! n
#elif __GLASGOW_HASKELL__ >= 802
hsTypeVarName :: HsType GhcRn -> Maybe (Located Name)
hsTypeVarName (HsTyVar _ n) = Just $! n
#elif __GLASGOW_HASKELL__ >= 800
hsTypeVarName :: HsType Name -> Maybe (Located Name)
hsTypeVarName (HsTyVar n) = Just $! n
#else
hsTypeVarName :: HsType Name -> Maybe Name
hsTypeVarName (HsTyVar n) = Just $! n
#endif
hsTypeVarName _ = Nothing


getWarnMsg :: Warn -> String
#if __GLASGOW_HASKELL__ >= 804
getWarnMsg = unLoc . warnMsg
#else
getWarnMsg = unLoc

type Warn = Located String
#endif


#if __GLASGOW_HASKELL__ < 804
needsTemplateHaskellOrQQ = needsTemplateHaskell
#endif

mgModSummaries :: GHC.ModuleGraph -> [GHC.ModSummary]
#if __GLASGOW_HASKELL__ < 804
mgModSummaries = id
#else
mgModSummaries = GHC.mgModSummaries
#endif

#if __GLASGOW_HASKELL__ < 806
pattern HsForAllTyCompat binders <- HsForAllTy binders _
#else
pattern HsForAllTyCompat binders <- HsForAllTy _ binders _
#endif

#if __GLASGOW_HASKELL__ < 806
pattern UserTyVarCompat n <- UserTyVar n
pattern KindedTyVarCompat n <- KindedTyVar n _
#else
pattern UserTyVarCompat n <- UserTyVar _ n
pattern KindedTyVarCompat n <- KindedTyVar _ n _
#endif

pattern HsVarCompat v <-
#if __GLASGOW_HASKELL__ < 806
  HsVar v
#else
  HsVar _ v
#endif

pattern HsWrapCompat e <-
#if __GLASGOW_HASKELL__ < 806
  HsWrap _ e
#else
  HsWrap _ _ e
#endif

pattern HsParCompat e <-
#if __GLASGOW_HASKELL__ < 806
  HsPar e
#else
  HsPar _ e
#endif

pattern SectionLCompat e <-
#if __GLASGOW_HASKELL__ < 806
  SectionL _ e
#else
  SectionL _ _ e
#endif

pattern SectionRCompat e <-
#if __GLASGOW_HASKELL__ < 806
  SectionR _ e
#else
  SectionR _ _ e
#endif

pattern HsAppCompat f <-
#if __GLASGOW_HASKELL__ < 806
  HsApp f _
#else
  HsApp _ f _
#endif

pattern VarPatCompat v <-
#if __GLASGOW_HASKELL__ < 806
  VarPat v
#else
  VarPat _ v
#endif


#if __GLASGOW_HASKELL__ >= 802
pattern HsConLikeOutCompat v <-
#if __GLASGOW_HASKELL__ < 806
  HsConLikeOut v
#elif __GLASGOW_HASKELL__
  HsConLikeOut _ v
#endif
#endif

pattern RecordUpdCompat r dcs <-
#if __GLASGOW_HASKELL__ < 806
  RecordUpd _ r dcs _ _ _
#else
  RecordUpd (RecordUpdTc dcs _ _ _) _ r
#endif

pattern AsPatCompat asVar <-
#if __GLASGOW_HASKELL__ < 806
  AsPat (L _ asVar) _
#else
  AsPat _ (L _ asVar) _
#endif

pattern ClsInstDCompat v <-
#if __GLASGOW_HASKELL__ < 806
  ClsInstD v
#else
  ClsInstD _ v
#endif

pattern ClsInstDeclCompat lty lbinds  <-
#if __GLASGOW_HASKELL__ < 806
  ClsInstDecl lty lbinds _ _ _ _
#else
  ClsInstDecl _ lty lbinds _ _ _ _
#endif

pattern FieldOccCompat n l <-
#if __GLASGOW_HASKELL__ < 806
  FieldOcc l n
#else
  FieldOcc n l
#endif

pattern UnambiguousCompat n l <-
#if __GLASGOW_HASKELL__ < 806
  Unambiguous l n
#else
  Unambiguous n l
#endif

pattern AmbiguousCompat n l <-
#if __GLASGOW_HASKELL__ < 806
  Ambiguous l n
#else
  Ambiguous n l
#endif

pattern HsRecFldCompat f <-
#if __GLASGOW_HASKELL__ < 806
  HsRecFld f
#else
  HsRecFld _ f
#endif

pattern IEModuleContentsCompat f <-
#if __GLASGOW_HASKELL__ < 806
  IEModuleContents f
#else
  IEModuleContents _ f
#endif

pattern HsValBindsCompat f <-
#if __GLASGOW_HASKELL__ < 806
  HsValBinds f
#else
  HsValBinds _ f
#endif

pattern ValBindsCompat f g <-
#if __GLASGOW_HASKELL__ < 806
  ValBindsIn f g
#else
  ValBinds _ f g
#endif


#if __GLASGOW_HASKELL__ < 806
pattern ValDCompat f <-
  ValD f
  where
    ValDCompat f = ValD f
#else
pattern ValDCompat :: HsBind (GhcPass p) -> HsDecl (GhcPass p)
pattern ValDCompat f <-
  ValD _ f
  where
    ValDCompat f = ValD NoExt f
#endif

#if __GLASGOW_HASKELL__ < 806
pattern SigDCompat f <-
  SigD f
  where
    SigDCompat f = SigD f
#else
pattern SigDCompat :: Sig (GhcPass p) -> HsDecl (GhcPass p)
pattern SigDCompat f <-
  SigD _ f
  where
    SigDCompat f = SigD NoExt f
#endif


{-# COMPLETE MatchCompat #-}

pattern MatchCompat ms <-
#if __GLASGOW_HASKELL__ < 806
  ((GHC.grhssLocalBinds . GHC.m_grhss) -> ms)
#else
  (gomatch' -> ms)

gomatch' GHC.Match { GHC.m_grhss = GHC.GRHSs { GHC.grhssLocalBinds = lbs } } = lbs
gomatch' GHC.XMatch{} = error "GHC.XMatch"
gomatch' (GHC.Match _ _ _ (GHC.XGRHSs _)) = error "GHC.XMatch"
#endif


exportedSymbols :: GHC.TypecheckedModule -> Maybe ([LImportDecl GhcRn], Maybe [LIE GhcRn])
exportedSymbols tm =
  case GHC.renamedSource tm of
    Nothing -> Nothing
    Just (_, limport, mlies, _) ->
#if __GLASGOW_HASKELL__ >= 804
      Just (limport, fmap (map fst) mlies)
#else
      Just (limport, mlies)
#endif

emptyFatalWarningFlags :: DynFlags -> DynFlags
emptyFatalWarningFlags df = df { fatalWarningFlags = ES.empty }

-- Abstract Digraph

node_key :: Digraph.Node key payload -> key
node_key n =
#if __GLASGOW_HASKELL__ >= 804
  Digraph.node_key n
#else
  let (_, key, _) = n
  in key
#endif

node_payload :: Digraph.Node key payload -> payload
node_payload n =
#if __GLASGOW_HASKELL__ >= 804
  Digraph.node_payload n
#else
  let (payload, _, _) = n
  in payload
#endif

node_dependencies :: Digraph.Node key payload -> [key]
node_dependencies n =
#if __GLASGOW_HASKELL__ >= 804
  Digraph.node_dependencies n
#else
  let (_, _, deps) = n
  in deps
#endif

verticesG = Digraph.verticesG
