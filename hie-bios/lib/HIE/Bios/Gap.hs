{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, CPP #-}

module HIE.Bios.Gap (
    WarnFlags
  , emptyWarnFlags
  , makeUserStyle
  , getModuleName
  , getTyThing
  , fixInfo
  , getModSummaries
  , LExpression
  , LBinding
  , LPattern
  , inTypes
  , outType
  ) where

import DynFlags (DynFlags)
import GHC(LHsBind, LHsExpr, LPat, Type)
import HsExpr (MatchGroup)
import Outputable (PrintUnqualified, PprStyle, Depth(AllTheWay), mkUserStyle)

----------------------------------------------------------------
----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 802
#else
import GHC.PackageDb (ExposedModule(..))
#endif

#if __GLASGOW_HASKELL__ >= 804
import DynFlags (WarningFlag)
import qualified EnumSet as E (EnumSet, empty)
import GHC (mgModSummaries, ModSummary, ModuleGraph)
#else
import qualified Data.IntSet as I (IntSet, empty)
#endif

#if __GLASGOW_HASKELL__ >= 806
import HsExpr (MatchGroupTc(..))
import HsExtension (GhcTc)
import GHC (mg_ext)
#elif __GLASGOW_HASKELL__ >= 804
import HsExtension (GhcTc)
import GHC (mg_res_ty, mg_arg_tys)
#else
import GHC (Id, mg_res_ty, mg_arg_tys)
#endif

----------------------------------------------------------------
----------------------------------------------------------------

makeUserStyle :: DynFlags -> PrintUnqualified -> PprStyle
#if __GLASGOW_HASKELL__ >= 802
makeUserStyle dflags style = mkUserStyle dflags style AllTheWay
#else
makeUserStyle _      style = mkUserStyle        style AllTheWay
#endif

#if __GLASGOW_HASKELL__ >= 802
getModuleName :: (a, b) -> a
getModuleName = fst
#else
getModuleName :: ExposedModule unitid modulename -> modulename
getModuleName = exposedName
#endif

----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 804
type WarnFlags = E.EnumSet WarningFlag
emptyWarnFlags :: WarnFlags
emptyWarnFlags = E.empty
#else
type WarnFlags = I.IntSet
emptyWarnFlags :: WarnFlags
emptyWarnFlags = I.empty
#endif

#if __GLASGOW_HASKELL__ >= 804
getModSummaries :: ModuleGraph -> [ModSummary]
getModSummaries = mgModSummaries

getTyThing :: (a, b, c, d, e) -> a
getTyThing (t,_,_,_,_) = t

fixInfo :: (a, b, c, d, e) -> (a, b, c, d)
fixInfo (t,f,cs,fs,_) = (t,f,cs,fs)
#else
getModSummaries :: a -> a
getModSummaries = id

getTyThing :: (a, b, c, d) -> a
getTyThing (t,_,_,_) = t

fixInfo :: (a, b, c, d) -> (a, b, c, d)
fixInfo = id
#endif

----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 806
type LExpression = LHsExpr GhcTc
type LBinding    = LHsBind GhcTc
type LPattern    = LPat    GhcTc

inTypes :: MatchGroup GhcTc LExpression -> [Type]
inTypes = mg_arg_tys . mg_ext
outType :: MatchGroup GhcTc LExpression -> Type
outType = mg_res_ty . mg_ext
#elif __GLASGOW_HASKELL__ >= 804
type LExpression = LHsExpr GhcTc
type LBinding    = LHsBind GhcTc
type LPattern    = LPat    GhcTc

inTypes :: MatchGroup GhcTc LExpression -> [Type]
inTypes = mg_arg_tys
outType :: MatchGroup GhcTc LExpression -> Type
outType = mg_res_ty
#else
type LExpression = LHsExpr Id
type LBinding    = LHsBind Id
type LPattern    = LPat    Id

inTypes :: MatchGroup Id LExpression -> [Type]
inTypes = mg_arg_tys
outType :: MatchGroup Id LExpression -> Type
outType = mg_res_ty
#endif
