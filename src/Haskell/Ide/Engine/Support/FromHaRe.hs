{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Haskell.Ide.Engine.Support.FromHaRe
  (
    initRdrNameMap
  , NameMap
  , hsNamessRdr
  ) where

-- Code migrated from HaRe, until HaRe comes back

-- import Control.Monad.State
import Data.List
import Data.Maybe

import qualified GHC           as GHC
-- import qualified GhcMonad      as GHC
-- import qualified Haskell.Ide.Engine.PluginApi as HIE (makeRevRedirMapFunc)
import qualified Module        as GHC
import qualified Name          as GHC
import qualified Unique        as GHC
-- import qualified HscTypes      as GHC (md_exports)
-- import qualified TcRnTypes     as GHC (tcg_rdr_env)
#if __GLASGOW_HASKELL__ > 710
import qualified Var
#endif

import qualified Data.Generics as SYB

-- import Language.Haskell.GHC.ExactPrint
-- import Language.Haskell.GHC.ExactPrint.Annotate
-- import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Types

-- import Language.Haskell.Refact.Utils.Monad
-- import Language.Haskell.Refact.Utils.TypeSyn
-- import Language.Haskell.Refact.Utils.Types
import qualified Data.Map as Map

-- import Outputable

-- ---------------------------------------------------------------------

type NameMap = Map.Map GHC.SrcSpan GHC.Name
-- ---------------------------------------------------------------------

-- |We need the ParsedSource because it more closely reflects the actual source
-- code, but must be able to work with the renamed representation of the names
-- involved. This function constructs a map from every Located RdrName in the
-- ParsedSource to its corresponding name in the RenamedSource. It also deals
-- with the wrinkle that we need to Location of the RdrName to make sure we have
-- the right Name, but not all RdrNames have a Location.
-- This function is called before the RefactGhc monad is active.
initRdrNameMap :: GHC.TypecheckedModule -> NameMap
initRdrNameMap tm = r
  where
    parsed  = GHC.pm_parsed_source $ GHC.tm_parsed_module tm
    renamed = GHC.tm_renamed_source tm
#if __GLASGOW_HASKELL__ > 710
    typechecked = GHC.tm_typechecked_source tm
#endif

    checkRdr :: GHC.Located GHC.RdrName -> Maybe [(GHC.SrcSpan,GHC.RdrName)]
    checkRdr (GHC.L l n@(GHC.Unqual _)) = Just [(l,n)]
    checkRdr (GHC.L l n@(GHC.Qual _ _)) = Just [(l,n)]
    checkRdr (GHC.L _ _)= Nothing

    checkName :: GHC.Located GHC.Name -> Maybe [GHC.Located GHC.Name]
    checkName ln = Just [ln]

    rdrNames = gfromJust "initRdrNameMap" $ SYB.everything mappend (nameSybQuery checkRdr ) parsed
#if __GLASGOW_HASKELL__ >= 806
    names1   = gfromJust "initRdrNameMap" $ SYB.everything mappend (nameSybQuery checkName) renamed
    names2 = names1 ++ SYB.everything (++) ([] `SYB.mkQ` fieldOcc
                                              `SYB.extQ` hsRecFieldN) renamed
    names  = names2 ++ SYB.everything (++) ([] `SYB.mkQ` hsRecFieldT) typechecked

    fieldOcc :: GHC.FieldOcc GhcRn -> [GHC.Located GHC.Name]
    fieldOcc (GHC.FieldOcc n (GHC.L l _)) = [(GHC.L l n)]
    fieldOcc (GHC.XFieldOcc _) = []

    hsRecFieldN :: GHC.LHsExpr GhcRn -> [GHC.Located GHC.Name]
    hsRecFieldN (GHC.L _ (GHC.HsRecFld _ (GHC.Unambiguous n (GHC.L l _) ) )) = [GHC.L l n]
    hsRecFieldN _ = []

    hsRecFieldT :: GHC.LHsExpr GhcTc -> [GHC.Located GHC.Name]
    hsRecFieldT (GHC.L _ (GHC.HsRecFld _ (GHC.Ambiguous n (GHC.L l _)) )) = [GHC.L l (Var.varName n)]
    hsRecFieldT _ = []
#elif __GLASGOW_HASKELL__ > 710
    names1   = gfromJust "initRdrNameMap" $ SYB.everything mappend (nameSybQuery checkName) renamed
    names2 = names1 ++ SYB.everything (++) ([] `SYB.mkQ` fieldOcc
                                              `SYB.extQ` hsRecFieldN) renamed
    names  = names2 ++ SYB.everything (++) ([] `SYB.mkQ` hsRecFieldT) typechecked

    fieldOcc :: GHC.FieldOcc GhcRn -> [GHC.Located GHC.Name]
    fieldOcc (GHC.FieldOcc (GHC.L l _) n) = [(GHC.L l n)]

    hsRecFieldN :: GHC.LHsExpr GhcRn -> [GHC.Located GHC.Name]
    hsRecFieldN (GHC.L _ (GHC.HsRecFld (GHC.Unambiguous (GHC.L l _) n) )) = [GHC.L l n]
    hsRecFieldN _ = []

    hsRecFieldT :: GHC.LHsExpr GhcTc -> [GHC.Located GHC.Name]
    hsRecFieldT (GHC.L _ (GHC.HsRecFld (GHC.Ambiguous (GHC.L l _) n) )) = [GHC.L l (Var.varName n)]
    hsRecFieldT _ = []
#else
    names    = gfromJust "initRdrNameMap" $ SYB.everything mappend (nameSybQuery checkName) renamed
#endif

#if __GLASGOW_HASKELL__ >= 806
    namesIe = names
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)))
    -- This is a workaround for https://ghc.haskell.org/trac/ghc/ticket/14189
    -- namesIeParsedL = SYB.everything (++) ([] `SYB.mkQ` ieThingWith) (GHC.hsmodExports $ GHC.unLoc parsed)
    namesIeParsed = Map.fromList $ SYB.everything (++) ([] `SYB.mkQ` ieThingWith) (GHC.hsmodExports $ GHC.unLoc parsed)


    ieThingWith :: GHC.IE GhcPs -> [(GHC.SrcSpan, [GHC.SrcSpan])]
    ieThingWith (GHC.IEThingWith l _ sub_rdrs _) = [(GHC.getLoc l,map GHC.getLoc sub_rdrs)]
    ieThingWith _ = []

    renamedExports = case renamed of
                       Nothing -> Nothing
                       Just (_,_,es,_) -> es
    namesIeRenamed = SYB.everything (++) ([] `SYB.mkQ` ieThingWithNames) renamedExports

    ieThingWithNames :: GHC.IE GhcRn -> [GHC.Located GHC.Name]
    ieThingWithNames (GHC.IEThingWith l _ sub_rdrs _) = (GHC.ieLWrappedName l:nameSubs)
      where
        rdrSubLocs = gfromJust "ieThingWithNames" $ Map.lookup (GHC.getLoc l) namesIeParsed
        nameSubs = map (\(loc,GHC.L _ lwn) -> GHC.L loc (GHC.ieWrappedName lwn)) $ zip rdrSubLocs sub_rdrs
    ieThingWithNames _ = []

    namesIe = case SYB.everything mappend (nameSybQuery checkName) namesIeRenamed of
       Nothing -> names
       Just ns -> names ++ ns
#else
    namesIe = names
#endif

    nameMap = Map.fromList $ map (\(GHC.L l n) -> (l,n)) namesIe

    -- If the name does not exist (e.g. a TH Splice that has been expanded, make a new one)
    -- No attempt is made to make sure that equivalent ones have equivalent names.
    lookupName l n i = case Map.lookup l nameMap of
      Just v -> v
      Nothing -> case n of
                   GHC.Unqual u -> mkNewGhcNamePure 'h' i Nothing  (GHC.occNameString u)
#if __GLASGOW_HASKELL__ <= 710
                   GHC.Qual q u -> mkNewGhcNamePure 'h' i (Just (GHC.Module (GHC.stringToPackageKey "") q)) (GHC.occNameString u)
#else
                   GHC.Qual q u -> mkNewGhcNamePure 'h' i (Just (GHC.Module (GHC.stringToUnitId "") q)) (GHC.occNameString u)
#endif
                   _            -> error "initRdrNameMap:should not happen"

    r = Map.fromList $ map (\((l,n),i) -> (l,lookupName l n i)) $ zip rdrNames [1..]

-- ---------------------------------------------------------------------

nameSybQuery :: (SYB.Typeable a, SYB.Typeable t)
             => (GHC.Located a -> Maybe r) -> t -> Maybe r
nameSybQuery checker = q
  where
    q = Nothing `SYB.mkQ`  worker
#if __GLASGOW_HASKELL__ <= 710
                `SYB.extQ` workerBind
                `SYB.extQ` workerExpr
                `SYB.extQ` workerHsTyVarBndr
                `SYB.extQ` workerLHsType
#endif

    worker (pnt :: (GHC.Located a))
      = checker pnt

#if __GLASGOW_HASKELL__ <= 710
    workerBind (GHC.L l (GHC.VarPat name))
      = checker (GHC.L l name)
    workerBind _ = Nothing

    workerExpr ((GHC.L l (GHC.HsVar name)))
      = checker (GHC.L l name)
    workerExpr _ = Nothing

    -- workerLIE ((GHC.L _l (GHC.IEVar (GHC.L ln name))) :: (GHC.LIE a))
    --   = checker (GHC.L ln name)
    -- workerLIE _ = Nothing

    workerHsTyVarBndr ((GHC.L l (GHC.UserTyVar name)))
      = checker (GHC.L l name)
    workerHsTyVarBndr _ = Nothing

    workerLHsType ((GHC.L l (GHC.HsTyVar name)))
      = checker (GHC.L l name)
    workerLHsType _ = Nothing
#endif

-- ---------------------------------------------------------------------

mkNewGhcNamePure :: Char -> Int -> Maybe GHC.Module -> String -> GHC.Name
mkNewGhcNamePure c i maybeMod name =
  let un = GHC.mkUnique c i -- H for HaRe :)
      n = case maybeMod of
               Nothing   -> GHC.mkInternalName un      (GHC.mkVarOcc name) GHC.noSrcSpan
               Just modu -> GHC.mkExternalName un modu (GHC.mkVarOcc name) GHC.noSrcSpan
  in n

-- ---------------------------------------------------------------------

-- |Get all the names in the given syntax element
hsNamessRdr :: (SYB.Data t) => t -> [GHC.Located GHC.RdrName]
hsNamessRdr t = nub $ fromMaybe [] r
  where
     r = (SYB.everything mappend (inName) t)

     checker :: GHC.Located GHC.RdrName -> Maybe [GHC.Located GHC.RdrName]
     checker x = Just [x]

     inName :: (SYB.Typeable a) => a -> Maybe [GHC.Located GHC.RdrName]
     inName = nameSybQuery checker

-- ---------------------------------------------------------------------
