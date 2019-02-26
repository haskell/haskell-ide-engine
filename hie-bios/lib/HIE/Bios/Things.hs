module HIE.Bios.Things (
    GapThing(..)
  , fromTyThing
  , infoThing
  ) where

import ConLike (ConLike(..))
import FamInstEnv
import GHC
import HscTypes
import qualified InstEnv
import NameSet
import Outputable
import PatSyn
import PprTyThing
import Var (varType)

import Data.List (intersperse)
import Data.Maybe (catMaybes)

import HIE.Bios.Gap (getTyThing, fixInfo)

-- from ghc/InteractiveUI.hs

----------------------------------------------------------------

data GapThing = GtA Type
              | GtT TyCon
              | GtN
              | GtPatSyn PatSyn

fromTyThing :: TyThing -> GapThing
fromTyThing (AnId i)                   = GtA $ varType i
fromTyThing (AConLike (RealDataCon d)) = GtA $ dataConUserType d
fromTyThing (AConLike (PatSynCon p))   = GtPatSyn p
fromTyThing (ATyCon t)                 = GtT t
fromTyThing _                          = GtN

----------------------------------------------------------------

infoThing :: String -> Ghc SDoc
infoThing str = do
    names <- parseName str
    mb_stuffs <- mapM (getInfo False) names
    let filtered = filterOutChildren getTyThing $ catMaybes mb_stuffs
    return $ vcat (intersperse (text "") $ map (pprInfo . fixInfo) filtered)

filterOutChildren :: (a -> TyThing) -> [a] -> [a]
filterOutChildren get_thing xs
    = [x | x <- xs, not (getName (get_thing x) `elemNameSet` implicits)]
  where
    implicits = mkNameSet [getName t | x <- xs, t <- implicitTyThings (get_thing x)]

pprInfo :: (TyThing, GHC.Fixity, [InstEnv.ClsInst], [FamInst]) -> SDoc
pprInfo (thing, fixity, insts, famInsts)
    = pprTyThingInContextLoc thing
   $$ show_fixity fixity
   $$ InstEnv.pprInstances insts
   $$ pprFamInsts famInsts
  where
    show_fixity fx
      | fx == defaultFixity = Outputable.empty
      | otherwise           = ppr fx <+> ppr (getName thing)
