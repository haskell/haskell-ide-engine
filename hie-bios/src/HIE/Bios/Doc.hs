module HIE.Bios.Doc where

import GHC (DynFlags, getPrintUnqual, pprCols, GhcMonad)
import Outputable (PprStyle, SDoc, withPprStyleDoc, neverQualify)
import Pretty (Mode(..), Doc, Style(..), renderStyle, style)

import HIE.Bios.Gap (makeUserStyle)

showPage :: DynFlags -> PprStyle -> SDoc -> String
showPage dflag stl = showDocWith dflag PageMode . withPprStyleDoc dflag stl

showOneLine :: DynFlags -> PprStyle -> SDoc -> String
showOneLine dflag stl = showDocWith dflag OneLineMode . withPprStyleDoc dflag stl

getStyle :: (GhcMonad m) => DynFlags -> m PprStyle
getStyle dflags = makeUserStyle dflags <$> getPrintUnqual

styleUnqualified :: DynFlags -> PprStyle
styleUnqualified dflags = makeUserStyle dflags neverQualify

showDocWith :: DynFlags -> Mode -> Doc -> String
showDocWith dflags md = renderStyle mstyle
  where
    mstyle = style { mode = md, lineLength = pprCols dflags }
