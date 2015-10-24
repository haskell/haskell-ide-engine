{-# LANGUAGE CPP #-}

module Haskell.Ide.Plugin where

import Haskell.Ide.Types

import DynamicLoading
import FastString
import HscTypes
import Module
import Name (Name)
import OccName ( mkVarOcc, mkTcOcc )
import Outputable
import Panic
import PrelNames ( tcQual_RDR )
import RdrName ( RdrName, mkRdrQual )
import TypeRep ( mkTyConTy )
import Unique ( mkPreludeTyConUnique )

-- Copy-modified from DynamicLoading
loadPlugin :: HscEnv -> ModuleName -> IO Plugin
loadPlugin hsc_env mod_name
  = do { let plugin_rdr_name = mkRdrQual mod_name (mkVarOcc "plugin")
             dflags = hsc_dflags hsc_env
       ; mb_name <- lookupRdrNameInModuleForPlugins hsc_env mod_name
             plugin_rdr_name
       ; case mb_name of {
            Nothing ->
                throwGhcExceptionIO (CmdLineError $ showSDoc dflags $ hsep
                          [ ptext (sLit "The module"), ppr mod_name
                          , ptext (sLit "did not export the plugin name")
                          , ppr plugin_rdr_name ]) ;
            Just name ->

     do { let types_mod = mkModuleNameFS (fsLit "Haskell.Ide.Types")
              plugin_tyname_rdr = mkRdrQual types_mod (mkTcOcc "Plugin")
        ; mb_plugin_tyname <- lookupRdrNameInModuleForPlugins hsc_env types_mod
              plugin_tyname_rdr
        ; case mb_plugin_tyname of {
            Nothing ->
                throwGhcExceptionIO (CmdLineError $ showSDoc dflags $ hsep
                          [ ptext (sLit "The module"), ppr types_mod
                          , ptext (sLit "did not export the plugin type")
                          , ppr plugin_tyname_rdr ]) ;
            Just plugin_tyname ->

    -- FIXME: Check that the resolved module matches CURRENT_PACKAGE_KEY
    -- (from cabal_macros.h)

     do { plugin_tycon <- forceLoadTyCon hsc_env plugin_tyname
        ; mb_plugin <- getValueSafely hsc_env name (mkTyConTy plugin_tycon)
        ; case mb_plugin of
            Nothing ->
                throwGhcExceptionIO (CmdLineError $ showSDoc dflags $ hsep
                          [ ptext (sLit "The value")
                          , ppr name
                          , ptext (sLit " in ")
                          , ppr mod_name
                          , ptext (sLit " did not have the type")
                          , ppr plugin_tyname, ptext (sLit "as required")])
            Just plugin -> return plugin } } } } }
