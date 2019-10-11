module Haskell.Ide.Engine.GhcUtils where

import qualified Language.Haskell.LSP.Core as Core

import qualified HscMain as G
import           Module
import           HscTypes
import qualified Data.Text as T

-- Convert progress continuation to a messager
toMessager :: (Core.Progress -> IO ()) -> G.Messager
toMessager k _hsc_env (nk, n) _rc_reason ms =
  let prog = Core.Progress (Just (fromIntegral nk/ fromIntegral n))  (Just mod_name)
      mod_name = T.pack $ moduleNameString (moduleName (ms_mod ms))
  in k prog

{-
toMessager :: Messager
toMessager hsc_env mod_index recomp mod_summary =
    case recomp of
        MustCompile -> showMsg "Compiling " ""
        UpToDate
            | verbosity (hsc_dflags hsc_env) >= 2 -> showMsg "Skipping  " ""
            | otherwise -> return ()
        RecompBecause reason -> showMsg "Compiling " (" [" ++ reason ++ "]")
    where
        dflags = hsc_dflags hsc_env
        showMsg msg reason =
            compilationProgressMsg dflags $
            (showModuleIndex mod_index ++
            msg ++ showModMsg dflags (hscTarget dflags)
                              (recompileRequired recomp) mod_summary)
                ++ reason
-}
