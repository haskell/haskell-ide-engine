module HIE.Bios.Lang where

import DynFlags (supportedLanguagesAndExtensions)

import HIE.Bios.Types

-- | Listing language extensions.

listLanguages :: Options -> IO String
listLanguages opt = return $ convert opt supportedLanguagesAndExtensions
