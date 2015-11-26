module Ide.SublimeText where

import PluginList (plugins)
import qualified Data.Map as M

-- | Generate sublime text plugin in $REPOSITORY_ROOT/plugin-sublime folder
generate :: IO ()
generate = do
  let func = M.assocs plugins
  -- TODO
  writeFile "plugin-sublime/haskell-ide.py" $ unlines
  putStrLn "done"
  return ()

tempCode = unlines
  [ "import sublime, sublimeplugin  "
  , "  "
  , "# Extends TextCommand so that run() receives a View to modify.  "
  , "class DuplicateCommand(sublimeplugin.TextCommand):  "
  , "    def run(self, view, args):  "
  , "        # Walk through each region in the selection  "
  , "        for region in view.sel():  "
  , "            # Only interested in empty regions, otherwise they may span multiple  "
  , "            # lines, which doesn't make sense for this command.  "
  , "            if region.empty():  "
  , "                # Expand the region to the full line it resides on, excluding the newline  "
  , "                line = view.line(region)  "
  , "                # Extract the string for the line, and add a newline  "
  , "                lineContents = view.substr(line) + '\n'  "
  , "                # Add the text at the beginning of the line  "
  , "                view.insert(line.begin(), lineContents)  "
  ]
