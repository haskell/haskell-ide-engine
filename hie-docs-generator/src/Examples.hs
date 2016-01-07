{-# LANGUAGE OverloadedStrings #-}
module Examples where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Text as T
import           Haskell.Ide.Engine.BasePlugin
import           Haskell.Ide.HaRePlugin
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Transport.JsonStdio
import           Options

jsonStdioExample :: PluginId -> UntaggedCommandDescriptor -> WireRequest
jsonStdioExample pluginId (CommandDesc{cmdName = name,cmdContexts = contexts,cmdAdditionalParams = cmdParams}) =
  WireReq wireCmd (M.union contextUnion additionalParams)
  where wireCmd = pluginId <> ":" <> name
        contextUnion = mconcat $ map contextParams contexts
        additionalParams = mconcat $ map paramDesc $ cmdParams

contextParams :: AcceptedContext -> ParamMap
contextParams ctx = mconcat . map paramDesc . contextMapping $ ctx

paramDesc :: ParamDescription -> ParamMap
paramDesc desc =
  M.singleton (pName desc) $
  case pType desc of
    PtText -> ParamTextP (pHelp desc)
    PtFile -> ParamFileP (pHelp desc)
    PtPos -> ParamPosP (42,23)

-- test :: BSL.ByteString
-- test =
--   A.encode $
--   jsonStdioExample "base"
--                    (cmdDesc $ pdCommands baseDescriptor !! 2)

-- test2 :: BSL.ByteString
-- test2 = AP.encodePretty $ jsonStdioExample "hare" (cmdDesc $ pdCommands hareDescriptor !! 5)
