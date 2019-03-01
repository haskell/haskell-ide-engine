{-# LANGUAGE BangPatterns #-}

module HIE.Bios.Logger (
    withLogger
  , checkErrorPrefix
  , getSrcSpan
  ) where

import Bag (Bag, bagToList)
import CoreMonad (liftIO)
import DynFlags (LogAction, dopt, DumpFlag(Opt_D_dump_splices))
import ErrUtils
import Exception (ghandle)
import FastString (unpackFS)
import GHC (DynFlags(..), SrcSpan(..), Severity(SevError), GhcMonad)
import qualified GHC as G
import HscTypes (SourceError, srcErrorMessages)
import Outputable (PprStyle, SDoc)

import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import System.FilePath (normalise)

import HIE.Bios.Doc (showPage, getStyle)
import HIE.Bios.GHCApi (withDynFlags, withCmdFlags)
import HIE.Bios.Types (Options(..), convert)

----------------------------------------------------------------

type Builder = [String] -> [String]

newtype LogRef = LogRef (IORef Builder)

newLogRef :: IO LogRef
newLogRef = LogRef <$> newIORef id

readAndClearLogRef :: Options -> LogRef -> IO String
readAndClearLogRef opt (LogRef ref) = do
    b <- readIORef ref
    writeIORef ref id
    return $! convert opt (b [])

appendLogRef :: DynFlags -> LogRef -> LogAction
appendLogRef df (LogRef ref) _ _ sev src style msg = do
        let !l = ppMsg src sev df style msg
        modifyIORef ref (\b -> b . (l:))

----------------------------------------------------------------

-- | Set the session flag (e.g. "-Wall" or "-w:") then
--   executes a body. Log messages are returned as 'String'.
--   Right is success and Left is failure.
withLogger ::
  (GhcMonad m)
  => Options -> (DynFlags -> DynFlags) -> m () -> m (Either String String)
withLogger opt setDF body = ghandle (sourceError opt) $ do
    logref <- liftIO newLogRef
    withDynFlags (setLogger logref . setDF) $ do
        withCmdFlags wflags $ do
            body
            liftIO $ Right <$> readAndClearLogRef opt logref
  where
    setLogger logref df = df { log_action =  appendLogRef df logref }
    wflags = filter ("-fno-warn" `isPrefixOf`) $ ghcOpts opt

----------------------------------------------------------------

-- | Converting 'SourceError' to 'String'.
sourceError ::
  (GhcMonad m)
  => Options -> SourceError -> m (Either String String)
sourceError opt err = do
    dflag <- G.getSessionDynFlags
    style <- getStyle dflag
    let ret = convert opt . errBagToStrList dflag style . srcErrorMessages $ err
    return (Left ret)

errBagToStrList :: DynFlags -> PprStyle -> Bag ErrMsg -> [String]
errBagToStrList dflag style = map (ppErrMsg dflag style) . reverse . bagToList

----------------------------------------------------------------

ppErrMsg :: DynFlags -> PprStyle -> ErrMsg -> String
ppErrMsg dflag style err = ppMsg spn SevError dflag style msg -- ++ ext
   where
     spn = errMsgSpan err
     msg = pprLocErrMsg err
     -- fixme
--     ext = showPage dflag style (pprLocErrMsg $ errMsgReason err)

ppMsg :: SrcSpan -> Severity-> DynFlags -> PprStyle -> SDoc -> String
ppMsg spn sev dflag style msg = prefix ++ cts
  where
    cts  = showPage dflag style msg
    defaultPrefix
      | isDumpSplices dflag = ""
      | otherwise           = checkErrorPrefix
    prefix = fromMaybe defaultPrefix $ do
        (line,col,_,_) <- getSrcSpan spn
        file <- normalise <$> getSrcFile spn
        let severityCaption = showSeverityCaption sev
        return $ file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++ severityCaption

checkErrorPrefix :: String
checkErrorPrefix = "Dummy:0:0:Error:"

showSeverityCaption :: Severity -> String
showSeverityCaption SevWarning = "Warning: "
showSeverityCaption _          = ""

getSrcFile :: SrcSpan -> Maybe String
getSrcFile (G.RealSrcSpan spn) = Just . unpackFS . G.srcSpanFile $ spn
getSrcFile _                   = Nothing

isDumpSplices :: DynFlags -> Bool
isDumpSplices dflag = dopt Opt_D_dump_splices dflag

getSrcSpan :: SrcSpan -> Maybe (Int,Int,Int,Int)
getSrcSpan (RealSrcSpan spn) = Just ( G.srcSpanStartLine spn
                                    , G.srcSpanStartCol spn
                                    , G.srcSpanEndLine spn
                                    , G.srcSpanEndCol spn)
getSrcSpan _ = Nothing
