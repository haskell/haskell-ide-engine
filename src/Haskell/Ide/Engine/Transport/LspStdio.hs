{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Ide.Engine.Transport.LspStdio
  (
    lspStdioTransport
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Haskell.Ide.Engine.Transport.Pipes
import           Haskell.Ide.Engine.Types
import qualified Pipes as P
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import           System.IO
import qualified Control.Exception as E
import           Data.Default
import           Data.Either.Utils
import           System.Exit
import qualified System.Log.Logger as L

import qualified Language.Haskell.LSP.Control  as CTRL
import qualified Language.Haskell.LSP.Core     as GUI
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J

-- ---------------------------------------------------------------------

lspStdioTransport :: TChan ChannelRequest -> IO ()
lspStdioTransport cin = do
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c


-- ---------------------------------------------------------------------

run :: IO Int
run = flip E.catches handlers $ do

  flip E.finally finalProc $ do
    CTRL.run hieHandlers hieOptions

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = L.removeAllHandlers
    ioExcept   (e :: E.IOException)       = print e >> return 1
    someExcept (e :: E.SomeException)     = print e >> return 1

-- ---------------------------------------------------------------------

hieOptions :: GUI.Options
hieOptions = def { GUI.codeLensProvider = Just def}

hieHandlers :: GUI.Handlers
hieHandlers = def { GUI.renameHandler = Just renameRequestHandler
                  , GUI.codeLensHandler = Just codeLensHandler
                  }

-- ---------------------------------------------------------------------

renameRequestHandler :: J.RenameRequest -> IO J.RenameResponse
renameRequestHandler (J.RequestMessage _ origId _ _) = do
  let loc = def :: J.Location
      res  = GUI.makeResponseMessage origId loc
  return res

-- ---------------------------------------------------------------------

codeLensHandler :: J.CodeLensRequest -> IO J.CodeLensResponse
codeLensHandler (J.RequestMessage _ origId _ _) = do
  let
    lens = J.CodeLens (J.Range (J.Position 1 1) (J.Position 1 10)) (Just (J.Command "codeLensCmd" "actionCmd" Nothing)) Nothing
    lenses = [lens]
    res = GUI.makeResponseMessage origId lenses
  return res
