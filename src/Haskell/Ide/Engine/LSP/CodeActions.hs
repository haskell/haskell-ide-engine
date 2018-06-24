{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Haskell.Ide.Engine.LSP.CodeActions where
  
import Control.Lens
import qualified Data.Aeson as J
import qualified Data.Bimap as BM
import qualified Data.Text as T
--TODO: Can we get rid of this?
import qualified Data.Vector as V
import Data.Maybe
import Data.Foldable
import Haskell.Ide.Engine.LSP.Reactor
import qualified Haskell.Ide.Engine.Plugin.ApplyRefact as ApplyRefact
import qualified Haskell.Ide.Engine.Plugin.Hoogle as Hoogle
import qualified Haskell.Ide.Engine.Plugin.HsImport as HsImport
import Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Core as Core
import qualified Language.Haskell.LSP.Types as J
import Language.Haskell.LSP.Messages

handleCodeActionReq :: TrackingNumber -> BM.Bimap T.Text T.Text -> (PluginRequest R -> R ()) -> J.CodeActionRequest -> R ()
handleCodeActionReq tn commandMap makeRequest req = do

  let params = req ^. J.params
      doc = params ^. J.textDocument . J.uri
      (J.List diags) = params ^. J.context . J.diagnostics

  let
    mkHlintCmd (J.Diagnostic (J.Range start _) _s (Just code) (Just "hlint") m _) = [J.Command title cmd cmdparams]
      where
        title :: T.Text
        title = "Apply hint:" <> head (T.lines m)
        -- NOTE: the cmd needs to be registered via the InitializeResponse message. See hieOptions above
        cmd = commandMap BM.! "applyrefact:applyOne"
        -- need 'file', 'start_pos' and hint title (to distinguish between alternative suggestions at the same location)
        args = J.Array $ V.singleton $ J.toJSON $ ApplyRefact.AOP doc start code
        cmdparams = Just args

    mkHlintCmd (J.Diagnostic _r _s _c _source _m _) = []

    hlintActions = concatMap mkHlintCmd $ filter validCommand diags
      where
            -- |Some hints do not have an associated refactoring
        validCommand (J.Diagnostic _ _ (Just code) (Just "hlint") _ _) =
          case code of
            "Eta reduce" -> False
            _            -> True
        validCommand _ = False

    missingVars = mapMaybe isMissingDiag diags
      where
        isMissingDiag :: J.Diagnostic -> Maybe T.Text
        isMissingDiag (J.Diagnostic _ _ _ (Just "ghcmod") msg _) = asum
          [T.stripPrefix "Variable not in scope: " msg,
            fmap T.init (T.stripPrefix "Not in scope: type constructor or class ‘" msg)]
        isMissingDiag _ = Nothing

    mkImportCmd modName = J.Command title cmd (Just cmdParams)
      where
        title = "Import module " <> modName
        cmd = commandMap BM.! "hsimport:import"
        cmdParams = J.toJSON [HsImport.ImportParams doc modName]

    searchModules term callback = do
      let searchReq = IReq tn (req ^. J.id)
                              (callback . take 5)
                              (Hoogle.searchModules term)
      makeRequest searchReq

    send codeActions =
      reactorSend $ RspCodeAction $ Core.makeResponseMessage req (J.List codeActions)
    in
      -- If we have some possible import code actions
      if not (null missingVars) then do
        let -- Will look like myFunc :: Int -> String
            variable = head missingVars
            makeCmds = map mkImportCmd
        searchModules variable $ \case
          -- Try with just the name and no type
          -- e.g. myFunc
          [] -> searchModules (head (T.words variable)) (send . (++ hlintActions) . makeCmds)
          -- We got some module sos use these
          xs -> send $ makeCmds xs ++ hlintActions
      else
        send hlintActions
-- TODO: make context specific commands for all sorts of things, such as refactorings          
