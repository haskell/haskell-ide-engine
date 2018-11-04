{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Plugin.Example2 where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.HashMap.Strict           as H
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Map                      as Map
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified GHC.Generics                  as Generics
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes hiding (_range)
import qualified Language.Haskell.LSP.Types      as J
import qualified Language.Haskell.LSP.Types.Lens as J

-- ---------------------------------------------------------------------

example2Descriptor :: PluginId -> PluginDescriptor
example2Descriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginName = "Hello World"
  , pluginDesc = "An example of writing an HIE plugin"
  , pluginCommands =
      [ PluginCommand "sayHello" "say hello" sayHelloCmd
      , PluginCommand "sayHelloTo ""say hello to the passed in param" sayHelloToCmd
      , PluginCommand "todo" "Add a TODO marker" todoCmd
      ]
  , pluginCodeActionProvider = Just codeActionProvider
  , pluginDiagnosticProvider
      = Just (DiagnosticProvider (S.singleton DiagnosticOnSave) (DiagnosticProviderSync diagnosticProvider))
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  }

-- ---------------------------------------------------------------------

sayHelloCmd :: CommandFunc () T.Text
sayHelloCmd = CmdSync $ \_ _ -> return (IdeResultOk sayHello)

sayHelloToCmd :: CommandFunc T.Text T.Text
sayHelloToCmd = CmdSync $ \_ n -> do
  r <- liftIO $ sayHelloTo n
  return $ IdeResultOk r

-- ---------------------------------------------------------------------

sayHello :: T.Text
sayHello = "hello from ExamplePlugin2"

sayHelloTo :: T.Text -> IO T.Text
sayHelloTo n = return $ "hello " <> n <> " from ExamplePlugin2"

-- ---------------------------------------------------------------------

diagnosticProvider :: DiagnosticProviderFuncSync
diagnosticProvider trigger uri = do
  liftIO $ logm "Example2.diagnosticProvider called"
  let diag = Diagnostic
              { _range = Range (Position 0 0) (Position 1 0)
              , _severity = Nothing
              , _code = Nothing
              , _source = Just "eg2"
              , _message = "Example plugin diagnostic, triggered by" <> T.pack (show trigger)
              , _relatedInformation = Nothing
              }
  return $ IdeResultOk $ Map.fromList [(uri,S.singleton diag)]

-- ---------------------------------------------------------------------

data TodoParams = TodoParams
  { file  :: Uri
  , range :: J.Range
  }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

todoCmd :: CommandFunc TodoParams J.WorkspaceEdit
todoCmd = CmdSync $ \_ (TodoParams uri r) -> return $ IdeResultOk $ makeTodo uri r

makeTodo :: J.Uri -> J.Range -> J.WorkspaceEdit
makeTodo uri (J.Range (J.Position startLine _) _) = res
  where
    pos = (J.Position startLine 0)
    textEdits = J.List
      [J.TextEdit (J.Range pos pos)
                  "-- TODO: from example2 plugin\n"
      ]
    res = J.WorkspaceEdit
      (Just $ H.singleton uri textEdits)
      Nothing

-- ---------------------------------------------------------------------


codeActionProvider :: CodeActionProvider
codeActionProvider plId docId _ _ r _context = do
  cmd <- mkLspCommand plId "todo" title  (Just cmdParams)
  return $ IdeResultOk [codeAction cmd]
  where
    codeAction :: J.Command -> J.CodeAction
    codeAction cmd = J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [])) Nothing (Just cmd)
    title = "Add TODO marker"
    cmdParams = [toJSON (TodoParams (docId ^. J.uri) r )]
