{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Plugin.Example2 where

import           Control.Monad.IO.Class
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Map                      as Map
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           Haskell.Ide.Engine.MonadTypes hiding (_range)

-- ---------------------------------------------------------------------

example2Descriptor :: PluginDescriptor
example2Descriptor = PluginDescriptor
  {
    pluginName = "Hello World"
  , pluginDesc = "An example of writing an HIE plugin"
  , pluginCommands =
      [ PluginCommand "sayHello" "say hello" sayHelloCmd
      , PluginCommand "sayHelloTo ""say hello to the passed in param" sayHelloToCmd
      ]
  , pluginCodeActionProvider = noCodeActions
  , pluginDiagnosticProvider = Just (DiagnosticProvider (S.singleton DiagnosticOnSave) diagnosticProvider)
  }

-- ---------------------------------------------------------------------

sayHelloCmd :: CommandFunc () T.Text
sayHelloCmd = CmdSync $ \_ -> return (IdeResultOk sayHello)

sayHelloToCmd :: CommandFunc T.Text T.Text
sayHelloToCmd = CmdSync $ \n -> do
  r <- liftIO $ sayHelloTo n
  return $ IdeResultOk r

-- ---------------------------------------------------------------------

sayHello :: T.Text
sayHello = "hello from ExamplePlugin2"

sayHelloTo :: T.Text -> IO T.Text
sayHelloTo n = return $ "hello " <> n <> " from ExamplePlugin2"

-- ---------------------------------------------------------------------

diagnosticProvider :: Uri -> IdeGhcM (IdeResult (Map.Map Uri (S.Set Diagnostic)))
diagnosticProvider uri = do
  let diag = Diagnostic
              { _range = Range (Position 0 0) (Position 1 0)
              , _severity = Nothing
              , _code = Nothing
              , _source = Just "eg2"
              , _message = "Example plugin diagnostic"
              , _relatedInformation = Nothing
              }
  return $ IdeResultOk $ Map.fromList [(uri,S.singleton diag)]
