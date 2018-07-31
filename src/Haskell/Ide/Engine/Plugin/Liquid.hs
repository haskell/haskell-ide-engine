{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Plugin.Liquid where

import           Control.Monad.IO.Class
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map                      as Map
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           GHC.Generics
import           Haskell.Ide.Engine.MonadTypes hiding (_range)
import           System.FilePath


-- ---------------------------------------------------------------------

liquidDescriptor :: PluginDescriptor
liquidDescriptor = PluginDescriptor
  {
    pluginName = "Liquid Haskell"
  , pluginDesc = "Integration with Liquid Haskell"
  , pluginCommands =
      [ PluginCommand "sayHello" "say hello" sayHelloCmd
      , PluginCommand "sayHelloTo ""say hello to the passed in param" sayHelloToCmd
      ]
  , pluginCodeActionProvider = Nothing
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

data LiquidJson
  = LJ
    { status :: T.Text
    , types  :: Value
    , errors :: [LiquidError]
    } deriving (Show,Generic)

data LiquidPos
  = LP
    { line :: Int
    , column :: Int
    } deriving (Show,Generic,Eq)

data LiquidError =
  LE
    { start :: LiquidPos
    , stop  :: LiquidPos
    , message :: T.Text
    } deriving (Show,Generic,Eq)


instance FromJSON LiquidJson
instance ToJSON   LiquidJson
instance FromJSON LiquidPos
instance ToJSON   LiquidPos
instance FromJSON LiquidError
instance ToJSON   LiquidError

-- ---------------------------------------------------------------------

diagnosticProvider :: DiagnosticTrigger -> Uri -> IdeGhcM (IdeResult (Map.Map Uri (S.Set Diagnostic)))
diagnosticProvider _trigger uri = do
  me <- liftIO $ readJsonAnnot uri
  case me of
    Nothing -> return $ IdeResultOk Map.empty
    Just es -> return $ IdeResultOk m
      where
        m = Map.fromList [(uri,S.fromList (map liquidErrorToDiagnostic es))]
  -- let diag = Diagnostic
  --             { _range = Range (Position 5 0) (Position 7 0)
  --             , _severity = Nothing
  --             , _code = Nothing
  --             , _source = Just "eg2"
  --             , _message = "Liquid plugin diagnostic, vim annot in " <> T.pack (vimAnnotFile uri)
  --             , _relatedInformation = Nothing
  --             }
  -- return $ IdeResultOk $ Map.fromList [(uri,S.singleton diag)]

-- ---------------------------------------------------------------------

liquidErrorToDiagnostic :: LiquidError -> Diagnostic
liquidErrorToDiagnostic (LE f t msg) =
  Diagnostic
    { _range = Range (lpToPos f) (lpToPos t)
    , _severity = Just DsError
    , _code = Nothing
    , _source = Just "liquid"
    , _message = msg
    , _relatedInformation = Nothing
    }
  where
    lpToPos (LP r c) = Position (r - 1) (c - 1)

-- ---------------------------------------------------------------------

-- | Pull the errors out of the JSON annotation file, if it exists
readJsonAnnot :: Uri -> IO (Maybe [LiquidError])
readJsonAnnot uri = do
  jf <- BS.readFile (jsonAnnotFile uri)
  case decode jf :: Maybe LiquidJson of
    Nothing -> return Nothing
    Just j -> return (Just (errors j))

-- ---------------------------------------------------------------------

-- | For a Uri representing
-- "path/to/file/Foo.hs" return
-- "path/to/file/.liquid/Foo.hs.json"
jsonAnnotFile :: Uri -> FilePath
jsonAnnotFile uri = liquidFileFor uri "json"

-- | For a Uri representing
-- "path/to/file/Foo.hs" return
-- "path/to/file/.liquid/Foo.hs.vim.annot"
vimAnnotFile :: Uri -> FilePath
vimAnnotFile uri = liquidFileFor uri "vim.annot"

-- | For a Uri representing
-- "path/to/file/Foo.hs" return
-- "path/to/file/.liquid/Foo.hs.EXT"
liquidFileFor :: Uri -> String -> FilePath
liquidFileFor uri ext =
  case uriToFilePath uri of
    Nothing -> error $ " Liquid.vimAnnotFile:bad uri:" ++ show uri
    Just fp -> r
      where
        d = takeDirectory fp
        f = takeFileName fp
        r = d </> ".liquid" </> f <.> ext

-- ---------------------------------------------------------------------
