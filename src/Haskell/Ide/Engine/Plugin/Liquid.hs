{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns      #-}
module Haskell.Ide.Engine.Plugin.Liquid where

import           Control.Monad
import           Control.Monad.IO.Class
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map                      as Map
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           GHC.Generics
import           Haskell.Ide.Engine.MonadTypes hiding (_range)
import           Haskell.Ide.Engine.PluginUtils
import qualified Language.Haskell.LSP.Types as J
import           System.Directory
import           System.FilePath
import           Text.Parsec
import           Text.Parsec.Text

-- ---------------------------------------------------------------------

liquidDescriptor :: PluginId -> PluginDescriptor
liquidDescriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginName = "Liquid Haskell"
  , pluginDesc = "Integration with Liquid Haskell"
  , pluginCommands =
      [ PluginCommand "sayHello" "say hello" sayHelloCmd
      , PluginCommand "sayHelloTo ""say hello to the passed in param" sayHelloToCmd
      ]
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Just (DiagnosticProvider (S.singleton DiagnosticOnSave) diagnosticProvider)
  , pluginHoverProvider      = Just hoverProvider
  , pluginSymbolProvider = Nothing
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

lpToPos :: LiquidPos -> Position
lpToPos (LP r c) = Position (r - 1) (c - 1)

-- ---------------------------------------------------------------------

-- | Pull the errors out of the JSON annotation file, if it exists
readJsonAnnot :: Uri -> IO (Maybe [LiquidError])
readJsonAnnot uri = do
  let fileName = jsonAnnotFile uri
  exists <- doesFileExist fileName
  if exists
    then do
      jf <- BS.readFile fileName
      case decode jf :: Maybe LiquidJson of
        Nothing -> return Nothing
        Just j -> return (Just (errors j))
    else return Nothing

-- | Pull the errors out of the JSON annotation file, if it exists
readVimAnnot :: Uri -> IO (Maybe [LiquidError])
readVimAnnot uri = do
  let fileName = vimAnnotFile uri
  exists <- doesFileExist fileName
  if exists
    then do
      vf <- T.readFile fileName
      return $ Just (parseType vf)
    else return Nothing

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

-- type HoverProvider = Uri -> Position -> IdeM (IdeResponse Hover)

hoverProvider :: HoverProvider
hoverProvider uri pos =
  pluginGetFile "Liquid.hoverProvider: " uri $ \file ->
    ifCachedModuleAndData file (IdeResultOk []) $
      \cm () -> do
        merrs <- liftIO $ readVimAnnot uri
        case merrs of
          Nothing -> return (IdeResultOk [])
          Just lerrs -> do
            let perrs = map (\le@(LE s e _) -> (lpToPos s,lpToPos e,le)) lerrs
                ls    = getThingsAtPos cm pos perrs
            hs <- forM ls $ \(r,LE _s _e msg) -> do
              let msgs = T.splitOn "\\n" msg
                  msg' = J.CodeString (J.LanguageString "haskell" (T.unlines msgs))
              return $ J.Hover (J.List [msg']) (Just r)
            return (IdeResultOk hs)

-- ---------------------------------------------------------------------

parseType :: T.Text -> [LiquidError]
parseType str =
  case parse parseTypes "" str of
            -- Left _    -> []
            Left err -> error $ show err
            Right les  -> les

-- ---------------------------------------------------------------------

parseTypes :: Parser [LiquidError]
parseTypes = parseTypeFromVim `sepBy` (string "\n")

-- | Parse a line of the form
-- 6:1-6:10::Main.weAreEven :: "[{v : GHC.Types.Int | v mod 2 == 0}]"
parseTypeFromVim :: Parser LiquidError
parseTypeFromVim = do
  sr <- number
  _ <- char ':'
  sc <- number
  _ <- char '-'
  er <- number
  _ <- char ':'
  ec <- number
  _ <- string "::"
  _ <- manyTill anyChar (try (string "::"))
  _ <- string " \""
  msg <- manyTill anyChar (try (string "\""))
  return $ LE (LP sr sc) (LP er ec) (T.pack msg)

number :: Parser Int
number = do
  s <- many1 digit
  return (read s)

-- ---------------------------------------------------------------------
