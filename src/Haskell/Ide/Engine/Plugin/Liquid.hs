{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Haskell.Ide.Engine.Plugin.Liquid where

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Exception (bracket)
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import           Data.Aeson
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Map                      as Map
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           GHC.Generics
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes hiding (_range)
import           Haskell.Ide.Engine.PluginUtils
import qualified Language.Haskell.LSP.Types    as J
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Process
import           Text.Parsec
import           Text.Parsec.Text

-- ---------------------------------------------------------------------

liquidDescriptor :: PluginId -> PluginDescriptor
liquidDescriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginName = "Liquid Haskell"
  , pluginDesc = "Integration with Liquid Haskell"
  , pluginCommands =
      [ PluginCommand "sayHello"   "say hello"                        sayHelloCmd
      , PluginCommand "sayHelloTo" "say hello to the passed in param" sayHelloToCmd
      ]
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Just (DiagnosticProvider
                                    (S.singleton DiagnosticOnSave)
                                    (DiagnosticProviderAsync diagnosticProvider))
  , pluginHoverProvider      = Just hoverProvider
  , pluginSymbolProvider     = Nothing
  , pluginFormattingProvider = Nothing
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

newtype LiquidData =
  LiquidData
    { tid :: Maybe (Async ())
    }

instance ExtensionClass LiquidData where
  initialValue = LiquidData Nothing

-- ---------------------------------------------------------------------

-- diagnosticProvider :: DiagnosticTrigger -> Uri -> IdeM (IdeResult (Map.Map Uri (S.Set Diagnostic)))

diagnosticProvider :: DiagnosticProviderFuncAsync
diagnosticProvider DiagnosticOnSave uri cb = pluginGetFile "Liquid.diagnosticProvider:" uri $ \file ->
  withCachedModuleAndData file (IdeResultOk ()) $ \_tm _info () -> do
    -- New save, kill any prior instance that was running
    LiquidData mtid <- get
    mapM_ (liftIO . cancel) mtid

    tid <- liftIO $ async $ generateDiagnosics cb uri file
    put (LiquidData (Just tid))

    return $ IdeResultOk ()
diagnosticProvider _ _ _ = return (IdeResultOk ())

-- ---------------------------------------------------------------------

generateDiagnosics :: (Map.Map Uri (S.Set Diagnostic) -> IO ()) -> Uri -> FilePath -> IO ()
generateDiagnosics cb uri file = do
  r <- runLiquidHaskell file
  case r of
    Nothing -> do
      -- TODO: return an LSP warning
      logm "Liquid.generateDiagnostics:no liquid exe found"
      return ()
    Just (_ec,_) -> do
      me <- liftIO $ readJsonAnnot uri
      case me of
        Nothing -> do
          logm "Liquid.generateDiagnostics:no liquid results parsed"
          return ()
        Just es -> do
          logm "Liquid.generateDiagnostics:liquid results parsed"
          cb m
          where
            m = Map.fromList [(uri,S.fromList (map liquidErrorToDiagnostic es))]
      return ()

-- ---------------------------------------------------------------------

-- Find and run the liquid haskell executable
runLiquidHaskell :: FilePath -> IO (Maybe (ExitCode,[String]))
runLiquidHaskell fp = do
  mexe <- findExecutable "liquid"
  case mexe of
    Nothing -> return Nothing
    Just lh -> do
      -- Putting quotes around the fp to help windows users with
      -- spaces in paths
      let cmd = lh ++ " --json \"" ++ fp ++ "\""
          dir = takeDirectory fp
          cp = (shell cmd) { cwd = Just dir }
      logm $ "runLiquidHaskell:cmd=[" ++ cmd ++ "]"
      mpp <- lookupEnv "GHC_PACKAGE_PATH"
      (ec,o,e) <- bracket
        (unsetEnv "GHC_PACKAGE_PATH")
        (\_ -> mapM_ (setEnv "GHC_PACKAGE_PATH") mpp)
        (\_ -> readCreateProcessWithExitCode cp "")
      logm $ "runLiquidHaskell:v=" ++ show (ec,o,e)
      return $ Just (ec,[o,e])

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
      \_ info () -> do
        merrs <- liftIO $ readVimAnnot uri
        case merrs of
          Nothing -> return (IdeResultOk [])
          Just lerrs -> do
            let perrs = map (\le@(LE s e _) -> (lpToPos s,lpToPos e,le)) lerrs
                ls    = getThingsAtPos info pos perrs
            hs <- forM ls $ \(r,LE _s _e msg) -> do
              let msgs = T.splitOn "\\n" msg
                  msgm = J.markedUpContent "haskell" (T.unlines msgs)
              return $ J.Hover (J.HoverContents msgm) (Just r)
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
parseTypes = parseTypeFromVim `sepBy` string "\n"

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
