{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Haskell.Ide.Engine.Plugin.Ormolu
  ( ormoluDescriptor
  )
where

import           Haskell.Ide.Engine.MonadTypes

#if __GLASGOW_HASKELL__ >= 806
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO
                                                , MonadIO(..)
                                                )
import           Data.Aeson                     ( Value(Null) )
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Text                     as T
import           GHC
import           Ormolu
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Support.HieExtras
import           HIE.Bios.Types
import qualified DynFlags                      as D
import qualified EnumSet                       as S
#endif

ormoluDescriptor :: PluginId -> PluginDescriptor
ormoluDescriptor plId = PluginDescriptor
  { pluginId                 = plId
  , pluginName               = "Ormolu"
  , pluginDesc               = "A formatter for Haskell source code."
  , pluginCommands           = []
  , pluginCodeActionProvider = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider      = Nothing
  , pluginSymbolProvider     = Nothing
  , pluginFormattingProvider = Just provider
  }


provider :: FormattingProvider
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE BlockArguments #-}
provider contents uri typ _ = pluginGetFile contents uri $ \fp -> do
  opts <- lookupComponentOptions fp
  let cradleOpts =
        map DynOption
          $   filter exop
          $   join
          $   maybeToList
          $   componentOptions
          <$> opts

      fromDyn tcm _ () =
        let
          df = getDynFlags tcm
          pp =
            let p = D.sPgm_F $ D.settings df
            in  if null p then [] else ["-pgmF=" <> p]
          pm = map (("-fplugin=" <>) . moduleNameString) $ D.pluginModNames df
          ex = map (("-X" <>) . show) $ S.toList $ D.extensionFlags df
        in
          return $ map DynOption $ pp <> pm <> ex
  fileOpts <- ifCachedModuleAndData fp cradleOpts fromDyn
  let
    conf o = Config o False False True False
    fmt :: T.Text -> [DynOption] -> IdeM (Either OrmoluException T.Text)
    fmt cont o =
      liftIO $ try @OrmoluException (ormolu (conf o) fp $ T.unpack cont)

  case typ of
    FormatText -> ret (fullRange contents) <$> fmt contents cradleOpts
    FormatRange r ->
      let
        txt = T.lines $ extractRange r contents
        lineRange (Range (Position sl _) (Position el _)) =
          Range (Position sl 0) $ Position el $ T.length $ last txt
        fixLine t = if T.all isSpace $ last txt then t else T.init t
        unStrip ws new =
          fixLine $ T.unlines $ map (ws `T.append`) $ T.lines new
        mStrip = case txt of
          (l : _) ->
            let ws = fst $ T.span isSpace l
            in  (,) ws . T.unlines <$> traverse (T.stripPrefix ws) txt
          _ -> Nothing
        err = return $ IdeResultFail
          (IdeError
            PluginError
            (T.pack
              "You must format a whole block of code. Ormolu does not support arbitrary ranges."
            )
            Null
          )
        fmt' (ws, striped) =
          ret (lineRange r) <$> (fmap (unStrip ws) <$> fmt striped fileOpts)
      in
        maybe err fmt' mStrip
 where
  ret _ (Left err) = IdeResultFail
    (IdeError PluginError (T.pack $ "ormoluCmd: " ++ show err) Null)
  ret r (Right new) = IdeResultOk [TextEdit r new]

  exop s =
    "-X" `isPrefixOf` s || "-fplugin=" `isPrefixOf` s || "-pgmF=" `isPrefixOf` s
#else
provider _ _ _ _ = return $ IdeResultOk [] -- NOP formatter
#endif
