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
import           Ormolu
import           Haskell.Ide.Engine.PluginUtils
import           HIE.Bios.Types
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
  let opts' =
        map DynOption
          $   filter exop
          $   join
          $   maybeToList
          $   componentOptions
          <$> opts
      conf = Config opts' False False True False
      fmt :: T.Text -> IdeM (Either OrmoluException T.Text)
      fmt cont = liftIO $ try @OrmoluException (ormolu conf fp $ T.unpack cont)

  case typ of
    FormatText -> ret (fullRange contents) <$> fmt contents
    FormatRange r ->
      let
        txt = T.lines $ extractRange r contents
        lineRange (Range (Position sl _) (Position el _)) =
          Range (Position sl 0) $ Position el $ T.length $ last txt
        -- Pragmas will not be picked up in a non standard location,
        -- or when range starts on a Pragma
        extPragmas = takeWhile ("{-#" `T.isPrefixOf`)
        pragmas =
          let cp = extPragmas $ T.lines contents
              rp = not $ null $ extPragmas txt
          in  if null cp || rp
                then []
                -- head txt is safe when extractRange txt is safe
                else cp <> if T.all isSpace $ head txt then [] else [""]
        fixLine t = if T.all isSpace $ last txt then t else T.init t
        unStrip ws new =
          fixLine
            $ T.unlines
            $ map (ws `T.append`)
            $ drop (length pragmas)
            $ T.lines new
        mStrip = case txt of
          (l : _) ->
            let ws = fst $ T.span isSpace l
            in  (,) ws . T.unlines <$> traverse (T.stripPrefix ws) txt
          _ -> Nothing
      in
        maybe
          (return $ IdeResultFail
            (IdeError
              PluginError
              (T.pack
                "You must format a whole block of code. Ormolu does not support arbitrary ranges."
              )
              Null
            )
          )
          (\(ws, striped) ->
            ret (lineRange r)
              <$> (fmap (unStrip ws) <$> fmt (T.unlines pragmas <> striped))
          )
          mStrip
 where
  ret _ (Left err) = IdeResultFail
    (IdeError PluginError (T.pack $ "ormoluCmd: " ++ show err) Null)
  ret r (Right new) = IdeResultOk [TextEdit r new]

  exop s =
    "-X" `isPrefixOf` s || "-fplugin=" `isPrefixOf` s || "-pgmF=" `isPrefixOf` s
#else
provider _ _ _ _ = return $ IdeResultOk [] -- NOP formatter
#endif
