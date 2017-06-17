{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE LambdaCase            #-}
module Haskell.Ide.BrittanyPlugin where

import           Data.Aeson
import           Data.Monoid
import qualified Data.Text as T
import           Data.Vinyl
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Control.Monad.IO.Class
import           Language.Haskell.Brittany
import           Language.Haskell.Brittany.Types
import           Language.Haskell.Brittany.Config
import qualified DynFlags as GHC
import qualified GHC.LanguageExtensions.Type as GHC
import qualified Data.Text.Lazy as TextL
import qualified GhcMod.Utils as GM
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import           Control.Lens


brittanyCmd :: TextDocumentIdentifier -> Maybe Range -> IdeM (IdeResponse [J.TextEdit])
brittanyCmd tdi range =
  pluginGetFile "genapplicative: " (tdi ^. J.uri) $ \file -> do
    case range of
      Just r -> do
        text <- GM.withMappedFile file $ liftIO . readFile
        res <- liftIO $ runBrittany $ extractRange r text
        case res of
          Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "brittanyCmd: " ++ show err) Null)
          Right newText -> do
            let newString = TextL.unpack newText
                textEdit = J.TextEdit r $ T.pack newString
            return $ IdeResponseOk [textEdit]
      Nothing -> do
        text <- GM.withMappedFile file $ liftIO . readFile
        res <- liftIO $ runBrittany text
        case res of
          Left err -> return $ IdeResponseFail (IdeError PluginError
                      (T.pack $ "brittanyCmd: " ++ show err) Null)
          Right newText -> do
            let newString = TextL.unpack newText
                startPos = Position 0 0
                endPos = toPos (l,c+1)
                l = length $ textLines
                c = length $ last textLines
                textLines = lines text
                textEdit = J.TextEdit (Range startPos endPos) $ T.pack newString
            return $ IdeResponseOk [textEdit]

extractRange :: Range -> String -> String
extractRange (Range (Position sl sc) (Position el ec)) s = newS
  where focusLines = take (el-sl+1) $ drop sl $ lines s
        fixFirstLine (x:xs) = drop sc x : xs
        fixLastLine' (x:xs) = take ec x : xs
        fixLastLine = reverse . fixLastLine' . reverse
        newS = unlines $ fixLastLine $ fixFirstLine $ focusLines

runBrittany :: String -> IO (Either String TextL.Text)
runBrittany text = do
    let config = staticDefaultConfig
    let ghcOptions = []
    let cppCheckFunc dynFlags = if GHC.xopt GHC.Cpp dynFlags
          then return $ Left "Encountered -XCPP. Aborting."
          else return $ Right False
    parseResult <- parseModuleFromString ghcOptions "stdin" cppCheckFunc text
    case parseResult of
      Left left -> do
        return $ Left left
      Right (anns, parsedSource, _) -> do
        (errsWarns, outLText) <- pPrintModuleAndCheck config anns parsedSource
        let shouldOutput = null errsWarns
        if shouldOutput then
          return $ Right outLText
        else
          return $ Left $ unlines $ map showErrs errsWarns

showErrs :: LayoutError -> String
showErrs (LayoutErrorUnusedComment s) = s
showErrs (LayoutWarning s) = s
showErrs (LayoutErrorUnknownNode s _) = s
showErrs LayoutErrorOutputCheck = "Brittany error - invalid output"

