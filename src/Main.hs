module Main where

import Text.XML.HaXml.Parse hiding (doctypedecl)
import Text.XML.HaXml.Validate
import Text.XML.HaXml.Types

import System
import System.FilePath ((</>))
import Control.Monad
import Control.Monad.State

import Paths_vty_ui_builder

import Text.PrettyPrint.HughesPJ

import Graphics.Vty.Widgets.Builder.DTDGenerator
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import Graphics.Vty.Widgets.Builder.Handlers

usage :: String
usage = "builder-test <XML filename>"

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ error usage

  let [xmlFilename] = args

  dataDir <- getDataDir
  let dtdPath = dataDir </> "dtd"

  masterDTD <- generateMasterDTD elementHandlers dtdPath
  dtd <- case dtdParse' "<generated>" masterDTD of
           Right (Just dtd) -> return dtd
           Right Nothing -> error "No DTD found in generated DTD text!"
           Left e -> error $ "Error parsing generated DTD: " ++ e

  xmlContents <- readFile xmlFilename
  case xmlParse' xmlFilename xmlContents of
    Left e -> putStrLn e

    Right (Document _ _ e _) -> do
         case partialValidate dtd e of
           [] -> do
             let (_, st') = runState (gen e "root") (GenState 0 empty elementHandlers)
             putStrLn $ render $ genDoc st'
           es -> mapM_ putStrLn es
