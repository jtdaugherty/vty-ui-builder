module Graphics.Vty.Widgets.Builder
    ( generateModuleSource
    )
where

import Control.Monad.State
import Text.PrettyPrint.HughesPJ

import Text.XML.HaXml.Parse hiding (doctypedecl)
import Text.XML.HaXml.Validate
import Text.XML.HaXml.Types

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import Graphics.Vty.Widgets.Builder.Handlers
import Graphics.Vty.Widgets.Builder.DTDGenerator

generateModuleSource :: FilePath
                     -> FilePath
                     -> [(String, ElementHandler a)]
                     -> IO String
generateModuleSource inputXmlPath dtdPath extraHandlers = do
  masterDTD <- generateMasterDTD (elementHandlers ++ extraHandlers) dtdPath
  dtd <- case dtdParse' "<generated>" masterDTD of
           Right (Just dtd) -> return dtd
           Right Nothing -> error "No DTD found in generated DTD text!"
           Left e -> error $ "Error parsing generated DTD: " ++ e

  xmlContents <- readFile inputXmlPath
  case xmlParse' inputXmlPath xmlContents of
    Left e -> error $ "Error parsing input XML " ++ (show inputXmlPath) ++ ": " ++ e
    Right (Document _ _ e _) -> do
         case partialValidate dtd e of
           [] -> do
             let (_, st') = runState (gen e "root") (GenState 0 empty elementHandlers [])
             return $ render $ genDoc st'
           es -> do
             mapM_ putStrLn es
             error $ "Error validating " ++ (show inputXmlPath)
