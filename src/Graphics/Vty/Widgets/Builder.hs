module Graphics.Vty.Widgets.Builder
    ( generateModuleSource
    )
where

import Control.Monad.State
import Data.List (intersperse)
import Data.Maybe (fromJust)
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
             let (_, finalState) = runState (gen e "root") (GenState 0 empty elementHandlers [] [])
             return $ render $ fullModuleSource "FooBar" finalState
           es -> do
             mapM_ putStrLn es
             error $ "Error validating " ++ (show inputXmlPath)

fullModuleSource :: String -> GenState a -> Doc
fullModuleSource moduleName st =
    let typeDoc = generateTypes st
        rootType = fromJust $ lookup "root" $ valueTypes st
    in vcat [ text $ "module " ++ moduleName
            , text "   ( mkInterface"
            , text "   , InterfaceElements(..)"
            , text "   )"
            , text "where"
            , text ""
            , text "import Graphics.Vty"
            , text "import Graphics.Vty.Widgets.All"
            , text ""
            , text $ "type UIRootType = " ++ rootType
            , text ""
            , typeDoc
            , text ""
            , text $ "mkInterface :: IO (Widget UIRootType, InterfaceElements)"
            , text "mkInterface = do"
            , nest 2 $ vcat [ genDoc st
                            , mkElementsValue st
                            , text "return (root, elems)"
                            ]
            ]

mkElementsValue :: GenState a -> Doc
mkElementsValue st =
    let lines = header ++ [nest 2 $ vcat body] ++ footer
        body = intersperse (text ", ") $ (flip map) (namedValues st) $ \(fieldName, valName) ->
               text $ "elem_" ++ fieldName ++ " = " ++ valName
        header = [ text "InterfaceElements {"
                 ]
        footer = [ text "}"
                 ]
    in text "let elems = " <> (vcat lines)