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
import Graphics.Vty.Widgets.Builder.Config
import Graphics.Vty.Widgets.Builder.GenLib
import Graphics.Vty.Widgets.Builder.Handlers
import Graphics.Vty.Widgets.Builder.DTDGenerator

generateModuleSource :: BuilderConfig
                     -> FilePath
                     -> FilePath
                     -> [(String, ElementHandler a)]
                     -> IO String
generateModuleSource config inputXmlPath dtdPath extraHandlers = do
  masterDTD <- generateMasterDTD (elementHandlers ++ extraHandlers) dtdPath
  dtd <- case dtdParse' "<generated>" masterDTD of
           Right (Just dtd) -> return dtd
           Right Nothing -> error "No DTD found in generated DTD text!"
           Left e -> error $ "Error parsing generated DTD: " ++ e

  xmlContents <- readFile inputXmlPath
  case xmlParse' inputXmlPath xmlContents of
    Left e -> error $ "Error parsing input XML "
              ++ (show inputXmlPath) ++ ": " ++ e
    Right (Document _ _ e _) -> do
         case partialValidate dtd e of
           [] -> do
             let (_, finalState) = runState (gen e $ ValueName "root")
                                   (GenState 0 empty elementHandlers [] [] [])
             return $ render $ fullModuleSource config finalState
           es -> do
             mapM_ putStrLn es
             error $ "Error validating " ++ (show inputXmlPath)

fullModuleSource :: BuilderConfig -> GenState a -> Doc
fullModuleSource config st =
    let typeDoc = generateTypes st
        preamble = if generateModulePreamble config
                   then [ text $ "module " ++ moduleName config
                        , text "   ( mkInterface"
                        , text "   , InterfaceElements(..)"
                        , text "   )"
                        , text "where"
                        ]
                   else []
        imports = if generateImports config
                  then [ text ""
                       , text "import Graphics.Vty"
                       , text "import Graphics.Vty.Widgets.All"
                       ]
                  else []
    in vcat $ preamble
           ++ imports
           ++ [ text ""
              , typeDoc
              , text ""
              , text $ "mkInterface :: IO (Collection, InterfaceElements)"
              , text "mkInterface = do"
              , nest 2 $ vcat [ genDoc st
                              , mkElementsValue st
                              , text "return (c, elems)"
                              ]
              ]

mkElementsValue :: GenState a -> Doc
mkElementsValue st =
    let ls = header ++ [nest 2 $ addCommas body "  "] ++ footer
        body = elem_lines ++ if_act_lines
        elem_lines = (flip map) (namedValues st) $ \(fieldName, valName) ->
                     text "elem_" <> toDoc fieldName
                     <> text " = " <> toDoc valName
        if_act_lines = (flip map) (interfaceNames st) $
                       \(ifName, (_, actName)) ->
                           text "switchTo_"
                                    <> text ifName
                                    <> text " = "
                                    <> toDoc actName
        header = [ text "InterfaceElements {"
                 ]
        footer = [ text "}"
                 ]
    in text "let elems = " <> (vcat ls)