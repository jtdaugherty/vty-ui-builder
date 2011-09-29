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
                   then if generateMain config
                        then [ text "module Main where"
                             ]
                        else [ text $ "module " ++ moduleName config
                             , text "   ( buildCollection"
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
        lastIf = (length $ interfaceNames st) - 1
        firstIfname = fst (interfaceNames st !! lastIf)
        main = if generateMain config
               then [ text ""
                    , text "main :: IO ()"
                    , text "main = do"
                    , text "  (c, values) <- buildCollection"
                    , text "  (fg_" <> text firstIfname <> text " values) `onKeyPressed` \\_ k _ ->"
                    , text "    case k of"
                    , text "      (KASCII 'q') -> shutdownUi >> return True"
                    , text "      _ -> return False"
                    , text ""
                    , text "  runUi c defaultContext"
                    ]
               else []
    in vcat $ preamble
           ++ imports
           ++ [ text ""
              , typeDoc
              , text ""
              , text $ "buildCollection :: IO (Collection, InterfaceElements)"
              , text "buildCollection = do"
              , nest 2 $ vcat [ genDoc st
                              , mkElementsValue st
                              , text "return (c, elems)"
                              ]
              ]
           ++ main

mkElementsValue :: GenState a -> Doc
mkElementsValue st =
    let ls = header ++ [nest 2 $ addCommas body "  "] ++ footer
        body = elem_lines ++ if_act_lines ++ if_fg_lines
        elem_lines = (flip map) (namedValues st) $ \(fieldName, valName) ->
                     text "elem_" <> toDoc fieldName
                     <> text " = " <> toDoc valName
        if_act_lines = (flip map) (interfaceNames st) $
                       \(ifName, vals) ->
                           text "switchTo_"
                                    <> text ifName
                                    <> text " = "
                                    <> (toDoc $ switchActionName vals)
        if_fg_lines = (flip map) (interfaceNames st) $
                      \(ifName, vals) ->
                          text "fg_"
                                   <> text ifName
                                   <> text " = "
                                   <> (toDoc $ focusGroupName vals)
        header = [ text "InterfaceElements {"
                 ]
        footer = [ text "}"
                 ]
    in text "let elems = " <> (vcat ls)