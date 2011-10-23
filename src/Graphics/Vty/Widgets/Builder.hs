module Graphics.Vty.Widgets.Builder
    ( generateSourceForDocument
    , validateAgainstDTD
    )
where

import System.IO
import qualified Data.Map as Map
import Control.Monad.State
import Text.PrettyPrint.HughesPJ

import Text.XML.HaXml.Parse hiding (doctypedecl)
import Text.XML.HaXml.Validate
import Text.XML.HaXml.Types

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.Config
import Graphics.Vty.Widgets.Builder.GenLib
import Graphics.Vty.Widgets.Builder.DTDGenerator
import Graphics.Vty.Widgets.Builder.ValidateLib

getSourceGenerator :: ElementHandler -> AnyElementSourceGenerator
getSourceGenerator (WidgetElementHandler h _ _) = WSrc h
getSourceGenerator (StructureElementHandler h _ _) = SSrc h

generateSourceForDocument :: BuilderConfig
                          -> ValidatedElement
                          -> [ElementHandler]
                          -> IO String
generateSourceForDocument config (Validated e) theHandlers = do
  let (_, finalState) = runState (gen e "root") initialState
      initialState = GenState { nameCounters = Map.empty
                              , genDoc = empty
                              , handlers = map (\h -> (elementName h, getSourceGenerator h)) theHandlers
                              , interfaceNames = []
                              , focusMethods = []
                              , imports = []
                              , allWidgetNames = []
                              , registeredFieldNames = []
                              , focusValues = []
                              , paramNames = []
                              }

  return $ render $ generateSourceDoc config finalState

validateAgainstDTD :: Handle
                   -> FilePath
                   -> [(FilePath, [ElementHandler])]
                   -> IO (Either [String] ValidatedElement)
validateAgainstDTD inputXmlHandle inputXmlPath elemInfo = do
  masterDTD <- generateMasterDTD elemInfo
  dtd <- case dtdParse' "<generated>" masterDTD of
           Right (Just dtd) -> return dtd
           Right Nothing -> error "No DTD found in generated DTD text!"
           Left e -> error $ "Error parsing generated DTD: " ++ e

  xmlContents <- hGetContents inputXmlHandle
  case xmlParse' inputXmlPath xmlContents of
    Left e -> error $ "Error parsing input XML "
              ++ (show inputXmlPath) ++ ": " ++ e
    Right (Document _ _ e _) -> do
         case partialValidate dtd e of
           [] -> do
             let hs = concat $ map snd elemInfo
             result <- doValidation e hs
             case result of
               [] -> return $ Right $ Validated e
               es -> return $ Left es
           es -> return $ Left es

blk :: [Doc] -> Doc
blk ls = nest 2 $ vcat ls

generateSourceDoc :: BuilderConfig -> GenState -> Doc
generateSourceDoc config st =
    let typeDoc = [ text ""
                  , generateTypes st
                  ]
        preamble = if generateMain config
                   then [ text "module Main where"
                        ]
                   else [ text $ "module " ++ moduleName config
                        , nest 3 $ vcat [ text "( buildCollection"
                                        , text ", InterfaceElements(..)"
                                        , text ")"
                                        ]
                        , text "where"
                        ]
        vtyImports = [ text ""
                     , text "import Graphics.Vty hiding (Button)"
                     , text "import Graphics.Vty.Widgets.All"
                     ]
        customImports = map (text . ("import " ++)) (imports st)
        lastIf = (length $ interfaceNames st) - 1
        keyHandlers = (flip map) (zip [0..] $ interfaceNames st) $
                       \(i, (nam, _)) ->
                           let nextIfName = fst $ interfaceNames st !! if i == lastIf
                                                                       then 0
                                                                       else i + 1

                           in vcat [ text ""
                                   , "(fg_" >- nam >- " values) `onKeyPressed` \\_ k _ ->"
                                   , blk [ text "case k of"
                                         , blk [ text "KEsc -> shutdownUi >> return True"
                                               , "(KASCII 'n') -> switchTo_" >- nextIfName >- " values >> return True"
                                               , text "_ -> return False"
                                               ]
                                         ]
                                   ]

        builderDoc = [ text ""
                     , text "buildCollection :: " <> theParamTypes <> text "IO (Collection, InterfaceElements)"
                     , text "buildCollection " <> theParamNames <> text "= do"
                     , nest 2 $ vcat [ genDoc st
                                     , mkElementsValue st
                                     , text "return (c, elems)"
                                     ]
                     ]

        theParamTypes = hcat $ map (\typ -> (toDoc $ TyCon "Widget" [mkTyCon typ]) <> text " -> ")
                        (map snd $ paramNames st)
        theParamNames = hcat $ map (\(n,_) -> text n <> text " ") (paramNames st)

        main = [ text ""
               , text "main :: IO ()"
               , text "main = do"
               , blk [ text "(c, values) <- buildCollection"
                     , vcat keyHandlers
                     , text ""
                     , text "runUi c defaultContext"
                     ]
               ]
        sections = [ (generateModulePreamble, preamble)
                   , (generateImports, vtyImports)
                   , (const True, customImports)
                   , (generateInterfaceType, typeDoc)
                   , (generateInterfaceBuilder, builderDoc)
                   , (generateMain, main)
                   ]
    in vcat $ map (\(f, docLines) -> if f config then vcat docLines else empty) sections

mkElementsValue :: GenState -> Doc
mkElementsValue st =
    let ls = header ++ [nest 2 $ addCommas body "  "] ++ footer
        body = elem_lines ++ if_act_lines ++ if_fg_lines
        elem_lines = (flip map) (registeredFieldNames st) $ \(fieldName, fieldValName) ->
                     let valName = case fieldValName of
                                     WName wName -> widgetName wName
                                     VName vName -> valueName vName
                     in "elem_" >- fieldName >- " = " >- valName
        if_act_lines = (flip map) (interfaceNames st) $
                       \(ifName, vals) ->
                           "switchTo_" >- ifName >- " = "
                             >- (switchActionName vals)
        if_fg_lines = (flip map) (interfaceNames st) $
                      \(ifName, vals) ->
                          "fg_" >- ifName >- " = " >- (focusGroupName vals)
        header = [ text "InterfaceElements {"
                 ]
        footer = [ text "}"
                 ]
    in "let elems = " >- (vcat ls)