module Graphics.Vty.Widgets.Builder
    ( generateSource
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

generateSource :: BuilderConfig
               -> ValidatedElement
               -> [(String, ElementHandler)]
               -> IO String
generateSource config (Validated e) theHandlers = do
  let (_, finalState) = runState (gen e $ ValueName "root") initialState
      initialState = GenState { nameCounters = Map.empty
                              , genDoc = empty
                              , handlers = theHandlers
                              , namedValues = []
                              , valueTypes = []
                              , interfaceNames = []
                              , focusMethods = []
                              }

  return $ render $ generateSourceDoc config finalState

validateAgainstDTD :: Handle
                   -> FilePath
                   -> FilePath
                   -> [String]
                   -> [String]
                   -> [(String, ElementValidator)]
                   -> IO (Either [String] ValidatedElement)
validateAgainstDTD inputXmlHandle inputXmlPath dtdPath structuralElementNames widgetElementNames validators = do
  masterDTD <- generateMasterDTD structuralElementNames widgetElementNames dtdPath
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
             result <- doValidation e validators
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
        imports = [ text ""
                  , text "import Graphics.Vty hiding (Button)"
                  , text "import Graphics.Vty.Widgets.All"
                  ]
        lastIf = (length $ interfaceNames st) - 1
        keyHandlers = (flip map) (zip [0..] $ interfaceNames st) $
                       \(i, (nam, _)) ->
                           let nextIfName = fst $ interfaceNames st !! if i == lastIf
                                                                       then 0
                                                                       else i + 1

                           in vcat [ text ""
                                   , text "(fg_" <> text nam <> text " values) `onKeyPressed` \\_ k _ ->"
                                   , blk [ text "case k of"
                                         , blk [ text "KEsc -> shutdownUi >> return True"
                                               , text "(KASCII 'n') -> switchTo_" <> text nextIfName <> text " values >> return True"
                                               , text "_ -> return False"
                                               ]
                                         ]
                                   ]

        builderDoc = [ text ""
                     , text $ "buildCollection :: IO (Collection, InterfaceElements)"
                     , text "buildCollection = do"
                     , nest 2 $ vcat [ genDoc st
                                     , mkElementsValue st
                                     , text "return (c, elems)"
                                     ]
                     ]

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
                   , (generateImports, imports)
                   , (generateInterfaceType, typeDoc)
                   , (generateInterfaceBuilder, builderDoc)
                   , (generateMain, main)
                   ]
    in vcat $ map (\(f, docLines) -> if f config then vcat docLines else empty) sections

mkElementsValue :: GenState -> Doc
mkElementsValue st =
    let ls = header ++ [nest 2 $ addCommas body "  "] ++ footer
        body = elem_lines ++ if_act_lines ++ if_fg_lines
        elem_lines = (flip map) (namedValues st) $ \(fieldName, (fieldValName, _)) ->
                     text "elem_" <> toDoc fieldName
                     <> text " = " <> toDoc fieldValName
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