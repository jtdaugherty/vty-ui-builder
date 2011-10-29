module Graphics.Vty.Widgets.Builder
    ( generateSourceForDocument
    , validateAgainstDTD
    , style
    , mode
    )
where

import System.IO
import qualified Data.Map as Map
import Control.Monad.State
import Data.List (intercalate)
import Data.Maybe

import Text.XML.HaXml.Parse hiding (doctypedecl)
import Text.XML.HaXml.Validate
import Text.XML.HaXml.Types

import qualified Language.Haskell.Exts as Hs

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.Config
import Graphics.Vty.Widgets.Builder.GenLib
import Graphics.Vty.Widgets.Builder.DTDGenerator
import Graphics.Vty.Widgets.Builder.ValidateLib

getSourceGenerator :: ElementHandler -> AnyElementSourceGenerator
getSourceGenerator (WidgetElementHandler h _ _) = WSrc h
getSourceGenerator (StructureElementHandler h _ _) = SSrc h

style :: Hs.Style
style = Hs.style { Hs.lineLength = 72 }

mode :: Hs.PPHsMode
mode = Hs.defaultMode { Hs.doIndent = 2, Hs.spacing = True }

generateSourceForDocument :: BuilderConfig
                          -> ValidatedElement
                          -> [ElementHandler]
                          -> IO (Either String String)
generateSourceForDocument config (Validated e) theHandlers = do
  let (_, finalState) = runState (gen e $ mkName "root") initialState
      initialState = GenState { nameCounters = Map.empty
                              , hsStatements = []
                              , handlers = map (\h -> (elementName h, getSourceGenerator h)) theHandlers
                              , interfaceNames = []
                              , focusMethods = []
                              , imports = []
                              , allWidgetNames = []
                              , registeredFieldNames = []
                              , focusValues = []
                              , paramNames = []
                              }

  -- If the user wants to generate a main function, we can't do that
  -- if the generated collection constructor function takes parameters
  -- because we don't have values to provide for them.
  case generateMain config && (not $ null $ paramNames finalState) of
    True -> return $ Left $ concat
            [ "configuration indicates that a 'main' should be generated, "
            , "but parameters are required to construct the interface. "
            , "Turn off 'main' generation to generate the interface source."
            ]
    False -> return $ Right $ Hs.prettyPrintStyleMode style mode $
             generateModule config finalState

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

generateModule :: BuilderConfig -> GenState -> Hs.Module
generateModule config st =
    let modName = if generateMain config
                  then "Main"
                  else moduleName config

        theExports = if null exportList
                     then Nothing
                     else Just exportList

        exportList = if generateMain config
                     then []
                     else catMaybes [ if generateInterfaceBuilder config
                                      then Just $ Hs.EVar (Hs.UnQual $ mkName "buildCollection")
                                      else Nothing
                                    , if generateInterfaceType config
                                      then Just $ Hs.EThingAll (Hs.UnQual $ mkName "InterfaceElements")
                                      else Nothing
                                    ]

        theImports = [ mkImportDecl "Graphics.Vty" ["Button"]
                     , mkImportDecl "Graphics.Vty.Widgets.All" []
                     ] ++ imports st

        main = mkMain $ concat [ [tBind [mkName "c", mkName "values"] "buildCollection" []]
                               , mkKeyHandlers st
                               , [act $ call "runUi" [expr $ mkName "c", expr $ mkName "defaultContext"]]
                               ]

        moduleBody = concat [ if generateInterfaceType config then [mkInterfaceType st] else []
                            , if generateInterfaceBuilder config then mkBuilderFunction st else []
                            , if generateMain config then main else []
                            ]

    in Hs.Module noLoc (Hs.ModuleName modName) [] Nothing theExports theImports moduleBody

-- Using the registered element names in the input document, generate
-- a type with fields for each of the named elements.
mkInterfaceType :: GenState -> Hs.Decl
mkInterfaceType st =
    let elem_fields = (flip map) (registeredFieldNames st) $ \(fieldName, valName) ->
                      let typeExpr = case valName of
                                       WName wName -> mkTyp "Widget" [widgetType wName]
                                       VName vName -> valueType vName
                      in ([mkName $ "elem_" ++ nameStr fieldName], Hs.UnBangedTy typeExpr)

        if_act_fields = (flip map) (interfaceNames st) $ \(ifName, _) ->
                        ([mkName $ "switchTo_" ++ ifName], Hs.UnBangedTy $ parseType "IO ()")

        if_fg_fields = (flip map) (interfaceNames st) $ \(ifName, _) ->
                       ([mkName $ "fg_" ++ ifName], Hs.UnBangedTy $ parseType "Widget FocusGroup")

        fields = concat [ elem_fields
                        , if_act_fields
                        , if_fg_fields
                        ]

        qualConDecl = Hs.QualConDecl noLoc [] [] conDecl
        conDecl = Hs.RecDecl (mkName "InterfaceElements") fields

    in Hs.DataDecl noLoc Hs.DataType [] (mkName "InterfaceElements") [] [qualConDecl] []

mkBuilderFunction :: GenState -> [Hs.Decl]
mkBuilderFunction st =
    let theParamTypes = map (\typ -> mkTyp "Widget" [typ]) (map snd $ paramNames st)
        theParamNames = map (Hs.PVar . fst) $ paramNames st

        typeStr = intercalate " -> " $
                  map Hs.prettyPrint theParamTypes ++ ["IO (Collection, InterfaceElements)"]

    in [ Hs.TypeSig noLoc [mkName "buildCollection"] $ parseType typeStr
       , Hs.FunBind [ Hs.Match noLoc (Hs.Ident "buildCollection") theParamNames Nothing
                                   (Hs.UnGuardedRhs (Hs.Do $ hsStatements st
                                                                 ++ [ mkElementsValue st
                                                                    , act $ call "return" [ mkTup [ expr $ mkName "c"
                                                                                                  , expr $ mkName "elems"
                                                                                                  ]
                                                                                          ]
                                                                    ]
                                                    )
                                   ) (Hs.BDecls [])
                    ]
       ]

mkKeyHandlers :: GenState -> [Hs.Stmt]
mkKeyHandlers st =
    (flip map) (zip [0..] $ interfaceNames st) $
                   \(i, (nam, _)) ->
                       let nextIfName = fst $ interfaceNames st !! if i == lastIf
                                                                   then 0
                                                                   else i + 1
                           lastIf = (length $ interfaceNames st) - 1

                       in act $ opApp (Hs.Var $ Hs.UnQual $ mkName $ "(fg_" ++ nam ++ " values)")
                              "onKeyPressed"
                              $ Hs.Lambda noLoc [Hs.PWildCard, Hs.PVar $ mkName "k", Hs.PWildCard] $
                                Hs.Case (Hs.Var $ Hs.UnQual $ mkName "k")
                                      [ Hs.Alt noLoc (Hs.PApp (Hs.UnQual $ mkName "KEsc") [])
                                                   (Hs.UnGuardedAlt $ Hs.Do [ act $ call "shutdownUi" []
                                                                            , act $ call "return" [Hs.Con $ Hs.UnQual $ mkName "True"]
                                                                            ])
                                                   (Hs.BDecls [])
                                      , Hs.Alt noLoc (Hs.PApp (Hs.UnQual $ mkName "KASCII") [Hs.PLit $ Hs.Char 'n'])
                                                   (Hs.UnGuardedAlt $ Hs.Do [ act $ call ("switchTo_" ++ nextIfName) [expr $ mkName "values"]
                                                                            , act $ call "return" [Hs.Con $ Hs.UnQual $ mkName "True"]
                                                                            ])
                                                   (Hs.BDecls [])
                                      , Hs.Alt noLoc Hs.PWildCard
                                                   (Hs.UnGuardedAlt $ Hs.Do [act $ call "return" [Hs.Con $ Hs.UnQual $ mkName "False"]])
                                                   (Hs.BDecls [])
                                      ]

mkMain :: [Hs.Stmt] -> [Hs.Decl]
mkMain body =
    [ Hs.TypeSig noLoc [mkName "main"] $ parseType "IO ()"
    , Hs.FunBind [Hs.Match noLoc (Hs.Ident "main") [] Nothing
                        (Hs.UnGuardedRhs (Hs.Do body)) $ Hs.BDecls []]
    ]

mkElementsValue :: GenState -> Hs.Stmt
mkElementsValue st =
    let elem_values = (flip map) (registeredFieldNames st) $ \(fieldName, fieldValName) ->
                      let valName = case fieldValName of
                                      WName wName -> widgetName wName
                                      VName vName -> valueName vName
                      in ("elem_" ++ nameStr fieldName, valName)

        if_act_values = (flip map) (interfaceNames st) $
                        \(ifName, vals) ->
                            ("switchTo_" ++ ifName,
                             switchActionName vals)

        if_fg_values = (flip map) (interfaceNames st) $
                       \(ifName, vals) ->
                           ("fg_" ++ ifName,
                            focusGroupName vals)

        allFieldValues :: [(String, Hs.Name)]
        allFieldValues = concat [ elem_values
                                , if_act_values
                                , if_fg_values
                                ]

        fields :: [Hs.FieldUpdate]
        fields = map mkFieldUpdate allFieldValues

        mkFieldUpdate :: (String, Hs.Name) -> Hs.FieldUpdate
        mkFieldUpdate (nam, val) = Hs.FieldUpdate (Hs.UnQual $ mkName nam) $
                                   Hs.Var (Hs.UnQual val)

        elemsValue :: Hs.Exp
        elemsValue = Hs.RecConstr (Hs.UnQual $ mkName "InterfaceElements")
                     fields
    in mkLet [(mkName "elems", elemsValue)]