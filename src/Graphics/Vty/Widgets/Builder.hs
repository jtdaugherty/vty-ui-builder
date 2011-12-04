module Graphics.Vty.Widgets.Builder
where

import qualified Data.Map as Map
import Control.Monad.State
import Data.List (intercalate)
import Data.Maybe

import qualified Language.Haskell.Exts as Hs

import qualified Graphics.Vty.Widgets.Builder.AST as A
import qualified Graphics.Vty.Widgets.Builder.Names as Names
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.Config
import Graphics.Vty.Widgets.Builder.GenLib
import Graphics.Vty.Widgets.Builder.Validation (doFullValidation)
import Graphics.Vty.Widgets.Builder.Handlers (handleDoc)

style :: Hs.Style
style = Hs.style { Hs.lineLength = 72 }

mode :: Hs.PPHsMode
mode = Hs.defaultMode { Hs.doIndent = 2, Hs.spacing = True }

prettyPrintSource :: Hs.Module -> String
prettyPrintSource = Hs.prettyPrintStyleMode style mode

generateSourceForDocument :: BuilderConfig
                          -> A.Doc
                          -> [WidgetElementHandler]
                          -> IO (Either [Error] Hs.Module)
generateSourceForDocument config doc theHandlers = do
  case doFullValidation doc theHandlers of
    Left es -> return $ Left es
    Right valState ->
        do
          let (_, finalState) = runState (handleDoc doc) initialState
              initialState = GenState { nameCounters = Map.empty
                                      , hsStatements = []
                                      , handlers = map (\h -> (specType h, h)) theHandlers
                                      , interfaceNames = []
                                      , focusMethods = []
                                      , allWidgetNames = []
                                      , registeredFieldNames = []
                                      , focusValues = []
                                      , paramNames = []
                                      , errorMessages = []
                                      , validationState = valState
                                      }

          case errorMessages finalState of
            [] -> do
               -- If the user wants to generate a main function, we
               -- can't do that if the generated collection
               -- constructor function takes parameters because we
               -- don't have values to provide for them.
               case generateMain config && (not $ null $ paramNames finalState) of
                 True -> return $ Left [
                             Error A.noLoc $ concat
                             [ "configuration indicates that a 'main' should be generated, "
                             , "but parameters are required to construct the interface. "
                             , "Turn off 'main' generation to generate the interface source."
                             ]
                            ]
                 False -> do
                       let moduleBody = generateModuleBody config finalState
                           result = generateModule config doc moduleBody
                       return $ Right result
            msgs -> return $ Left msgs

generateModule :: BuilderConfig -> A.Doc -> [Hs.Decl] -> Hs.Module
generateModule config doc moduleBody =
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
                     , mkImportDecl "Data.Monoid" []
                     ] ++ (map (\i -> mkImportDecl (A.importModuleName i) []) $ A.documentImports doc)

    in Hs.Module noLoc (Hs.ModuleName modName) [] Nothing theExports theImports moduleBody

generateModuleBody :: BuilderConfig -> GenState -> [Hs.Decl]
generateModuleBody config st =
    let main = mkMain $ concat [ [tBind [Names.collectionName, mkName "values"] "buildCollection" []]
                               , mkKeyHandlers st
                               , [act $ call "runUi" [expr Names.collectionName, expr $ mkName "defaultContext"]]
                               ]

        moduleBody = concat [ if generateInterfaceType config then [mkInterfaceType st] else []
                            , if generateInterfaceBuilder config then mkBuilderFunction st else []
                            , if generateMain config then main else []
                            ]

    in moduleBody

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
                                                                    , act $ call "return" [ mkTup [ expr Names.collectionName
                                                                                                  , expr Names.uiElementsName
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

                           prevIfName = fst $ interfaceNames st !! if i == firstIf
                                                                   then lastIf
                                                                   else i - 1
                           firstIf = 0

                       in act $ opApp (Hs.Var $ Hs.UnQual $ mkName $ "(fg_" ++ nam ++ " values)")
                              (mkName "onKeyPressed")
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
                                      , Hs.Alt noLoc (Hs.PApp (Hs.UnQual $ mkName "KASCII") [Hs.PLit $ Hs.Char 'p'])
                                                   (Hs.UnGuardedAlt $ Hs.Do [ act $ call ("switchTo_" ++ prevIfName) [expr $ mkName "values"]
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
    in mkLet [(Names.uiElementsName, elemsValue)]
