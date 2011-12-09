module Graphics.Vty.Widgets.Builder
    ( generateSourceForDocument
    , prettyPrintSource
    , style
    , mode
    )
where

import qualified Data.Map as Map
import Control.Applicative ((<$>))
import Control.Monad.State
import Data.List (intercalate)
import Data.Maybe

import qualified Language.Haskell.Exts as Hs

import qualified Graphics.Vty.Widgets.Builder.AST as A
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S
import qualified Graphics.Vty.Widgets.Builder.Names as Names
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.Util
import Graphics.Vty.Widgets.Builder.Config
import Graphics.Vty.Widgets.Builder.Validation (validateDocument)
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
  case validateDocument doc theHandlers of
    Left es -> return $ Left es
    Right () -> do
      let (_, finalState) = runState (handleDoc doc) initialState
          initialState = GenState { nameCounters = Map.empty
                                  , hsStatements = []
                                  , elemHandlers =
                                      map (\h -> (specType h, h)) theHandlers
                                  , interfaceNames = []
                                  , focusMethods = []
                                  , allWidgetNames = []
                                  , registeredFieldNames = []
                                  , focusValues = []
                                  , referenceTargets = []
                                  , document = doc
                                  , currentInterface = Nothing
                                  }

      -- If the user wants to generate a main function, we can't do
      -- that if the generated collection constructor function takes
      -- parameters because we don't have values to provide for them.
      case generateMain config && (not $ null $ A.documentParams doc) of
        True -> return $ Left [
                 Error A.noLoc $ concat
                           [ "configuration indicates that a 'main' should be generated, "
                           , "but parameters are required to construct the interface. "
                           , "Turn off 'main' generation to generate the interface source."
                           ]
                ]
        False -> do
          let moduleBody = generateModuleBody config doc finalState
              result = generateModule config doc moduleBody
          return $ Right result

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
                                      then Just $ Hs.EVar (Hs.UnQual $ S.mkName "buildCollection")
                                      else Nothing
                                    , if generateInterfaceType config
                                      then Just $ Hs.EThingAll (Hs.UnQual $ S.mkName "InterfaceElements")
                                      else Nothing
                                    ]

        theImports = [ S.mkImportDecl "Graphics.Vty" ["Button"]
                     , S.mkImportDecl "Graphics.Vty.Widgets.All" []
                     , S.mkImportDecl "Data.Monoid" []
                     ] ++ (map (\i -> S.mkImportDecl (A.importModuleName i) []) $ A.documentImports doc)

    in Hs.Module S.noLoc (Hs.ModuleName modName) [] Nothing theExports theImports moduleBody

generateModuleBody :: BuilderConfig -> A.Doc -> GenState -> [Hs.Decl]
generateModuleBody config doc st =
    let main = mkMain $ concat [ [S.tBind [Names.collectionName, S.mkName "values"] "buildCollection" []]
                               , mkKeyHandlers st
                               , [S.act $ S.call "runUi" [S.expr Names.collectionName, S.expr $ S.mkName "defaultContext"]]
                               ]

        moduleBody = concat [ if generateInterfaceType config then [mkInterfaceType st] else []
                            , if generateInterfaceBuilder config then mkBuilderFunction doc st else []
                            , if generateMain config then main else []
                            ]

    in moduleBody

-- Using the registered element names in the input document, generate
-- a type with fields for each of the named elements.
mkInterfaceType :: GenState -> Hs.Decl
mkInterfaceType st =
    let elem_fields = (flip map) (registeredFieldNames st) $ \(fieldName, valName) ->
                      let typeExpr = case valName of
                                       WName wName -> S.mkTyp "Widget" [widgetType wName]
                                       VName vName -> valueType vName
                      in ([S.mkName $ "elem_" ++ S.nameStr fieldName], Hs.UnBangedTy typeExpr)

        if_act_fields = (flip map) (interfaceNames st) $ \(ifName, _) ->
                        ([S.mkName $ "switchTo_" ++ ifName], Hs.UnBangedTy $ S.parseType "IO ()")

        if_fg_fields = (flip map) (interfaceNames st) $ \(ifName, _) ->
                       ([S.mkName $ "fg_" ++ ifName], Hs.UnBangedTy $ S.parseType "Widget FocusGroup")

        fields = concat [ elem_fields
                        , if_act_fields
                        , if_fg_fields
                        ]

        qualConDecl = Hs.QualConDecl S.noLoc [] [] conDecl
        conDecl = Hs.RecDecl (S.mkName "InterfaceElements") fields

    in Hs.DataDecl S.noLoc Hs.DataType [] (S.mkName "InterfaceElements") [] [qualConDecl] []

mkBuilderFunction :: A.Doc -> GenState -> [Hs.Decl]
mkBuilderFunction doc st =
    let theParamTypes = map (S.mkTyp "Widget" . (:[])) $ (S.parseType . A.paramName) <$> A.documentParams doc
        theParamNames = map (Hs.PVar . S.mkName) $ A.paramName <$> A.documentParams doc

        typeStr = intercalate " -> " $
                  map Hs.prettyPrint theParamTypes ++ ["IO (Collection, InterfaceElements)"]

    in [ Hs.TypeSig S.noLoc [S.mkName "buildCollection"] $ S.parseType typeStr
       , Hs.FunBind [ Hs.Match S.noLoc (Hs.Ident "buildCollection") theParamNames Nothing
                                   (Hs.UnGuardedRhs (Hs.Do $ hsStatements st
                                                                 ++ [ mkElementsValue st
                                                                    , S.act $ S.call "return" [ S.mkTup [ S.expr Names.collectionName
                                                                                                        , S.expr Names.uiElementsName
                                                                                                        ]
                                                                                              ]
                                                                    ]
                                                    )
                                   ) (Hs.BDecls [])
                    ]
       ]

mkKeyHandlers :: GenState -> [Hs.Stmt]
mkKeyHandlers st =
    let nextIfName i = fst $ interfaceNames st !! if i == lastIf
                                                  then 0
                                                  else i + 1
        lastIf = (length $ interfaceNames st) - 1
        prevIfName i = fst $ interfaceNames st !! if i == firstIf
                                                  then lastIf
                                                  else i - 1
        firstIf = 0
    in foreach (zip [0..] $ interfaceNames st) $
           \(i, (nam, _)) ->
               mkKeyHandlerStmt nam (nextIfName i) (prevIfName i)

mkKeyHandlerStmt :: String -> String -> String -> Hs.Stmt
mkKeyHandlerStmt ifName nextIfName prevIfName = do
  S.act $ S.opApp (Hs.Var $ Hs.UnQual $ S.mkName $ "(fg_" ++ ifName ++ " values)")
       (S.mkName "onKeyPressed")
       $ Hs.Lambda S.noLoc [Hs.PWildCard, Hs.PVar $ S.mkName "k", Hs.PWildCard] $
         Hs.Case (Hs.Var $ Hs.UnQual $ S.mkName "k")
               [ Hs.Alt S.noLoc (Hs.PApp (Hs.UnQual $ S.mkName "KEsc") [])
                            (Hs.UnGuardedAlt $ Hs.Do [ S.act $ S.call "shutdownUi" []
                                                     , S.act $ S.call "return" [Hs.Con $ Hs.UnQual $ S.mkName "True"]
                                                     ])
                            (Hs.BDecls [])
               , Hs.Alt S.noLoc (Hs.PApp (Hs.UnQual $ S.mkName "KASCII") [Hs.PLit $ Hs.Char 'n'])
                            (Hs.UnGuardedAlt $ Hs.Do [ S.act $ S.call ("switchTo_" ++ nextIfName) [S.expr $ S.mkName "values"]
                                                     , S.act $ S.call "return" [Hs.Con $ Hs.UnQual $ S.mkName "True"]
                                                     ])
                            (Hs.BDecls [])
               , Hs.Alt S.noLoc (Hs.PApp (Hs.UnQual $ S.mkName "KASCII") [Hs.PLit $ Hs.Char 'p'])
                            (Hs.UnGuardedAlt $ Hs.Do [ S.act $ S.call ("switchTo_" ++ prevIfName) [S.expr $ S.mkName "values"]
                                                     , S.act $ S.call "return" [Hs.Con $ Hs.UnQual $ S.mkName "True"]
                                                     ])
                            (Hs.BDecls [])
               , Hs.Alt S.noLoc Hs.PWildCard
                            (Hs.UnGuardedAlt $ Hs.Do [S.act $ S.call "return" [Hs.Con $ Hs.UnQual $ S.mkName "False"]])
                            (Hs.BDecls [])
               ]

mkMain :: [Hs.Stmt] -> [Hs.Decl]
mkMain body =
    [ Hs.TypeSig S.noLoc [S.mkName "main"] $ S.parseType "IO ()"
    , Hs.FunBind [Hs.Match S.noLoc (Hs.Ident "main") [] Nothing
                        (Hs.UnGuardedRhs (Hs.Do body)) $ Hs.BDecls []]
    ]

mkElementsValue :: GenState -> Hs.Stmt
mkElementsValue st =
    let elem_values = (flip map) (registeredFieldNames st) $ \(fieldName, fieldValName) ->
                      let valName = case fieldValName of
                                      WName wName -> widgetName wName
                                      VName vName -> valueName vName
                      in ("elem_" ++ S.nameStr fieldName, valName)

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
        mkFieldUpdate (nam, val) = Hs.FieldUpdate (Hs.UnQual $ S.mkName nam) $
                                   Hs.Var (Hs.UnQual val)

        elemsValue :: Hs.Exp
        elemsValue = Hs.RecConstr (Hs.UnQual $ S.mkName "InterfaceElements")
                     fields
    in S.mkLet [(Names.uiElementsName, elemsValue)]
