{-# OPTIONS_GHC -w #-}
module Graphics.Vty.Widgets.Builder.GenLib
    ( gen
    , append
    , newEntry
    , elemAttribute
    , attrsToExpr
    , registerInterface
    , lookupFocusMethod
    , declareWidget
    , withField
    , mergeFocus
    , getWidgetStateType
    , registerWidgetName
    , lookupFocusValue
    , registerParam
    , parseType
    , nameStr
    , getFieldValueName
    , specChildWidgets
    , specChildElements
    , getElementStringContent
    , registerFieldValueName
    , getNamedWidgetNames
    , widgetLikeType
    , specType
    , doFullValidation
    , putError

    -- Combinators for constructing data model values during
    -- validation
    , required
    , requiredInt
    , requiredChar
    , optional
    , optionalInt
    , requiredEqual
    , firstChildWidget

    -- Common names
    , collectionName
    , uiElementsName

    -- Helper functions for source generation
    , call
    , bind
    , tBind
    , noLoc
    , act
    , expr
    , mkTyp
    , mkList
    , parens
    , mkName
    , mkString
    , mkInt
    , mkChar
    , mkTup
    , opApp
    , mkLet
    , mkImportDecl
    )
where

import Control.Applicative hiding (optional)
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.Util
import qualified Graphics.Vty.Widgets.Builder.AST as A
import qualified Language.Haskell.Exts as Hs

doFullValidation :: A.Doc
                 -> [WidgetSpecHandler]
                 -> [Error]
doFullValidation doc theHandlers =
    -- Match up widget specs in the document with handlers
    let handlersBySpecType = map (\s -> (specType s, s)) theHandlers
        mapping = map (\s -> (s, lookup (A.widgetType s) handlersBySpecType)) $ allSpecs doc

        mkMsg s = show (A.widgetLocation s) ++ ": unknown widget type " ++ show (A.widgetType s)

        process (s, Nothing) = Just $ Error (A.widgetLocation s) $ mkMsg s
        process (s, Just h) = doSpecValidation h s

        msgs = catMaybes $ map process mapping

    in msgs

allSpecs :: A.Doc -> [A.WidgetSpec]
allSpecs doc =
    concat [ A.documentSharedWidgets doc
           , concat $ map interfaceSpecs $ A.documentInterfaces doc
           ]
        where
          interfaceSpecs = wLikeSpecs . A.interfaceContent

          wLikeSpecs (A.Ref _) = []
          wLikeSpecs (A.Widget w) = w : subSpecs w

          subSpecs spec =
              concat $ map wSpecContentSpecs $ A.widgetSpecContents spec

          wSpecContentSpecs (A.ChildWidgetLike w) = wLikeSpecs w
          wSpecContentSpecs (A.Text _ _) = []
          wSpecContentSpecs (A.ChildElement _) = []

doSpecValidation :: WidgetSpecHandler
                 -> A.WidgetSpec
                 -> Maybe Error
doSpecValidation (WidgetSpecHandler _ doValidate _) spec =
    case doValidate spec of
      Left e -> Just e
      Right _ -> Nothing

generateWidgetSource :: WidgetSpecHandler
                     -> A.WidgetSpec
                     -> Hs.Name
                     -> GenM WidgetHandlerResult
generateWidgetSource (WidgetSpecHandler genSrc doValidate specTyp) spec nam = do
  case doValidate spec of
    Left e -> error $ "Error while generating widget source for type " ++ show specTyp ++
              " (up-front validation should have prevented this): " ++ show e
    Right val -> genSrc spec nam val

specType :: WidgetSpecHandler
         -> String
specType (WidgetSpecHandler _ _ t) = t

gen :: A.WidgetLike -> Hs.Name -> GenM ()
gen (A.Widget spec) nam = do
  hs <- gets handlers
  case lookup (A.widgetType spec) hs of
    Nothing -> error $ show (A.widgetLocation spec) ++
               ": no handler for widget type " ++ (show $ A.widgetType spec)
    Just handler -> do
      result <- generateWidgetSource handler spec nam

      -- Register the widget value name.
      registerWidgetName $ resultWidgetName result

      -- If the element has an ID, use that to set up field
      -- information so we know how to assign the widget to the field.
      case getAttribute spec "id" of
        Nothing -> return ()
        Just newName -> do
                      let fieldValName = case fieldValueName result of
                                           Just fValName -> VName fValName
                                           Nothing -> WName $ resultWidgetName result
                      -- When determining which value constitutes the
                      -- type of the interface elements field for this
                      -- widget, use the custom field value if
                      -- specified by the handler (which may not have
                      -- type Widget a), or fall back to the widget
                      -- value if no custom field value was specified.
                      registerFieldValueName (mkName newName) fieldValName
                      -- When setting up the widget value to be added
                      -- to the focus group for this widget, always
                      -- use the resultWidgetName name since it will
                      -- have the right type (Widget a) in the
                      -- generated source.
                      setFocusValue (mkName newName) $ resultWidgetName result

      -- Use common attributes on the element to annotate it with
      -- widget-agnostic properties.
      annotateWidget spec nam

gen (A.Ref (A.Reference tgt loc)) nam = do
  let target = mkName tgt

  val <- getFieldValueName target

  result <- case val of
              Nothing -> do
                result <- isValidParamName target
                case result of
                  False -> error $ show loc ++
                           ": ref: target '" ++ tgt ++ "' invalid"
                  True -> do
                           typ <- getParamType target
                           append $ mkLet [(nam, expr target)]
                           return $ declareWidget nam typ
              Just (WName valName) -> do
                append $ mkLet [(nam, expr $ widgetName valName)]
                typ <- getWidgetStateType $ widgetName valName
                return $ declareWidget nam typ
              Just (VName _) -> error $ show loc ++
                                ": ref: target '" ++ tgt
                                ++ "' references non-widget type"

  registerWidgetName $ resultWidgetName result

getNamedWidgetNames :: A.WidgetLike -> [A.WidgetId]
getNamedWidgetNames wlike = catMaybes $ getNamedWidgetNames' wlike
    where
      getNamedWidgetNames' (A.Ref _) = []
      getNamedWidgetNames' (A.Widget spec) =
          A.widgetId spec : (concat $ map getNamedWidgetNames' $ specChildWidgets spec)

putError :: A.SourceLocation -> String -> GenM ()
putError loc s =
    modify $ \st -> st { errorMessages = errorMessages st ++ [Error loc s] }

registerWidgetName :: WidgetName -> GenM ()
registerWidgetName wn =
    modify $ \st ->
        st { allWidgetNames = allWidgetNames st ++ [(widgetName wn, wn)] }

registerFieldValueName :: Hs.Name -> AnyName -> GenM ()
registerFieldValueName fName valName = do
  modify $ \st ->
      st { registeredFieldNames = registeredFieldNames st ++ [(fName, valName)]
         }

getFieldValueName :: Hs.Name -> GenM (Maybe AnyName)
getFieldValueName fName = lookup fName <$> registeredFieldNames <$> get

lookupFocusValue :: Hs.Name -> GenM (Maybe WidgetName)
lookupFocusValue s = lookup s <$> focusValues <$> get

mergeFocus :: Hs.Name -> Hs.Name -> GenM ()
mergeFocus wName fgName = do
  st <- get
  put $ st { focusMethods = (wName, Merge fgName) : focusMethods st }

lookupFocusMethod :: Hs.Name -> GenM (Maybe FocusMethod)
lookupFocusMethod valName = do
  st <- get
  return $ lookup valName (focusMethods st)

setFocusValue :: Hs.Name -> WidgetName -> GenM ()
setFocusValue s wName = do
  modify $ \st -> st { focusValues = (s, wName) : focusValues st }

getWidgetStateType :: Hs.Name -> GenM Hs.Type
getWidgetStateType nam = do
  vts <- gets allWidgetNames
  case lookup nam vts of
    Nothing -> error $ "BUG: request for state type for value "
               ++ show nam
               ++ " impossible; did the element handler forget"
               ++ " to register the type?"
    Just wName -> return $ widgetType wName

isValidParamName :: Hs.Name -> GenM Bool
isValidParamName s = (isJust . lookup s) <$> gets paramNames

getParamType :: Hs.Name -> GenM Hs.Type
getParamType s = do
  typ <- lookup s <$> gets paramNames
  case typ of
    Nothing -> error $ "Invalid parameter name: " ++ show s
    Just t -> return t

getAttribute :: A.WidgetSpec -> String -> Maybe String
getAttribute spec attrName = lookup attrName (A.widgetSpecAttributes spec)

elemAttribute :: A.Element -> String -> Maybe String
elemAttribute e attrName = lookup attrName (A.elementAttributes e)

requiredEqual :: A.WidgetSpec -> String -> String -> Either Error String
requiredEqual spec attrName expected =
    case required spec attrName of
      Left e -> Left e
      Right v -> if v == expected
                 then Right v
                 else Left $ Error (A.widgetLocation spec) $
                          "Attribute value for attribute " ++
                          show attrName ++ " must be " ++ show expected

firstChildWidget :: A.WidgetSpec -> Either Error A.WidgetLike
firstChildWidget spec =
    case specChildWidgets spec of
      (ch:_) -> Right ch
      _ -> Left $ Error (A.widgetLocation spec) "required first child widget is missing"

required :: A.WidgetSpec -> String -> Either Error String
required spec attrName =
    case optional' spec attrName of
      Nothing -> Left $ Error (A.widgetLocation spec) $
                 "attribute " ++ show attrName ++ " required"
      Just val -> Right val

optional :: A.WidgetSpec -> String -> Either Error (Maybe String)
optional spec nam = Right (optional' spec nam)

optional' :: A.WidgetSpec -> String -> Maybe String
optional' spec attrName =
    lookup attrName (A.widgetSpecAttributes spec)

getInt :: A.WidgetSpec -> String -> Either Error String -> Either Error Int
getInt spec attrName val =
    case val of
      Left e -> Left e
      Right s ->
          case reads s of
            [] -> Left $ Error (A.widgetLocation spec) $
                  "Attribute " ++ show attrName ++
                  " value must be an integer"
            ((v,_):_) -> Right v

requiredInt :: A.WidgetSpec -> String -> Either Error Int
requiredInt spec attrName = getInt spec attrName $ required spec attrName

requiredChar :: A.WidgetSpec -> String -> Either Error Char
requiredChar spec attrName =
    case required spec attrName of
      Left e -> Left e
      Right s ->
          if null s
          then Left $ Error (A.widgetLocation spec) $
                   "Attribute " ++ show attrName ++ " must be non-empty"
          else if length s > 1
               then Left $ Error (A.widgetLocation spec) $
                        "Attribute " ++ show attrName ++ " must be one character in length"
               else Right $ head s

optionalInt :: A.WidgetSpec -> String -> Either Error (Maybe Int)
optionalInt spec attrName =
    case optional' spec attrName of
      Nothing -> Right Nothing
      Just val -> Just <$> getInt spec attrName (Right val)

attrsToExpr :: (Maybe String, Maybe String) -> Maybe Hs.Exp
attrsToExpr (Nothing, Nothing) = Nothing
attrsToExpr (Just fg, Nothing) = Just $ call "fgColor" [expr $ mkName fg]
attrsToExpr (Nothing, Just bg) = Just $ call "bgColor" [expr $ mkName bg]
attrsToExpr (Just fg, Just bg) = Just $ opApp
                                 (expr $ mkName fg)
                                 (mkName "on")
                                 (expr $ mkName bg)

registerInterface :: String -> InterfaceValues -> GenM ()
registerInterface ifName vals = do
  st <- get
  -- It's important to append the interface information so that the
  -- order of the interfaces in the AST is preserved in the generated
  -- code.
  put $ st { interfaceNames = interfaceNames st ++ [(ifName, vals)]
           }

nameStr :: Hs.Name -> String
nameStr (Hs.Ident s) = s
nameStr n = error $ "Unsupported name: " ++ (show n)

noLoc :: Hs.SrcLoc
noLoc = Hs.SrcLoc { Hs.srcFilename = "-"
                  , Hs.srcLine = 0
                  , Hs.srcColumn = 0
                  }

mkTyp :: String -> [Hs.Type] -> Hs.Type
mkTyp tyCon [] = Hs.TyCon $ Hs.UnQual $ mkName tyCon
mkTyp tyCon args = mkApp (Hs.TyCon (Hs.UnQual (mkName tyCon))) args
    where
      mkApp ty [] = ty
      mkApp ty (ty':tys) = mkApp (Hs.TyApp ty ty') tys

call :: String -> [Hs.Exp] -> Hs.Exp
call func [] = Hs.Var $ Hs.UnQual $ mkName func
call func args =
    mkApp (Hs.Var $ Hs.UnQual $ mkName func) args
        where
          mkApp app [] = app
          mkApp app (arg:rest) = mkApp (Hs.App app arg) rest

bind :: Hs.Name -> String -> [Hs.Exp] -> Hs.Stmt
bind lval func args =
    Hs.Generator noLoc (Hs.PVar lval) $ call func args

tBind :: [Hs.Name] -> String -> [Hs.Exp] -> Hs.Stmt
tBind lvals func args =
    Hs.Generator noLoc (Hs.PTuple (map Hs.PVar lvals)) $ call func args

act :: Hs.Exp -> Hs.Stmt
act = Hs.Qualifier

expr :: Hs.Name -> Hs.Exp
expr = Hs.Var . Hs.UnQual

parens :: Hs.Exp -> Hs.Exp
parens = Hs.Paren

mkString :: String -> Hs.Exp
mkString = Hs.Lit . Hs.String

mkInt :: Int -> Hs.Exp
mkInt = Hs.Lit . Hs.Int . toEnum

mkChar :: Char -> Hs.Exp
mkChar = Hs.Lit . Hs.Char

mkTup :: [Hs.Exp] -> Hs.Exp
mkTup = Hs.Tuple

mkList :: [Hs.Exp] -> Hs.Exp
mkList = Hs.List

opApp :: Hs.Exp -> Hs.Name -> Hs.Exp -> Hs.Exp
opApp a op b = Hs.InfixApp a (Hs.QVarOp $ Hs.UnQual op) b

mkLet :: [(Hs.Name, Hs.Exp)] -> Hs.Stmt
mkLet pairs = Hs.LetStmt $ Hs.BDecls $ map mkDecl pairs
    where
      mkDecl (nam, e) = Hs.PatBind
                        noLoc
                        (Hs.PVar nam)
                        Nothing
                        (Hs.UnGuardedRhs e)
                        (Hs.BDecls [])

annotateWidget :: A.WidgetSpec -> Hs.Name -> GenM ()
annotateWidget spec nam = do
  -- Normal attribute override
  let normalResult = ( getAttribute spec "normalFg"
                     , getAttribute spec "normalBg"
                     )
  case attrsToExpr normalResult of
    Nothing -> return ()
    Just e ->
        append $ act $ call "setNormalAttribute" [expr nam, e]

  -- Focus attribute override
  let focusResult = ( getAttribute spec "focusFg"
                    , getAttribute spec "focusBg"
                    )
  case attrsToExpr focusResult of
    Nothing -> return ()
    Just e ->
        append $ act $ call "setFocusAttribute" [expr nam, e]

mkName :: String -> Hs.Name
mkName = Hs.Ident

widgetLikeType :: A.WidgetLike -> String
widgetLikeType (A.Ref _) = "ref"
widgetLikeType (A.Widget w) = A.widgetType w

specChildWidgets :: A.WidgetSpec -> [A.WidgetLike]
specChildWidgets =
    concat . map extractSpec . filter isChildWidgetLike . A.widgetSpecContents
    where
      extractSpec (A.ChildWidgetLike s) = [s]
      extractSpec (A.ChildElement _) = []
      extractSpec (A.Text _ _) = []

specChildElements :: A.WidgetSpec -> [A.Element]
specChildElements =
    concat . map extractSpec . filter isChildElement . A.widgetSpecContents
    where
      extractSpec (A.ChildElement e) = [e]
      extractSpec (A.ChildWidgetLike _) = []
      extractSpec (A.Text _ _) = []

isChildWidgetLike :: A.WidgetSpecContent -> Bool
isChildWidgetLike (A.ChildWidgetLike _) = True
isChildWidgetLike _ = False

isChildElement :: A.WidgetSpecContent -> Bool
isChildElement (A.ChildElement _) = True
isChildElement _ = False

getElementStringContent :: A.Element -> String
getElementStringContent =
    concat . map elemText . A.elementContents
    where
      elemText (A.ElemText s _) = s
      elemText _ = []

append :: Hs.Stmt -> GenM ()
append stmt =
    modify $ \st -> st { hsStatements = hsStatements st ++ [stmt] }

newEntry :: String -> GenM Hs.Name
newEntry n = do
  st <- get

  let val = case Map.lookup n (nameCounters st) of
              Just nextVal -> nextVal
              Nothing -> 1

  let newMap = Map.insert n (val + 1) (nameCounters st)
  put $ st { nameCounters = newMap }

  return $ mkName $ (replace '-' '_' n) ++ show val

declareWidget :: Hs.Name -> Hs.Type -> WidgetHandlerResult
declareWidget nam tyCon =
    WidgetHandlerResult { resultWidgetName = WidgetName { widgetName = nam
                                                        , widgetType = tyCon
                                                        }
                        , fieldValueName = Nothing
                        }

withField :: WidgetHandlerResult -> (Hs.Name, Hs.Type) -> WidgetHandlerResult
withField mh (val, typ) =
    mh { fieldValueName = Just $ ValueName { valueName = val
                                           , valueType = typ
                                           }
       }

mkImportDecl :: String -> [String] -> Hs.ImportDecl
mkImportDecl name hidden =
    Hs.ImportDecl { Hs.importLoc = noLoc
                  , Hs.importModule = Hs.ModuleName name
                  , Hs.importQualified = False
                  , Hs.importSrc = False
                  , Hs.importPkg = Nothing
                  , Hs.importAs = Nothing
                  , Hs.importSpecs = case hidden of
                                       [] -> Nothing
                                       is -> Just (True, map (Hs.IVar . mkName) is)
                  }

registerParam :: Hs.Name -> Hs.Type -> GenM ()
registerParam nam typ =
    modify $ \st -> st { paramNames = paramNames st ++ [(nam, typ)] }

parseType :: String -> Hs.Type
parseType s =
    case Hs.parse s of
      Hs.ParseOk val -> val
      Hs.ParseFailed _ msg -> error $ "Error parsing type string '" ++ s ++ "': " ++ msg

collectionName :: Hs.Name
collectionName = mkName "c"

uiElementsName :: Hs.Name
uiElementsName = mkName "elems"
