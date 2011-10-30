module Graphics.Vty.Widgets.Builder.GenLib
    ( gen
    , elemChildren
    , getString
    , append
    , newEntry
    , getAttribute
    , getIntAttributeValue
    , attrsToExpr
    , registerInterface
    , lookupFocusMethod
    , declareWidget
    , withField
    , addImport
    , mergeFocus
    , getWidgetStateType
    , lookupWidgetName
    , registerWidgetName
    , lookupFocusValue
    , registerParam
    , isValidParamName
    , getParamType
    , parseType
    , nameStr

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

import Control.Applicative
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Combinators hiding (when)

import Graphics.Vty.Widgets.Builder.Types
import qualified Language.Haskell.Exts as Hs

gen :: Element Posn -> Hs.Name -> GenM ()
gen e@(Elem (N n) _ _) nam = do
  hs <- gets handlers
  case lookup n hs of
    Nothing -> error $ "No handler for element type " ++ (show n)
    Just handler -> do
      case handler of
        SSrc h -> h e nam
        WSrc h -> do
          result <- h e nam

          -- Register the widget value name.
          registerWidgetName $ resultWidgetName result

          -- If the element has an ID, use that to set up field
          -- information so we know how to assign the widget to the
          -- field.
          case getAttribute e "id" of
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
          annotateElement e nam
gen _ _ = error "Got unsupported element structure"

lookupWidgetName :: Hs.Name -> GenM (Maybe WidgetName)
lookupWidgetName nam = lookup nam <$> allWidgetNames <$> get

registerWidgetName :: WidgetName -> GenM ()
registerWidgetName wn =
    modify $ \st ->
        st { allWidgetNames = allWidgetNames st ++ [(widgetName wn, wn)] }

registerFieldValueName :: Hs.Name -> AnyName -> GenM ()
registerFieldValueName fName valName =
    modify $ \st ->
        st { registeredFieldNames = registeredFieldNames st ++ [(fName, valName)] }

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

getAttribute :: Element a -> String -> Maybe String
getAttribute (Elem _ attrs _) attrName =
    case lookup (N attrName) attrs of
      Just (AttValue ((Left s):_)) -> Just s
      _ -> Nothing

getIntAttributeValue :: String -> Maybe Int
getIntAttributeValue s = do
  case reads s of
    [] -> Nothing
    ((i,_):_) -> return i

attrsToExpr :: (Maybe String, Maybe String) -> Maybe Hs.Exp
attrsToExpr (Nothing, Nothing) = Nothing
attrsToExpr (Just fg, Nothing) = Just $ call "fgColor" [expr $ mkName fg]
attrsToExpr (Nothing, Just bg) = Just $ call "bgColor" [expr $ mkName bg]
attrsToExpr (Just fg, Just bg) = Just $ opApp
                                 (expr $ mkName fg)
                                 "on"
                                 (expr $ mkName bg)

registerInterface :: String -> InterfaceValues -> GenM ()
registerInterface ifName vals = do
  st <- get
  -- It's important to append the interface information so that the
  -- order of the interfaces in the XML is preserved in the generated
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

opApp :: Hs.Exp -> String -> Hs.Exp -> Hs.Exp
opApp a op b = Hs.InfixApp a (Hs.QVarOp $ Hs.UnQual $ mkName op) b

mkLet :: [(Hs.Name, Hs.Exp)] -> Hs.Stmt
mkLet pairs = Hs.LetStmt $ Hs.BDecls $ map mkDecl pairs
    where
      mkDecl (nam, e) = Hs.PatBind
                        noLoc
                        (Hs.PVar nam)
                        Nothing
                        (Hs.UnGuardedRhs e)
                        (Hs.BDecls [])

annotateElement :: Element a -> Hs.Name -> GenM ()
annotateElement elemt nam = do
  -- Normal attribute override
  let normalResult = ( getAttribute elemt "normalFg"
                     , getAttribute elemt "normalBg"
                     )
  case attrsToExpr normalResult of
    Nothing -> return ()
    Just e ->
        append $ act $ call "setNormalAttribute" [expr nam, e]

  -- Focus attribute override
  let focusResult = ( getAttribute elemt "focusFg"
                    , getAttribute elemt "focusBg"
                    )
  case attrsToExpr focusResult of
    Nothing -> return ()
    Just e ->
        append $ act $ call "setFocusAttribute" [expr nam, e]

mkName :: String -> Hs.Name
mkName = Hs.Ident

elemChildren :: Element a -> [Element a]
elemChildren (Elem _ _ cs) = map getElem contents
    where
      getElem (CElem e _) = e
      getElem _ = error "BUG: getElem got a non-element!"
      contents = concat $ map elm cs

getString :: Content i -> String
getString (CString _ s _) = s
getString _ = error "Cannot get string from non-CString content"

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

  let replace _ _ [] = []
      replace from_ to_ (e:es) = if e == from_
                                 then to_ : replace from_ to_ es
                                 else e : replace from_ to_ es

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

addImport :: String -> GenM ()
addImport s = modify $ \st -> st { imports = imports st ++ [mkImportDecl s []] }

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
