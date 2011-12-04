module Graphics.Vty.Widgets.Builder.GenLib
    ( gen
    , append
    , newEntry
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
    , getElementStringContent
    , registerFieldValueName
    , getNamedWidgetNames
    , widgetLikeName
    , specType
    , doFullValidation
    , putError
    , mkValidationState
    , getAttribute
    , getAttributes
    , getChildWidgetLikes
    , getChildElements
    , widgetElementName

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
import Data.List (nub)
import Data.Maybe
import qualified Data.Map as Map

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.Util
import qualified Graphics.Vty.Widgets.Builder.AST as A
import qualified Language.Haskell.Exts as Hs

widgetElementName :: A.WidgetElement -> String
widgetElementName = A.elementName . A.getElement

getAttributes :: (A.IsElement e) => e -> [(String, String)]
getAttributes = A.elementAttributes . A.getElement

getChildWidgetLikes :: (A.IsElement e) => e -> [A.WidgetLike]
getChildWidgetLikes = catMaybes . map getWL . A.elementContents . A.getElement
    where
      getWL (A.ChildWidgetLike w) = Just w
      getWL _ = Nothing

getChildElements :: (A.IsElement e) => e -> [A.Element]
getChildElements = catMaybes . map getEls . A.elementContents . A.getElement
    where
      getEls (A.ChildElement el) = Just el
      getEls _ = Nothing

doFullValidation :: A.Doc
                 -> [WidgetElementHandler]
                 -> Either [Error] ValidationState
doFullValidation doc theHandlers =
    case mkValidationState doc of
      Left es -> Left es
      Right st -> if null msgs
                  then Right st
                  else Left msgs
          where
            -- Match up widget specs in the document with handlers
            handlersBySpecType = map (\s -> (specType s, s)) theHandlers
            mapping = map (\s -> (s, lookup (widgetElementName s) handlersBySpecType)) $ allSpecs doc

            mkMsg s = "unknown widget type " ++ show (widgetElementName s)

            process (s, Nothing) = Just $ Error (A.sourceLocation s) $ mkMsg s
            process (s, Just h) = doSpecValidation h s st

            msgs = catMaybes $ map process mapping

mkValidationState :: A.Doc -> Either [Error] ValidationState
mkValidationState doc =
    checkValidRefTargets *> (ValidationState params <$> (resolveRefs $ allRefs doc))
        where
          params = map A.paramName $ A.documentParams doc

          -- Find all the widget specs that have IDs, i.e., those
          -- which may be referenced.
          validRefTargets = [ (fromJust $ A.widgetId s, s)
                              | s <- allSpecs doc, isJust $ A.widgetId s
                            ]

          -- Group each valid reference target (widget ID) with the
          -- list of specs which claimed the ID.  Right now the IDs
          -- must be unique over the whole document, so if any ID maps
          -- to more than one widget spec, that's illegal.
          mapping = [ (wId, snd <$> filter ((== wId) . fst) validRefTargets)
                     | wId <- nub $ map fst validRefTargets
                    ]

          duplicateRefs = filter ((> 1) . length . snd)

          checkValidRefTargets :: Either [Error] ()
          checkValidRefTargets = case duplicateRefs mapping of
                                   [] -> Right ()
                                   dups -> Left $ concat $ map mkErrors dups
          mkErrors (wId, specs) =
              [ Error (A.sourceLocation s) $ "Duplicate widget ID " ++ show wId ++ " defined"
                | s <- specs
              ]

          -- Use allRefs, report error if a reference cannot be resolved
          resolveRefs :: [(A.Interface, [A.Reference])] -> Either [Error] [(Hs.Name, A.WidgetElement)]
          resolveRefs [] = return []
          resolveRefs ((iface, rs):rest) = do
            a <- resolve iface rs
            b <- resolveRefs rest
            return $ a ++ b

          resolve :: A.Interface -> [A.Reference] -> Either [Error] [(Hs.Name, A.WidgetElement)]
          resolve _ [] = return []
          resolve iface ((A.Reference nam loc):rest) =
              case lookup nam validRefTargets of
                Just spec -> do
                  specs <- resolve iface rest
                  return $ (mkName nam, spec) : specs
                Nothing ->
                    case nam `elem` (map A.paramName $ A.documentParams doc) of
                      True -> resolve iface rest -- Can't get a spec,
                                                 -- but it's a valid
                                                 -- reference.
                      False ->
                          Left [Error loc $ "No widget with ID " ++ show nam
                                   ++ " is shared, is a parameter, or exists in interface " ++
                                          (show $ A.interfaceName iface)
                               ]

allRefs :: A.Doc -> [(A.Interface, [A.Reference])]
allRefs doc = [ (iface, catMaybes $ map getRef $ allWidgetLikes iface)
                    | iface <- A.documentInterfaces doc ]
    where
      getRef (A.Ref r) = Just r
      getRef (A.Widget _) = Nothing

allWidgetLikes :: A.Interface -> [A.WidgetLike]
allWidgetLikes iface = A.interfaceContent iface :
                       (wLikes $ A.interfaceContent iface)
    where
      wLikes (A.Ref _) = []
      wLikes (A.Widget w) = elementWLs $ A.getElement w

      elementWLs = concat . map elemWLs . A.elementContents

      elemWLs (A.Text _ _) = []
      elemWLs (A.ChildElement e) = elementWLs e
      elemWLs (A.ChildWidgetLike w) = w : wLikes w

allSpecs :: A.Doc -> [A.WidgetElement]
allSpecs doc =
    concat [ map snd $ A.documentSharedWidgets doc
           , catMaybes $ map getSpec $ concat $ map allWidgetLikes $ A.documentInterfaces doc
           ]
        where
          getSpec (A.Ref _) = Nothing
          getSpec (A.Widget w) = Just w

doSpecValidation :: WidgetElementHandler
                 -> A.WidgetElement
                 -> ValidationState
                 -> Maybe Error
doSpecValidation (WidgetElementHandler _ validator _) spec st =
    case runValidation (validator $ A.getElement spec) st of
      ValidationError e -> Just e
      Valid _ -> Nothing

generateWidgetSource :: WidgetElementHandler
                     -> A.WidgetElement
                     -> ValidationState
                     -> Hs.Name
                     -> GenM WidgetHandlerResult
generateWidgetSource (WidgetElementHandler genSrc validator specTyp) spec st nam = do
  case runValidation (validator $ A.getElement spec) st of
    ValidationError e -> error $ "Error while generating widget source for type " ++ show specTyp ++
                         " (up-front validation should have prevented this): " ++ show e
    Valid val -> genSrc nam val

specType :: WidgetElementHandler
         -> String
specType (WidgetElementHandler _ _ t) = t

gen :: A.WidgetLike -> Hs.Name -> GenM ()
gen (A.Widget spec) nam = do
  hs <- gets handlers
  case lookup (widgetElementName spec) hs of
    Nothing -> error $ show (A.sourceLocation spec) ++
               ": no handler for widget type " ++ (show $ widgetElementName spec)
    Just handler -> do
      st <- gets validationState
      result <- generateWidgetSource handler spec st nam

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
          A.widgetId spec : (concat $ map getNamedWidgetNames' $ getChildWidgetLikes spec)

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

getAttribute :: (A.IsElement a) => a -> String -> Maybe String
getAttribute val attrName = lookup attrName (getAttributes val)

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

annotateWidget :: A.WidgetElement -> Hs.Name -> GenM ()
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

widgetLikeName :: A.WidgetLike -> String
widgetLikeName (A.Ref _) = "ref"
widgetLikeName (A.Widget w) = widgetElementName w

getElementStringContent :: A.Element -> String
getElementStringContent =
    concat . map elemText . A.elementContents
    where
      elemText (A.Text s _) = s
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
registerParam nam typ = do
  modify $ \st -> st { paramNames = paramNames st ++ [(nam, typ)] }
  setFocusValue nam $ WidgetName { widgetName = nam
                                 , widgetType = typ
                                 }

parseType :: String -> Hs.Type
parseType s =
    case Hs.parse s of
      Hs.ParseOk val -> val
      Hs.ParseFailed _ msg -> error $ "Error parsing type string '" ++ s ++ "': " ++ msg
