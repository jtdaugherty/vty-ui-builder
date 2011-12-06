module Graphics.Vty.Widgets.Builder.GenLib
    ( gen
    , append
    , newEntry
    , registerInterface
    , lookupFocusMethod
    , declareWidget
    , withField
    , mergeFocus
    , getWidgetStateType
    , registerWidgetName
    , lookupFocusValue
    , getFieldValueName
    , getElementStringContent
    , registerFieldValueName
    , widgetLikeName
    , putError
    , getAttribute
    , registerReferenceTarget
    , setFocusValue
    )
where

import Control.Applicative hiding (optional)
import Control.Monad.State
import qualified Data.Map as Map

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.Util
import qualified Graphics.Vty.Widgets.Builder.AST as A
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S
import qualified Language.Haskell.Exts as Hs

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

gen :: A.WidgetLike -> Hs.Name -> GenM ()
gen (A.Widget spec) nam = do
  hs <- gets elemHandlers
  case lookup (A.widgetElementName spec) hs of
    Nothing -> error $ show (A.sourceLocation spec) ++
               ": no handler for widget type " ++ (show $ A.widgetElementName spec)
    Just handler -> do
      doc <- gets document
      iface <- gets currentInterface
      let st = ValidationState iface doc
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
                      registerFieldValueName (S.mkName newName) fieldValName

                      -- Register the 'id' as a valid reference target
                      -- so that 'ref' tags can use it
                      registerReferenceTarget (S.mkName newName) A.InterfaceWidgetRef
                        (widgetName $ resultWidgetName result)
                        (widgetType $ resultWidgetName result)

                      -- When setting up the widget value to be added
                      -- to the focus group for this widget, always
                      -- use the resultWidgetName name since it will
                      -- have the right type (Widget a) in the
                      -- generated source.
                      setFocusValue (S.mkName newName) $ resultWidgetName result

      -- Use common attributes on the element to annotate it with
      -- widget-agnostic properties.
      annotateWidget spec nam

gen (A.WidgetRef (A.WidgetReference tgt loc refType)) nam = do
  let target = S.mkName tgt

  val <- getReferenceTarget target refType

  result <- case val of
              Nothing -> error $ show loc ++
                         ": ref: target '" ++ tgt ++ "' invalid"
              Just (wName, typ) -> do
                append $ S.mkLet [(nam, S.expr wName)]
                return $ declareWidget nam typ

  registerWidgetName $ resultWidgetName result

getReferenceTarget :: Hs.Name -> A.ReferenceType -> GenM (Maybe (Hs.Name, Hs.Type))
getReferenceTarget nam typ =
    lookup (nam, typ) <$> gets referenceTargets

registerReferenceTarget :: Hs.Name -> A.ReferenceType -> Hs.Name -> Hs.Type -> GenM ()
registerReferenceTarget target refType valName typ =
    modify $ \st ->
        st { referenceTargets = referenceTargets st
                                ++ [((target, refType), (valName, typ))]
           }

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

getAttribute :: (A.IsElement a) => a -> String -> Maybe String
getAttribute val attrName = lookup attrName (A.getAttributes val)

registerInterface :: String -> InterfaceValues -> GenM ()
registerInterface ifName vals = do
  st <- get
  -- It's important to append the interface information so that the
  -- order of the interfaces in the AST is preserved in the generated
  -- code.
  put $ st { interfaceNames = interfaceNames st ++ [(ifName, vals)]
           }

annotateWidget :: A.WidgetElement -> Hs.Name -> GenM ()
annotateWidget spec nam = do
  -- Normal attribute override
  let normalResult = ( getAttribute spec "normalFg"
                     , getAttribute spec "normalBg"
                     )
  case S.attrsToExpr normalResult of
    Nothing -> return ()
    Just e ->
        append $ S.act $ S.call "setNormalAttribute" [S.expr nam, e]

  -- Focus attribute override
  let focusResult = ( getAttribute spec "focusFg"
                    , getAttribute spec "focusBg"
                    )
  case S.attrsToExpr focusResult of
    Nothing -> return ()
    Just e ->
        append $ S.act $ S.call "setFocusAttribute" [S.expr nam, e]

widgetLikeName :: A.WidgetLike -> String
widgetLikeName (A.WidgetRef _) = "ref"
widgetLikeName (A.Widget w) = A.widgetElementName w

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

  return $ S.mkName $ (replace '-' '_' n) ++ show val

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
