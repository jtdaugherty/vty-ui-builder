module Graphics.Vty.Widgets.Builder.GenLib
    ( gen
    , generateTypes
    , elemChildren
    , getString
    , append
    , newEntry
    , getAttribute
    , getIntAttributeValue
    , attrsToExpr
    , registerInterface
    , addCommas
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
    , mkTyCon
    )
where

import Control.Applicative
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Combinators hiding (when)
import Text.PrettyPrint.HughesPJ

import Graphics.Vty.Widgets.Builder.Types

gen :: Element Posn -> String -> GenM ()
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
                      registerFieldValueName newName fieldValName
                      -- When setting up the widget value to be added
                      -- to the focus group for this widget, always
                      -- use the resultWidgetName name since it will
                      -- have the right type (Widget a) in the
                      -- generated source.
                      setFocusValue newName $ resultWidgetName result

          -- Use common attributes on the element to annotate it with
          -- widget-agnostic properties.
          annotateElement e nam
          append $ text ""
gen _ _ = error "Got unsupported element structure"

lookupWidgetName :: String -> GenM (Maybe WidgetName)
lookupWidgetName nam = lookup nam <$> allWidgetNames <$> get

registerWidgetName :: WidgetName -> GenM ()
registerWidgetName wn =
    modify $ \st ->
        st { allWidgetNames = allWidgetNames st ++ [(widgetName wn, wn)] }

registerFieldValueName :: String -> AnyName -> GenM ()
registerFieldValueName fName valName =
    modify $ \st ->
        st { registeredFieldNames = registeredFieldNames st ++ [(fName, valName)] }

lookupFocusValue :: String -> GenM (Maybe WidgetName)
lookupFocusValue s = lookup s <$> focusValues <$> get

-- Using the registered element names in the input document, generate
-- a type with fields for each of the named elements.
generateTypes :: GenState -> Doc
generateTypes st =
    let header = [ text "data InterfaceElements = InterfaceElements {"
                 ]
        footer = [ text "}"
                 ]
        body = elem_lines ++ if_act_lines ++ if_fg_lines
        elem_lines = (flip map) (registeredFieldNames st) $ \(fieldName, valName) ->
                     let typeExpr = case valName of
                                      WName wName -> toDoc $ TyCon "Widget" [widgetType wName]
                                      VName vName -> text $ valueType vName
                     in "elem_" >- fieldName >- " :: " >- typeExpr
        if_act_lines = (flip map) (interfaceNames st) $ \(ifName, _) ->
                       (text $ "switchTo_" ++ ifName ++ " :: IO ()")
        if_fg_lines = (flip map) (interfaceNames st) $ \(ifName, _) ->
                      (text $ "fg_" ++ ifName ++ " :: Widget FocusGroup")

    in vcat (header ++ [nest 2 $ addCommas body "  "] ++ footer)

mergeFocus :: String -> String -> GenM ()
mergeFocus wName fgName = do
  st <- get
  put $ st { focusMethods = (wName, Merge fgName) : focusMethods st }

lookupFocusMethod :: String -> GenM (Maybe FocusMethod)
lookupFocusMethod valName = do
  st <- get
  return $ lookup valName (focusMethods st)

setFocusValue :: String -> WidgetName -> GenM ()
setFocusValue s wName = do
  modify $ \st -> st { focusValues = (s, wName) : focusValues st }

getWidgetStateType :: String -> GenM TyCon
getWidgetStateType nam = do
  vts <- gets allWidgetNames
  case lookup nam vts of
    Nothing -> error $ "BUG: request for state type for value "
               ++ show nam
               ++ " impossible; did the element handler forget"
               ++ " to register the type?"
    Just wName -> return $ widgetType wName

isValidParamName :: String -> GenM Bool
isValidParamName s = (isJust . lookup s) <$> gets paramNames

getParamType :: String -> GenM String
getParamType s = do
  typ <- lookup s <$> gets paramNames
  case typ of
    Nothing -> error $ "Invalid parameter name: " ++ s
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

attrsToExpr :: (Maybe String, Maybe String) -> Maybe String
attrsToExpr (Nothing, Nothing) = Nothing
attrsToExpr (Just fg, Nothing) = Just $ "fgColor " ++ fg
attrsToExpr (Nothing, Just bg) = Just $ "bgColor " ++ bg
attrsToExpr (Just fg, Just bg) = Just $ fg ++ " `on` " ++ bg

registerInterface :: String -> InterfaceValues -> GenM ()
registerInterface ifName vals = do
  st <- get
  -- It's important to append the interface information so that the
  -- order of the interfaces in the XML is preserved in the generated
  -- code.
  put $ st { interfaceNames = interfaceNames st ++ [(ifName, vals)]
           }

annotateElement :: Element a -> String -> GenM ()
annotateElement e nam = do
  -- Normal attribute override
  let normalResult = ( getAttribute e "normalFg"
                     , getAttribute e "normalBg"
                     )
  case attrsToExpr normalResult of
    Nothing -> return ()
    Just expr ->
        append $ "setNormalAttribute " >- nam >- " $ " >- expr

  -- Focus attribute override
  let focusResult = ( getAttribute e "focusFg"
                    , getAttribute e "focusBg"
                    )
  case attrsToExpr focusResult of
    Nothing -> return ()
    Just expr ->
        append $ "setFocusAttribute " >- nam >- " $ " >- expr

elemChildren :: Element a -> [Element a]
elemChildren (Elem _ _ cs) = map getElem contents
    where
      getElem (CElem e _) = e
      getElem _ = error "BUG: getElem got a non-element!"
      contents = concat $ map elm cs

getString :: Content i -> String
getString (CString _ s _) = s
getString _ = error "Cannot get string from non-CString content"

append :: Doc -> GenM ()
append d = do
  st <- get
  put $ st { genDoc = (genDoc st) $$ d }

newEntry :: String -> GenM String
newEntry n = do
  st <- get

  let val = case Map.lookup n (nameCounters st) of
              Just nextVal -> nextVal
              Nothing -> 1

  let newMap = Map.insert n (val + 1) (nameCounters st)
  put $ st { nameCounters = newMap }

  return $ n ++ show val

addCommas :: [Doc] -> String -> Doc
addCommas [] _ = text ""
addCommas (l:ls) s =
    text s <> l $$ (vcat $ map (text ", " <>) ls)

declareWidget :: String -> TyCon -> WidgetHandlerResult
declareWidget nam tyCon =
    WidgetHandlerResult { resultWidgetName = WidgetName { widgetName = nam
                                                        , widgetType = tyCon
                                                        }
                        , fieldValueName = Nothing
                        }

withField :: WidgetHandlerResult -> (String, String) -> WidgetHandlerResult
withField mh (val, typ) =
    mh { fieldValueName = Just $ ValueName { valueName = val
                                           , valueType = typ
                                           }
       }

addImport :: String -> GenM ()
addImport s = do
  st <- get
  put $ st { imports = imports st ++ [s] }

registerParam :: String -> String -> GenM ()
registerParam nam typ = do
  st <- get
  put $ st { paramNames = paramNames st ++ [(nam,typ)] }

mkTyCon :: String -> TyCon
mkTyCon s =
    if ' ' `elem` s
    then TyCon (takeWhile (/= ' ') s) [TyCon (tail $ dropWhile (/= ' ') s) []]
    else TyCon s []
