module Graphics.Vty.Widgets.Builder.GenLib
    ( gen
    , generateTypes
    , elemChildren
    , getString
    , append
    , newEntry
    , getAttribute
    , attrsToExpr
    , lookupFieldValueName
    , lookupWidgetValueName
    , registerInterface
    , registerType
    , getStateType
    , addCommas
    , widgetType
    , setFocusMethod
    , lookupFocusMethod
    , valNameStr
    , annotateElement
    , elemName
    , declareWidget
    , withField
    )
where

import Control.Applicative
import Control.Monad.State
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as Map
import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators hiding (when)
import Text.PrettyPrint.HughesPJ

import Graphics.Vty.Widgets.Builder.Types

gen :: ElementHandler
gen e@(Elem (N n) _ _) nam = do
  hs <- gets handlers
  case lookup n hs of
    Nothing -> error $ "No handler for element type " ++ (show n)
    Just h -> do
      maybeHr <- h e nam

      when (isJust maybeHr) $
           do
             let Just hr = maybeHr

             -- Always register the name and type of the widget
             -- created by the handler.
             let (wName, wType) = widgetValue hr
             registerType wName wType

             -- If the handler declared a specific name and type for
             -- the a field referencing its value, then register the
             -- name and value.
             when (isJust $ fieldValue hr) $
                  do
                    let Just (fName, fType) = fieldValue hr
                    registerType fName fType

             -- If the element has an ID, use that to set up field
             -- information so we know how to assign the widget to the
             -- field.
             case getAttribute e "id" of
               Nothing -> return ()
               Just newName -> do
                               -- If the handler result contains a
                               -- specific field value name, use that;
                               -- otherwise, fall back to the widget
                               -- value's name.
                               let fieldName = maybe (fst $ widgetValue hr) id (fst <$> fieldValue hr)
                               registerName (RegisteredName newName) fieldName (fst $ widgetValue hr)

      -- Use common attributes on the element to annotate it with
      -- widget-agnostic properties.
      annotateElement e nam
      append $ text ""
      return Nothing
gen _ _ = error "Got unsupported element structure"

-- Using the registered element names in the input document, generate
-- a type with fields for each of the named elements.
generateTypes :: GenState -> Doc
generateTypes st =
    let header = [ text "data InterfaceElements = InterfaceElements {"
                 ]
        footer = [ text "}"
                 ]
        body = elem_lines ++ if_act_lines ++ if_fg_lines
        elem_lines = (flip map) (namedValues st) $ \(fieldName, (valName, _)) ->
                     let typeExpr = case fromJust $ lookup valName $ valueTypes st of
                                      Custom s -> text s
                                      Widget t -> toDoc $ TyCon "Widget" [t]
                     in hcat [ text "elem_"
                             , toDoc fieldName
                             , text " :: "
                             , typeExpr
                             ]
        if_act_lines = (flip map) (interfaceNames st) $ \(ifName, _) ->
                       (text $ "switchTo_" ++ ifName ++ " :: IO ()")
        if_fg_lines = (flip map) (interfaceNames st) $ \(ifName, _) ->
                      (text $ "fg_" ++ ifName ++ " :: Widget FocusGroup")

    in vcat (header ++ [nest 2 $ addCommas body "  "] ++ footer)

widgetType :: [TyCon] -> TyCon
widgetType = TyCon "Widget"

setFocusMethod :: ValueName -> FocusMethod -> GenM ()
setFocusMethod valueName m = do
  st <- get
  put $ st { focusMethods = (valueName, m) : focusMethods st }

lookupFocusMethod :: ValueName -> GenM (Maybe FocusMethod)
lookupFocusMethod valueName = do
  st <- get
  return $ lookup valueName (focusMethods st)

registerType :: ValueName -> Type -> GenM ()
registerType valueName ty = do
  st <- get
  case lookup valueName (valueTypes st) of
    Just _ -> error $ "BUG: type registration for value "
              ++ show valueName
              ++ " happened already!"
    Nothing -> do
      put $ st { valueTypes  = (valueName, ty) : valueTypes st }

valNameStr :: ValueName -> String
valNameStr (ValueName s) = s

getStateType :: ValueName -> GenM TyCon
getStateType valueName = do
  vts <- gets valueTypes
  case lookup valueName vts of
    Nothing -> error $ "BUG: request for state type for value "
               ++ show valueName
               ++ " impossible; did the element handler forget"
               ++ " to register the type?"
    -- XXX: report this error in a better way.
    Just (Custom _) -> error $ "Error: request for widget state " ++
                       "type of non-widget value"
    Just (Widget t) -> return t

registerName :: RegisteredName -> ValueName -> ValueName -> GenM ()
registerName newName fieldValueName widgetValueName = do
  st <- get
  case lookup newName (namedValues st) of
    Just _ -> error "BUG: DTD should have disallowed multiple\
                    \instances of the same element 'name' attribute"
    Nothing -> do
      put $ st { namedValues = (newName, (fieldValueName, widgetValueName)) : namedValues st }

lookupFieldValueName :: RegisteredName -> GenM (Maybe ValueName)
lookupFieldValueName registeredName = do
  val <- lookup registeredName <$> gets namedValues
  return $ fst <$> val

lookupWidgetValueName :: RegisteredName -> GenM (Maybe ValueName)
lookupWidgetValueName registeredName = do
  val <- lookup registeredName <$> gets namedValues
  return $ snd <$> val

getAttribute :: Element a -> String -> Maybe String
getAttribute (Elem _ attrs _) attrName =
    case lookup (N attrName) attrs of
      Just (AttValue ((Left s):_)) -> Just s
      _ -> Nothing

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

annotateElement :: Element a -> ValueName -> GenM ()
annotateElement e nam = do
  -- Normal attribute override
  let normalResult = ( getAttribute e "normalFg"
                     , getAttribute e "normalBg"
                     )
  case attrsToExpr normalResult of
    Nothing -> return ()
    Just expr ->
        append $ hcat [ text "setNormalAttribute "
                      , toDoc nam
                      , text " $ "
                      , text expr
                      ]

  -- Focus attribute override
  let focusResult = ( getAttribute e "focusFg"
                    , getAttribute e "focusBg"
                    )
  case attrsToExpr focusResult of
    Nothing -> return ()
    Just expr ->
        append $ hcat [ text "setFocusAttribute "
                      , toDoc nam
                      , text " $ "
                      , text expr
                      ]

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

newEntry :: String -> GenM ValueName
newEntry n = do
  st <- get

  let val = case Map.lookup n (nameCounters st) of
              Just nextVal -> nextVal
              Nothing -> 1

  let newMap = Map.insert n (val + 1) (nameCounters st)
  put $ st { nameCounters = newMap }

  return $ ValueName $ n ++ show val

addCommas :: [Doc] -> String -> Doc
addCommas [] _ = text ""
addCommas (l:ls) s =
    text s <> l $$ (vcat $ map (text ", " <>) ls)

elemName :: Element a -> String
elemName (Elem (N s) _ _) = s
elemName _ = error "elemName does not support qualified names"

declareWidget :: ValueName -> TyCon -> Maybe HandlerResult
declareWidget val tyCon =
    Just $ HandlerResult { widgetValue = (val, Widget tyCon)
                         , fieldValue = Nothing
                         }

withField :: Maybe HandlerResult -> (ValueName, String) -> Maybe HandlerResult
withField mh (val, ty) =
    mh >>= \h -> return $ h { fieldValue = Just (val, Custom ty) }
