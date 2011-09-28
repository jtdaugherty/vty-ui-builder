module Graphics.Vty.Widgets.Builder.GenLib
    ( gen
    , generateTypes
    , elemChildren
    , getString
    , append
    , newEntry
    , getAttribute
    , attrsToExpr
    , lookupName
    , registerStateType
    , registerInterface
    , getStateType
    )
where

import Control.Applicative
import Control.Monad.State
import Data.List (intersperse)
import Data.Maybe (fromJust)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators
import Text.PrettyPrint.HughesPJ (Doc, text, ($$), vcat, nest)

import Graphics.Vty.Widgets.Builder.Types

gen :: ElementHandler a
gen e@(Elem (N n) _ _) nam = do
  hs <- gets handlers
  case lookup n hs of
    Nothing -> error $ "No handler for element type " ++ (show n)
    Just h -> do
      h e nam
      annotateElement e nam
      case getAttribute e "fieldName" of
        Nothing -> return ()
        Just newName -> registerName newName nam
gen _ _ = error "Got unsupported element structure"

-- Using the registered element names in the input document, generate
-- a type with fields for each of the named elements.
generateTypes :: GenState a -> Doc
generateTypes st =
    let header = [ text "data InterfaceElements = InterfaceElements {"
                 ]
        footer = [ text "}"
                 ]
        body = intersperse (text ", ") (elem_lines ++ if_act_lines)
        elem_lines = (flip map) (namedValues st) $ \(fieldName, valName) ->
                     (text $ "elem_" ++ fieldName ++ " :: Widget (" ++
                               (fromJust $ lookup valName $ valueTypes st) ++ ")")
        if_act_lines = (flip map) (interfaceNames st) $ \(ifName, (valName, actName)) ->
                       (text $ "switchTo_" ++ ifName ++ " :: IO ()")

    in vcat (header ++ [nest 2 $ vcat body] ++ footer)

registerStateType :: String -> String -> GenM a ()
registerStateType valueName typeStr = do
  st <- get
  case lookup valueName (valueTypes st) of
    Just _ -> error $ "BUG: type registration for value '" ++ valueName ++
              "' happened already!"
    Nothing -> do
      put $ st { valueTypes  = (valueName, typeStr) : valueTypes st }

getStateType :: String -> GenM a String
getStateType valueName = do
  vts <- gets valueTypes
  case lookup valueName vts of
    Nothing -> error $ "BUG: request for state type for value '" ++ valueName ++
               "' impossible"
    Just t -> return t

registerName :: String -> String -> GenM a ()
registerName newName valueName = do
  st <- get
  case lookup newName (namedValues st) of
    Just _ -> error "BUG: DTD should have disallowed multiple\
                    \instances of the same element 'name' attribute"
    Nothing -> do
      put $ st { namedValues = (newName, valueName) : namedValues st }

lookupName :: String -> GenM a (Maybe String)
lookupName registeredName = lookup registeredName <$> gets namedValues

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

registerInterface :: String -> String -> String -> GenM a ()
registerInterface ifName valName activateActionName = do
  st <- get
  put $ st { interfaceNames = (ifName, (valName, activateActionName))
                              : interfaceNames st }

annotateElement :: Element a -> String -> GenM a ()
annotateElement e nam = do
  -- Normal attribute override
  let normalResult = ( getAttribute e "normalFg"
                     , getAttribute e "normalBg"
                     )
  case attrsToExpr normalResult of
    Nothing -> return ()
    Just expr ->
        append $ text $ concat [ "setNormalAttribute "
                               , nam
                               , " $ "
                               , expr
                               ]

  -- Focus attribute override
  let focusResult = ( getAttribute e "focusFg"
                    , getAttribute e "focusBg"
                    )
  case attrsToExpr focusResult of
    Nothing -> return ()
    Just expr ->
        append $ text $ concat [ "setFocusAttribute "
                               , nam
                               , " $ "
                               , expr
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

append :: Doc -> GenM a ()
append d = do
  st <- get
  put $ st { genDoc = (genDoc st) $$ d }

newEntry :: GenM a String
newEntry = do
  st <- get
  put $ st { nameCounter = (nameCounter st) + 1
           }
  return $ "val" ++ show (nameCounter st)
