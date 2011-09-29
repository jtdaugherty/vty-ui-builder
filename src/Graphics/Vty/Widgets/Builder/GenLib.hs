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
    , addCommas
    )
where

import Control.Applicative
import Control.Monad.State
import Data.Maybe (fromJust)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators
import Text.PrettyPrint.HughesPJ

import Graphics.Vty.Widgets.Builder.Types

gen :: ElementHandler a
gen e@(Elem (N n) _ _) nam = do
  hs <- gets handlers
  case lookup n hs of
    Nothing -> error $ "No handler for element type " ++ (show n)
    Just h -> do
      h e nam
      annotateElement e nam
      append $ text ""
      case getAttribute e "fieldName" of
        Nothing -> return ()
        Just newName -> registerName (RegisteredName newName) nam
gen _ _ = error "Got unsupported element structure"

-- Using the registered element names in the input document, generate
-- a type with fields for each of the named elements.
generateTypes :: GenState a -> Doc
generateTypes st =
    let header = [ text "data InterfaceElements = InterfaceElements {"
                 ]
        footer = [ text "}"
                 ]
        body = elem_lines ++ if_act_lines ++ if_fg_lines
        elem_lines = (flip map) (namedValues st) $ \(fieldName, valName) ->
                     hcat [ text "elem_"
                          , toDoc fieldName
                          , text " :: "
                          , toDoc $ TyCon "Widget" [fromJust $ lookup valName $ valueTypes st]
                          ]
        if_act_lines = (flip map) (interfaceNames st) $ \(ifName, _) ->
                       (text $ "switchTo_" ++ ifName ++ " :: IO ()")
        if_fg_lines = (flip map) (interfaceNames st) $ \(ifName, _) ->
                      (text $ "fg_" ++ ifName ++ " :: Widget FocusGroup")

    in vcat (header ++ [nest 2 $ addCommas body "  "] ++ footer)

registerStateType :: ValueName -> TyCon -> GenM a ()
registerStateType valueName tyCon = do
  st <- get
  case lookup valueName (valueTypes st) of
    Just _ -> error $ "BUG: type registration for value "
              ++ show valueName
              ++ " happened already!"
    Nothing -> do
      put $ st { valueTypes  = (valueName, tyCon) : valueTypes st }

getStateType :: ValueName -> GenM a TyCon
getStateType valueName = do
  vts <- gets valueTypes
  case lookup valueName vts of
    Nothing -> error $ "BUG: request for state type for value "
               ++ show valueName
               ++ " impossible"
    Just t -> return t

registerName :: RegisteredName -> ValueName -> GenM a ()
registerName newName valueName = do
  st <- get
  case lookup newName (namedValues st) of
    Just _ -> error "BUG: DTD should have disallowed multiple\
                    \instances of the same element 'name' attribute"
    Nothing -> do
      put $ st { namedValues = (newName, valueName) : namedValues st }

lookupName :: RegisteredName -> GenM a (Maybe ValueName)
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

registerInterface :: String -> InterfaceValues -> GenM a ()
registerInterface ifName vals = do
  st <- get
  put $ st { interfaceNames = (ifName, vals)
                              : interfaceNames st }

annotateElement :: Element a -> ValueName -> GenM a ()
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

append :: Doc -> GenM a ()
append d = do
  st <- get
  put $ st { genDoc = (genDoc st) $$ d }

newEntry :: GenM a ValueName
newEntry = do
  st <- get
  put $ st { nameCounter = (nameCounter st) + 1
           }
  return $ ValueName $ "val" ++ show (nameCounter st)

addCommas :: [Doc] -> String -> Doc
addCommas [] _ = text ""
addCommas (l:ls) s =
    text s <> l $$ (vcat $ map (text ", " <>) ls)