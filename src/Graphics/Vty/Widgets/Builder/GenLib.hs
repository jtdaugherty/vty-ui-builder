module Graphics.Vty.Widgets.Builder.GenLib
    ( gen
    , elemChildren
    , getString
    , append
    , newEntry
    , getAttribute
    , attrsToExpr
    )
where

import Control.Monad.State
import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators
import Text.PrettyPrint.HughesPJ (Doc, text, ($$))

import Graphics.Vty.Widgets.Builder.Types

gen :: ElementHandler a
gen e@(Elem (N n) _ _) nam = do
  hs <- gets handlers
  case lookup n hs of
    Nothing -> error $ "No handler for element type " ++ (show n)
    Just h -> do
      h e nam
      annotateElement e nam
      case getAttribute e "name" of
        Nothing -> return ()
        Just newName -> registerName newName nam
gen _ _ = error "Got unsupported element structure"

registerName :: String -> String -> GenM a ()
registerName newName valueName = do
  st <- get
  case lookup newName (namedValues st) of
    Just _ -> error "BUG: DTD should have disallowed multiple\
                    \instances of the same element 'name' attribute"
    Nothing -> do
      put $ st { namedValues = (newName, valueName) : namedValues st }

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
