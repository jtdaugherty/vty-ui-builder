module Graphics.Vty.Widgets.Builder.GenLib
    ( gen
    , elemChildren
    , getString
    , append
    , newEntry
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
    Just h -> h e nam >> annotateElement e nam
gen _ _ = error "Got unsupported element structure"

getAttribute :: Element a -> String -> Maybe String
getAttribute (Elem _ attrs _) attrName =
    case lookup (N attrName) attrs of
      Just (AttValue ((Left s):_)) -> Just s
      _ -> Nothing

annotateElement :: Element a -> String -> GenM a ()
annotateElement e nam = do
  -- Normal attribute override
  let normalResult = ( getAttribute e "normalFg"
                     , getAttribute e "normalBg"
                     )
  case normalResult of
    (Nothing, Nothing) -> return ()
    (Just fg, Just bg) ->
        append $ text $ concat [ "setNormalAttribute "
                               , nam
                               , " $ "
                               , fg
                               , " `on` "
                               , bg
                               ]
    (Just fg, Nothing) ->
        append $ text $ concat [ "setNormalAttribute "
                               , nam
                               , " $ fgColor "
                               , fg
                               ]
    (Nothing, Just bg) ->
        append $ text $ concat [ "setNormalAttribute "
                               , nam
                               , " $ bgColor "
                               , bg
                               ]

  -- Focus attribute override
  let focusResult = ( getAttribute e "focusFg"
                    , getAttribute e "focusBg"
                    )
  case focusResult of
    (Nothing, Nothing) -> return ()
    (Just fg, Just bg) ->
        append $ text $ concat [ "setFocusAttribute "
                               , nam
                               , " $ "
                               , fg
                               , " `on` "
                               , bg
                               ]
    (Just fg, Nothing) ->
        append $ text $ concat [ "setFocusAttribute "
                               , nam
                               , " $ fgColor "
                               , fg
                               ]
    (Nothing, Just bg) ->
        append $ text $ concat [ "setFocusAttribute "
                               , nam
                               , " $ bgColor "
                               , bg
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
