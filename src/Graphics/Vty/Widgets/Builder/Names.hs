module Graphics.Vty.Widgets.Builder.Names
    ( collectionName
    , uiElementsName
    )
where

import qualified Language.Haskell.Exts as Hs

collectionName :: Hs.Name
collectionName = Hs.Ident "c"

uiElementsName :: Hs.Name
uiElementsName = Hs.Ident "elems"

