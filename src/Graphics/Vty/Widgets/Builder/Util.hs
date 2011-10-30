module Graphics.Vty.Widgets.Builder.Util
    ( foreach
    , elemName
    , replace
    )
where

import Text.XML.HaXml.Types

foreach :: [a] -> (a -> b) -> [b]
foreach = (flip map)

elemName :: Element a -> String
elemName (Elem (N s) _ _) = s
elemName _ = error "elemName does not support qualified names"

replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace from_ to_ (e:es) = if e == from_
                           then to_ : replace from_ to_ es
                           else e : replace from_ to_ es
