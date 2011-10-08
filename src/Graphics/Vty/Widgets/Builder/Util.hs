module Graphics.Vty.Widgets.Builder.Util
    ( foreach
    , elemName
    )
where

import Text.XML.HaXml.Types

foreach :: [a] -> (a -> b) -> [b]
foreach = (flip map)

elemName :: Element a -> String
elemName (Elem (N s) _ _) = s
elemName _ = error "elemName does not support qualified names"