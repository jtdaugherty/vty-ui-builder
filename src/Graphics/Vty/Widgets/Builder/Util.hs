module Graphics.Vty.Widgets.Builder.Util
    ( foreach
    )
where

foreach :: [a] -> (a -> b) -> [b]
foreach = (flip map)