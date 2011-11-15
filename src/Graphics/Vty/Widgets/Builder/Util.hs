module Graphics.Vty.Widgets.Builder.Util
    ( foreach
    , replace
    )
where

foreach :: [a] -> (a -> b) -> [b]
foreach = (flip map)

replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace from_ to_ (e:es) = if e == from_
                           then to_ : replace from_ to_ es
                           else e : replace from_ to_ es
