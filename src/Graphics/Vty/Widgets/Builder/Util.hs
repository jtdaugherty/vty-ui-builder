module Graphics.Vty.Widgets.Builder.Util
    ( foreach
    , replace
    , splitOn
    )
where

foreach :: [a] -> (a -> b) -> [b]
foreach = (flip map)

replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ [] = []
replace from_ to_ (e:es) = if e == from_
                           then to_ : replace from_ to_ es
                           else e : replace from_ to_ es

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delim vals = part : parts
    where
      part = takeWhile (/= delim) vals
      parts = splitOn delim $ drop (length part + 1) vals