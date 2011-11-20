module Graphics.Vty.Widgets.Builder.Reader
    ( DocumentReader(..)
    , readDocument
    )
where

import Graphics.Vty.Widgets.Builder.AST

data DocumentReader =
    DocumentReader { readDoc :: FilePath -> IO (Either [String] Doc)
                   }

readDocument :: DocumentReader -> FilePath -> IO (Either [String] Doc)
readDocument = readDoc