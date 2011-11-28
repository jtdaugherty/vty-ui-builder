module Graphics.Vty.Widgets.Builder.Reader
    ( DocumentReader(..)
    , readDocument
    )
where

import Graphics.Vty.Widgets.Builder.AST

data DocumentReader =
    DocumentReader { readDoc :: FilePath
                             -> String
                             -> IO (Either [(String, SourceLocation)] Doc)
                   }

readDocument :: DocumentReader
             -> FilePath
             -> IO (Either [(String, SourceLocation)] Doc)
readDocument r path = readFile path >>= readDoc r path