module Graphics.Vty.Widgets.Builder.Reader
    ( DocumentReader(..)
    , readDocument
    )
where

import Graphics.Vty.Widgets.Builder.AST

data DocumentReader =
    DocumentReader { checkFormat :: FilePath -> IO Bool
                   , readDoc :: FilePath -> IO (Either [String] Doc)
                   }

readDocument :: [DocumentReader] -> FilePath -> IO (Either [String] Doc)
readDocument [] _ = return $ Left ["No document readers were provided."]
readDocument (r:rs) path = do
  correctFormat <- checkFormat r path
  if correctFormat then
      readDoc r path else
      readDocument rs path