module Graphics.Vty.Widgets.Builder.DTDGenerator
    ( generateMasterDTD
    )
where

import System.FilePath ((</>))
import Data.List (intercalate)

import Graphics.Vty.Widgets.Builder.Types

mkAttList :: String -> String
mkAttList nam = concat [ "<!ATTLIST "
                       , nam
                       , " normalFg (%fgcolor;) #IMPLIED"
                       , " normalBg (%bgcolor;) #IMPLIED"
                       , " focusFg (%fgcolor;) #IMPLIED"
                       , " focusBg (%bgcolor;) #IMPLIED"
                       , " fieldName ID #IMPLIED"
                       , ">\n"
                       ]

mkLoadFragment :: String -> FilePath -> String
mkLoadFragment n path = concat [ "<!ENTITY % load" ++ n
                               , " SYSTEM \"" ++ path ++ "\">\n"
                               , "%load" ++ n ++ ";\n"
                               ]

generateMasterDTD :: [(String, ElementHandler a)] -> FilePath -> IO String
generateMasterDTD hs dtdPath = do
  let names = map fst hs
      attLists = map mkAttList names
      allEntity = "<!ENTITY % all \"" ++ (intercalate "|" names) ++ "\">\n"
      dtdLines = [ "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
                 , allEntity
                 ]
                 ++ map (\n -> mkLoadFragment n (dtdPath </> n ++ ".dtd")) names
                 ++ attLists

  return $ concat dtdLines
