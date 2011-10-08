module Graphics.Vty.Widgets.Builder.DTDGenerator
    ( generateMasterDTD
    , getDTDDir
    )
where

import System.Directory
import System.FilePath ((</>))
import Data.List (intercalate)
import Paths_vty_ui_builder

getDTDDir :: IO FilePath
getDTDDir = do
  dataDir <- getDataDir
  canonicalizePath $ dataDir </> "dtd"

mkAttList :: String -> String
mkAttList nam = concat [ "<!ATTLIST "
                       , nam
                       , " normalFg (%fgcolor;) #IMPLIED"
                       , " normalBg (%bgcolor;) #IMPLIED"
                       , " focusFg (%fgcolor;) #IMPLIED"
                       , " focusBg (%bgcolor;) #IMPLIED"
                       , " id ID #IMPLIED"
                       , ">\n"
                       ]

mkLoadFragment :: String -> FilePath -> String
mkLoadFragment n path = concat [ "<!ENTITY % load" ++ n
                               , " SYSTEM \"" ++ path ++ "\">\n"
                               , "%load" ++ n ++ ";\n"
                               ]

generateMasterDTD :: [String] -> FilePath -> IO String
generateMasterDTD names dtdPath = do
  let attLists = map mkAttList names
      allEntity = "<!ENTITY % all \"" ++ (intercalate "|" names) ++ "\">\n"
      dtdLines = [ "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
                 , allEntity
                 ]
                 ++ map (\n -> mkLoadFragment n (dtdPath </> n ++ ".dtd")) names
                 ++ attLists

  return $ concat dtdLines
