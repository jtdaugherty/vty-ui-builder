module Graphics.Vty.Widgets.Builder.DTDGenerator
    ( generateMasterDTD
    , getDTDDir
    )
where

import System.Directory
import System.FilePath ((</>))
import Data.List (intercalate)
import Paths_vty_ui_builder

import Graphics.Vty.Widgets.Builder.Types

getDTDDir :: IO FilePath
getDTDDir = do
  dataDir <- getDataDir
  canonicalizePath $ dataDir </> "dtd"

colorNames :: [String]
colorNames = [ "red"
             , "green"
             , "yellow"
             , "blue"
             , "magenta"
             , "cyan"
             , "white"
             , "black"
             , "bright_red"
             , "bright_green"
             , "bright_yellow"
             , "bright_black"
             , "bright_magenta"
             , "bright_cyan"
             , "bright_white"
             , "bright_blue"
             ]

commonEntities :: String
commonEntities = concat [ "<!ENTITY % fgcolor \"" ++ (intercalate "|" colorNames) ++ "\">\n"
                        , "<!ENTITY % bgcolor \"" ++ (intercalate "|" colorNames) ++ "\">\n"
                        ]

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

generateMasterDTD :: [(FilePath, [ElementHandler])] -> IO String
generateMasterDTD elementInfo = do
  let attLists = map mkAttList widgetNames
      widgetNames = map elementName $ filter isWidgetElement allHandlers
      allHandlers = concat $ map snd elementInfo

      loadFragments = concat $ map mkLoadFragments elementInfo

      mkLoadFragments (dtdPath, elemHandlers) =
          let names = map elementName elemHandlers
          in map (\n -> mkLoadFragment n (dtdPath </> n ++ ".dtd")) names

      allEntity = "<!ENTITY % all \"" ++ (intercalate "|" widgetNames) ++ "\">\n"
      dtdLines = [ "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
                 , commonEntities
                 , allEntity
                 ]
                 ++ loadFragments
                 ++ attLists

  return $ concat dtdLines
