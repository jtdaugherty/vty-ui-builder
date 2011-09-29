module Main where

import System
import System.FilePath ((</>))
import Control.Monad

import Paths_vty_ui_builder
import Graphics.Vty.Widgets.Builder
import Graphics.Vty.Widgets.Builder.Config

usage :: String
usage = "builder-test <generated module name> <XML filename>"

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ error usage

  let [modName, xmlFilename] = args

  dataDir <- getDataDir
  let dtdPath = dataDir </> "dtd"

      config = defaultConfig { moduleName = modName
                             , generateMain = True
                             }

  putStrLn =<< generateModuleSource config xmlFilename dtdPath []