module Main where

import System
import System.FilePath ((</>))
import Control.Monad

import Paths_vty_ui_builder
import Graphics.Vty.Widgets.Builder

usage :: String
usage = "builder-test <XML filename>"

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ error usage

  let [xmlFilename] = args

  dataDir <- getDataDir
  let dtdPath = dataDir </> "dtd"

  putStrLn =<< generateModuleSource xmlFilename dtdPath []