module Main where

import System
import System.IO
import Control.Monad

import System.Exit
import System.Console.GetOpt

import Graphics.Vty.Widgets.Builder
import Graphics.Vty.Widgets.Builder.Config
import Graphics.Vty.Widgets.Builder.DTDGenerator
import Graphics.Vty.Widgets.Builder.Handlers

data BuilderOpt = Help
                | ModuleName String
                | GeneratePreamble Bool
                | GenerateMain Bool
                | GenerateImports Bool
                | OutputFilename String
                | ValidateOnly
                | DTDPath FilePath
                | GenerateInterfaceType Bool
                | GenerateInterfaceBuilder Bool
                  deriving (Show, Eq)

options :: [OptDescr BuilderOpt]
options = [ Option "h" ["help"] (NoArg Help) "This help output"

          , Option "n" ["module-name"] (ReqArg ModuleName "NAME")
                       ("The name of the generated module (default: "
                        ++ (show $ moduleName defaultConfig) ++ ")")

          , Option "p" ["no-preamble"] (NoArg (GeneratePreamble False))
                       "Do not generate a module preamble"

          , Option "i" ["no-imports"] (NoArg (GenerateImports False))
                       "Do not generate vty-ui library imports"

          , Option "t" ["no-type"] (NoArg (GenerateInterfaceType False))
                       ("Do not generate the interface type used to return\n"
                        ++ "interface elements")

          , Option "f" ["no-function"] (NoArg (GenerateInterfaceBuilder False))
                       "Do not generate the function which builds the interface"

          , Option "m" ["main"] (NoArg (GenerateMain True))
                       ("Generate a \"main\" function for testing (implies "
                        ++ "-n \"Main\")")

          , Option "o" ["output"] (ReqArg OutputFilename "FILENAME")
                       "The output filename (default: standard output)"

          , Option "v" ["validate-only"] (NoArg ValidateOnly)
                       "Validate the input XML but do not generate any output"

          , Option "d" ["dtd-dir"] (ReqArg DTDPath "DIR")
                       "Path to the directory containing element DTD files"
          ]

usage :: [String] -> IO ()
usage errs = do
  uh <- usageHeader
  putStrLn $ usageInfo uh options
  dtdDir <- getDTDDir
  putStrLn $ "Default DTD dir: " ++ dtdDir

  mapM_ (putStrLn . ("Error: " ++)) errs

usageHeader :: IO String
usageHeader = do
  progName <- getProgName
  return $ concat [ "Usage: "
                  , progName
                  , " [options] <XML filename>\n"
                  ]

configFromOptions :: [BuilderOpt] -> BuilderConfig
configFromOptions [] = defaultConfig
configFromOptions (o:os) =
    let config = configFromOptions os
    in case o of
         ModuleName s -> config { moduleName = s }
         GeneratePreamble val -> config { generateModulePreamble = val }
         GenerateImports val -> config { generateImports = val }
         GenerateMain val -> config { generateMain = val }
         GenerateInterfaceType val -> config { generateInterfaceType = val }
         GenerateInterfaceBuilder val -> config { generateInterfaceBuilder = val }
         _ -> config

getOutputFilename :: [BuilderOpt] -> Maybe String
getOutputFilename [] = Nothing
getOutputFilename ((OutputFilename s):_) = Just s
getOutputFilename (_:os) = getOutputFilename os

dtdDirFromOpts :: [BuilderOpt] -> IO FilePath
dtdDirFromOpts [] = getDTDDir
dtdDirFromOpts ((DTDPath s):_) = return s
dtdDirFromOpts (_:os) = dtdDirFromOpts os

saveOutput :: [BuilderOpt] -> String -> IO ()
saveOutput opts output = do
  case getOutputFilename opts of
    Nothing -> putStrLn output
    Just path -> do
      h <- openFile path WriteMode `catch`
           \e -> do
             putStrLn $ "Error writing output to " ++ show path ++ ":"
             print e
             exitFailure
      hPutStrLn h output
      hClose h
      putStrLn $ "Output written to " ++ show path

main :: IO ()
main = do
  args <- getArgs
  let (opts, rest, errors) = getOpt Permute options args

  when (not $ null errors) $ do
         usage errors
         exitFailure

  when (Help `elem` opts) $ do
         usage []
         exitSuccess

  when (length rest /= 1) $ usage [] >> exitFailure
  let [xmlFilename] = rest
      config = configFromOptions opts

  inputHandle <- openFile xmlFilename ReadMode `catch`
                 \e -> do
                   putStrLn $ "Error opening " ++ xmlFilename ++ ":"
                   print e
                   exitFailure

  let elementNames = map fst elementHandlers
      handlers = elementHandlers

  dtdPath <- dtdDirFromOpts opts
  validationResult <- validateAgainstDTD inputHandle xmlFilename dtdPath elementNames

  case validationResult of
    Left es -> do
         putStrLn $ "Error validating " ++ (show xmlFilename) ++ ":"
         mapM_ putStrLn es
         exitFailure
    Right e -> do
         when (not (ValidateOnly `elem` opts)) $
              do
                output <- generateSource config e handlers
                saveOutput opts output
