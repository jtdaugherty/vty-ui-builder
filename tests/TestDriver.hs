module Main where

import System.IO
import System.Exit
import System.FilePath
import System.Directory
import Control.Monad
import Data.List

import Graphics.Vty.Widgets.Builder
import Graphics.Vty.Widgets.Builder.Handlers
import Graphics.Vty.Widgets.Builder.DTDGenerator

data ExpectedResult = Failure [String]
                    | Success
                      deriving (Eq, Show)

data TestCase =
    TestCase { inputDocument :: String
             , description :: String
             , expectedResult :: ExpectedResult
             , testFilename :: FilePath
             }
    deriving (Eq, Show)

testCaseDelim :: String
testCaseDelim = "---"

getTestCaseDir :: IO FilePath
getTestCaseDir = do
  cur <- getCurrentDirectory
  canonicalizePath $ cur </> "tests" </> "cases"

getTestCaseFilenames :: IO [FilePath]
getTestCaseFilenames = do
  files <- getDirectoryContents =<< getTestCaseDir
  return [ f | f <- files, ".test" `isSuffixOf` f ]

partitionBy :: (Eq a) => [a] -> a -> [[a]]
partitionBy stuff d = partitionBy' stuff d []
    where
      partitionBy' [] _ cur = if null cur
                              then []
                              else [cur]
      partitionBy' (e:es) delim cur =
          if e == delim
          then [cur] ++ partitionBy' es delim []
          else partitionBy' es delim (cur ++ [e])

cleanup :: [String] -> [String]
cleanup theLines = filter (not . ("#" `isPrefixOf`)) theLines

parseExpectedResult :: [String] -> Maybe ExpectedResult
parseExpectedResult [] = Nothing
parseExpectedResult ["success"] = Just Success
parseExpectedResult ls = Just $ Failure ls

parseTestCase :: FilePath -> String -> Maybe TestCase
parseTestCase filename input = do
  let theLines = lines input
      sections = partitionBy (cleanup theLines) testCaseDelim
  case sections of
    [desc, inputDoc, expected] ->
        do
          r <- parseExpectedResult expected
          return $ TestCase { description = intercalate " " desc
                            , inputDocument = unlines inputDoc
                            , expectedResult = r
                            , testFilename = filename
                            }
    _ -> Nothing

readTestCase :: FilePath -> IO TestCase
readTestCase filename = do
  contents <- readFile filename
  case parseTestCase filename contents of
    Nothing -> error $ "Cannot parse test case " ++ filename
    Just t -> return t

matchingFailures :: [String] -> [String] -> Bool
matchingFailures expectedEs actualEs =
    and $ map checkActual expectedEs
        where
          checkActual ('^':expected) = or $ map (expected `isPrefixOf`) actualEs
          checkActual expected = expected `elem` actualEs

runTestCase :: TestCase -> IO ()
runTestCase testCase = do
  let shortName = takeFileName $ testFilename testCase
  putStrLn $ shortName ++ " (" ++ (description testCase) ++ ")"

  tmpdir <- getTemporaryDirectory
  (filename, handle) <- openTempFile tmpdir "testcase.tmp"
  hPutStrLn handle (inputDocument testCase)
  hClose handle

  let elementNames = map fst elementHandlers
  dtdPath <- getDTDDir

  h <- openFile filename ReadMode
  result <- validateAgainstDTD h filename dtdPath elementNames

  case (result, expectedResult testCase) of
    (Left es, Success) ->
        do
          putStrLn "Error: validation failed but should have succeeded.  Errors were:"
          mapM_ putStrLn es
          exitFailure
    (Right _, Failure es) ->
        do
          putStrLn "Error: validation succeeded but should have failed.  Expected errors were:"
          mapM_ putStrLn es
          exitFailure
    (Left actualEs, Failure expectedEs) ->
        do
          when (not $ matchingFailures expectedEs actualEs) $
               do
                 putStrLn "Error: validation failed as expected, but with the wrong specifics."
                 putStrLn "Expected:"
                 mapM_ putStrLn expectedEs
                 putStrLn "Actual:"
                 mapM_ putStrLn actualEs
                 exitFailure
    (Right _, Success) -> return ()

main :: IO ()
main = do
  base <- getTestCaseDir
  cases <- getTestCaseFilenames

  forM_ cases $ \filename -> do
         testCase <- readTestCase (base </> filename)
         runTestCase testCase