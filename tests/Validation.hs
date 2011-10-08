module Validation
    ( getValidationTests
    )
where

import System.IO
import System.FilePath
import System.Directory
import Control.Monad
import Data.List

import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Graphics.Vty.Widgets.Builder
import Graphics.Vty.Widgets.Builder.Handlers
import Graphics.Vty.Widgets.Builder.DTDGenerator

data ExpectedResult = Failure [String]
                    | Success
                      deriving (Eq, Show)

data ValidationTest =
    ValidationTest { inputDocument :: String
                   , description :: String
                   , expectedResult :: ExpectedResult
                   , testFilename :: FilePath
                   }
    deriving (Eq, Show)

testCaseDelim :: String
testCaseDelim = "---"

getValidationTestDir :: IO FilePath
getValidationTestDir = do
  cur <- getCurrentDirectory
  canonicalizePath $ cur </> "tests" </> "validation"

getValidationTestFilenames :: IO [FilePath]
getValidationTestFilenames = do
  files <- getDirectoryContents =<< getValidationTestDir
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

parseValidationTest :: FilePath -> String -> Maybe ValidationTest
parseValidationTest filename input = do
  let theLines = lines input
      sections = partitionBy (cleanup theLines) testCaseDelim
  case sections of
    [desc, inputDoc, expected] ->
        do
          r <- parseExpectedResult expected
          return $ ValidationTest { description = intercalate " " desc
                                  , inputDocument = unlines inputDoc
                                  , expectedResult = r
                                  , testFilename = filename
                                  }
    _ -> Nothing

readValidationTest :: FilePath -> IO ValidationTest
readValidationTest filename = do
  contents <- readFile filename
  case parseValidationTest filename contents of
    Nothing -> error $ "Cannot parse test case " ++ filename
    Just t -> return t

matchingFailures :: [String] -> [String] -> Bool
matchingFailures expectedEs actualEs =
    and $ map checkActual expectedEs
        where
          checkActual ('^':expected) = or $ map (expected `isPrefixOf`) actualEs
          checkActual expected = expected `elem` actualEs

runValidationTest :: ValidationTest -> IO ()
runValidationTest tc = do
  tmpdir <- getTemporaryDirectory
  (filename, handle) <- openTempFile tmpdir "testcase.tmp"
  hPutStrLn handle (inputDocument tc)
  hClose handle

  let elementNames = map fst elementHandlers
  dtdPath <- getDTDDir

  h <- openFile filename ReadMode
  result <- validateAgainstDTD h filename dtdPath elementNames

  case (result, expectedResult tc) of
    (Left es, Success) ->
        do
          assertFailure $ concat [ "validation failed but should have succeeded.\n"
                                 , intercalate "\n" es
                                 ]
    (Right _, Failure es) ->
        do
          assertFailure $ concat [ "validation succeeded but should have failed with the following errors:\n"
                                 , intercalate "\n" es
                                 ]
    (Left actualEs, Failure expectedEs) ->
        do
          when (not $ matchingFailures expectedEs actualEs) $
               assertFailure $ concat [ "Error: validation failed as expected, but with the wrong specifics.\n"
                                      , "Expected:\n"
                                      , intercalate "\n" expectedEs
                                      , "Actual:\n"
                                      , intercalate "\n" actualEs
                                      ]
    (Right _, Success) -> return ()

getValidationTests :: IO Test
getValidationTests = do
  base <- getValidationTestDir
  caseFiles <- getValidationTestFilenames
  tests <- forM caseFiles $
           \filename -> readValidationTest (base </> filename)

  let mkTestCase tc = testCase (description tc) (runValidationTest tc)

  return $ testGroup "dtd-validation" (map mkTestCase tests)
