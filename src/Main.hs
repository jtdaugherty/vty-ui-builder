module Main where

import Text.XML.HaXml.Parse hiding (doctypedecl)
import Text.XML.HaXml.Validate
import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators hiding (when)

import System
import Control.Monad
import Control.Monad.State
import Data.List (intercalate)

import Text.PrettyPrint.HughesPJ

usage :: String
usage = "builder-test <XML filename>"

data GenState = GenState { nameCounter :: Int
                         , genDoc :: Doc
                         }
                deriving (Show)

type GenM a = State GenState a

append :: Doc -> GenM ()
append d = do
  st <- get
  put $ st { genDoc = (genDoc st) $$ d }

newEntry :: GenM String
newEntry = do
  st <- get
  put $ st { nameCounter = (nameCounter st) + 1
           }
  return $ "val" ++ show (nameCounter st)

handlers :: [(String, Element a -> GenM String)]
handlers = [ ("fText", genFormattedText)
           , ("vBox", genVBox)
           , ("hBox", genHBox)
           , ("interface", genInterface)
           ]

gen :: Element a -> GenM String
gen e@(Elem (N n) _ _) = do
  case lookup n handlers of
    Nothing -> error $ "No handler for element type " ++ (show n)
    Just h -> h e
gen _ = error "Got unsupported element structure"

getString :: Content i -> String
getString (CString _ s _) = s
getString _ = error "Cannot get string from non-CString content"

genInterface :: Element a -> GenM String
genInterface (Elem _ _ contents) = do
  let [CElem c1 _] = concat $ map elm contents
  gen c1

genVBox :: Element a -> GenM String
genVBox (Elem _ _ contents) = do
  nam <- newEntry

  let [CElem c1 _, CElem c2 _] = concat $ map elm contents

  c1name <- gen c1
  c2name <- gen c2

  append $ text $ nam ++ " <- vBox " ++ c1name ++ " " ++ c2name
  return nam

genHBox :: Element a -> GenM String
genHBox (Elem _ _ contents) = do
  nam <- newEntry

  let [CElem c1 _, CElem c2 _] = concat $ map elm contents

  c1name <- gen c1
  c2name <- gen c2

  append $ text $ nam ++ " <- hBox " ++ c1name ++ " " ++ c2name
  return nam

genFormattedText :: Element a -> GenM String
genFormattedText (Elem _ _ _) = do
  nam <- newEntry

  append $ text $ nam ++ " <- plainText \"\""
  append $ text $ "setText " ++ nam ++ " \"\""

  return nam

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ error usage

  let [xmlFilename] = args
      dtdFragments = ["vBox", "hBox", "fText"]
      dtdStr = concat ([ "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
                       , "<!ENTITY % all \"" ++ (intercalate "|" dtdFragments) ++ "\">\n"
                       ]
                       ++ map mkLoadFragment dtdFragments
                       ++ ["<!ELEMENT interface (%all;)>"]
                      )
      mkLoadFragment n = concat [ "<!ENTITY % load" ++ n ++ " SYSTEM \"dtd/" ++ n ++ ".dtd\">\n"
                                , "%load" ++ n ++ ";\n"
                                ]

  let Right (Just dtd) = dtdParse' "<generated>" dtdStr

  xmlContents <- readFile xmlFilename

  case xmlParse' xmlFilename xmlContents of
    Left e -> putStrLn e

    Right (Document _ _ e _) -> do
         let errors = partialValidate dtd e
         case errors of
           [] -> do
             let (_, st') = runState (gen e) (GenState 0 empty)
             putStrLn $ render $ genDoc st'
           _ -> mapM_ putStrLn errors
