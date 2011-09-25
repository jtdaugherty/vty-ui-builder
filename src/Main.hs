module Main where

import Text.XML.HaXml.Parse hiding (doctypedecl)
import Text.XML.HaXml.Validate
import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators hiding (when)

import System
import System.FilePath ((</>))
import Control.Monad
import Control.Monad.State

import Paths_vty_ui_builder

import Text.PrettyPrint.HughesPJ

import Graphics.Vty.Widgets.Builder.DTDGenerator
import Graphics.Vty.Widgets.Builder.Types

usage :: String
usage = "builder-test <XML filename>"

append :: Doc -> GenM a ()
append d = do
  st <- get
  put $ st { genDoc = (genDoc st) $$ d }

newEntry :: GenM a String
newEntry = do
  st <- get
  put $ st { nameCounter = (nameCounter st) + 1
           }
  return $ "val" ++ show (nameCounter st)

gen :: ElementHandler a
gen e@(Elem (N n) _ _) nam = do
  hs <- gets handlers
  case lookup n hs of
    Nothing -> error $ "No handler for element type " ++ (show n)
    Just h -> h e nam
gen _ _ = error "Got unsupported element structure"

elemChildren :: Element a -> [Element a]
elemChildren (Elem _ _ cs) = map getElem contents
    where
      getElem (CElem e _) = e
      getElem _ = error "BUG: getElem got a non-element!"
      contents = concat $ map elm cs

getString :: Content i -> String
getString (CString _ s _) = s
getString _ = error "Cannot get string from non-CString content"

genInterface :: ElementHandler a
genInterface e nam = do
  let [c1] = elemChildren e
  gen c1 nam

genVBox :: ElementHandler a
genVBox e nam = do
  let [c1, c2] = elemChildren e

  c1name <- newEntry
  c2name <- newEntry

  gen c1 c1name
  gen c2 c2name

  append $ text $ nam ++ " <- vBox " ++ c1name ++ " " ++ c2name

genHBox :: ElementHandler a
genHBox e nam = do
  let [c1, c2] = elemChildren e

  c1name <- newEntry
  c2name <- newEntry

  gen c1 c1name
  gen c2 c2name

  append $ text $ nam ++ " <- hBox " ++ c1name ++ " " ++ c2name

genFormattedText :: ElementHandler a
genFormattedText _ nam = do
  append $ text $ nam ++ " <- plainText \"\""
  append $ text $ "setText " ++ nam ++ " \"\""

elementHandlers :: [(String, ElementHandler a)]
elementHandlers = [ ("fText", genFormattedText)
                  , ("vBox", genVBox)
                  , ("hBox", genHBox)
                  , ("interface", genInterface)
                  ]

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ error usage

  let [xmlFilename] = args

  dataDir <- getDataDir
  let dtdPath = dataDir </> "dtd"

  masterDTD <- generateMasterDTD elementHandlers dtdPath
  dtd <- case dtdParse' "<generated>" masterDTD of
           Right (Just dtd) -> return dtd
           Right Nothing -> error "No DTD found in generated DTD text!"
           Left e -> error $ "Error parsing generated DTD: " ++ e

  xmlContents <- readFile xmlFilename
  case xmlParse' xmlFilename xmlContents of
    Left e -> putStrLn e

    Right (Document _ _ e _) -> do
         case partialValidate dtd e of
           [] -> do
             let (_, st') = runState (gen e "root") (GenState 0 empty elementHandlers)
             putStrLn $ render $ genDoc st'
           es -> mapM_ putStrLn es
