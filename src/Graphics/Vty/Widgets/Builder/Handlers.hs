module Graphics.Vty.Widgets.Builder.Handlers
    ( elementHandlers

    , genInterface
    , genVBox
    , genHBox
    , genFormattedText
    )
where

import Text.PrettyPrint.HughesPJ

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib

elementHandlers :: [(String, ElementHandler a)]
elementHandlers = [ ("interface", genInterface)
                  -- Since the 'interface' DTD specifies some global
                  -- entities for the other DTDs, it MUST come first
                  -- since those entities are *parsed* entities.  See
                  -- also http://www.w3.org/TR/xml/#dt-parsedent
                  , ("fText", genFormattedText)
                  , ("vBox", genVBox)
                  , ("hBox", genHBox)
                  ]

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
