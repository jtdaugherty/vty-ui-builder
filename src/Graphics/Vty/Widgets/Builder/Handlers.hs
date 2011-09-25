module Graphics.Vty.Widgets.Builder.Handlers
    ( elementHandlers

    , genInterface
    , genVBox
    , genHBox
    , genFormattedText
    )
where

import Control.Monad (forM)
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
  -- DTD: one child
  let [c1] = elemChildren e
  gen c1 nam

genVBox :: ElementHandler a
genVBox e nam = do
  -- DTD: >= 2 children
  names <- forM (elemChildren e) $
           \child -> do
                  chname <- newEntry
                  gen child chname
                  return chname

  let buildVBox [] = error "BUG: vBox cannot be built from zero children"
      buildVBox [c] = return c
      buildVBox (c1:c2:rest) = do
                  nextName <- newEntry
                  append $ text $ nextName ++ " <- vBox " ++ c1 ++ " " ++ c2
                  buildVBox (nextName:rest)

  result <- buildVBox names
  append $ text $ "let " ++ nam ++ " = " ++ result

genHBox :: ElementHandler a
genHBox e nam = do
  -- DTD: >= 2 children
  names <- forM (elemChildren e) $
           \child -> do
                  chname <- newEntry
                  gen child chname
                  return chname

  let buildHBox [] = error "BUG: hBox cannot be built from zero children"
      buildHBox [c] = return c
      buildHBox (c1:c2:rest) = do
                  nextName <- newEntry
                  append $ text $ nextName ++ " <- hBox " ++ c1 ++ " " ++ c2
                  buildHBox (nextName:rest)

  result <- buildHBox names
  append $ text $ "let " ++ nam ++ " = " ++ result

genFormattedText :: ElementHandler a
genFormattedText _ nam = do
  -- TODO: implement scanning child text and attr elements
  append $ text $ nam ++ " <- plainText \"\""
  append $ text $ "setText " ++ nam ++ " \"\""
