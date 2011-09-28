module Graphics.Vty.Widgets.Builder.Handlers
    ( elementHandlers

    , genInterface
    , genVBox
    , genHBox
    , genFormattedText
    )
where

import Control.Monad
import Data.List (intercalate)
import Text.PrettyPrint.HughesPJ
import Text.XML.HaXml.Types

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib

elementHandlers :: [(String, ElementHandler a)]
elementHandlers = [ ("collection", genCollection)
                  -- Since the 'collection' DTD specifies some global
                  -- entities for the other DTDs, it MUST come first
                  -- since those entities are *parsed* entities.  See
                  -- also http://www.w3.org/TR/xml/#dt-parsedent
                  , ("interface", genInterface)
                  , ("fText", genFormattedText)
                  , ("vBox", genVBox)
                  , ("hBox", genHBox)
                  -- This should never be invoked by 'gen', but should
                  -- instead be invoked directly by genInterface.
                  -- It's only here so that the DTD loader loads the
                  -- DTD fragment for this tag.
                  , ("focusGroup", genFocusGroup)
                  ]

genCollection :: ElementHandler a
genCollection e _ = do
  -- DTD: two children
  let chs = elemChildren e
  append $ text "c <- newCollection"
  forM_ chs $ \ch -> do
    nam <- newEntry
    genInterface ch nam

genInterface :: ElementHandler a
genInterface e nam = do
  -- DTD: two children
  let [ch, fg] = elemChildren e
      Just ifName = getAttribute e "name"

  gen ch nam
  actName <- newEntry
  fgName <- newEntry
  genFocusGroup fg fgName
  append $ hcat [ toDoc actName
                , text " <- addToCollection c "
                , toDoc nam
                , text " "
                , toDoc fgName
                ]
  registerInterface ifName nam actName

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
                  append $ hcat [ toDoc nextName
                                , text " <- vBox "
                                , toDoc c1
                                , text " "
                                , toDoc c2
                                ]

                  c1Type <- getStateType c1
                  c2Type <- getStateType c2

                  registerStateType nextName $ TyCon "Box" [c1Type, c2Type]
                  buildVBox (nextName:rest)

  result <- buildVBox names
  registerStateType nam =<< getStateType result

  append $ hcat [ text "let "
                , toDoc nam
                , text " = "
                , toDoc result
                ]

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
                  append $ hcat [ toDoc nextName
                                , text " <- hBox "
                                , toDoc c1
                                , text " "
                                , toDoc c2
                                ]

                  c1Type <- getStateType c1
                  c2Type <- getStateType c2

                  registerStateType nextName $ TyCon "Box" [c1Type, c2Type]
                  buildHBox (nextName:rest)

  result <- buildHBox names
  registerStateType nam =<< getStateType result

  append $ hcat [ text "let "
                , toDoc nam
                , text " = "
                , toDoc result
                ]

genFormattedText :: ElementHandler a
genFormattedText (Elem _ _ eContents) nam = do
  -- For each entry in the contents list: If it is a string, give it
  -- the default attribute and put it in a list.  If it is an Attr
  -- element, recurse on it, building another list of (string, attr)
  -- to merge.

  let processContent expr c =
          case c of
            CString _ cd _ -> [(cd, expr)]
            CElem (Elem (N "br") _ _) _ -> [("\n", expr)]
            CElem attr@(Elem (N "attr") _ _) _ -> processAttr attr
            _ -> error "BUG: fText got unsupported content, should \
                       \have been disallowed by DTD"

      processAttr attr@(Elem _ _ contents) =
          let attrResult = ( getAttribute attr "fg"
                           , getAttribute attr "bg"
                           )
              attrExpr = case attrsToExpr attrResult of
                           Nothing -> "def_attr"
                           Just expr -> expr
          in concat $ map (processContent attrExpr) contents

      collapse [] = []
      collapse [e] = [e]
      collapse ((s1, e1):(s2, e2):es) =
          if e1 == e2
          then collapse ((s1 ++ s2, e1) : es)
          else (s1, e1) : collapse ((s2, e2):es)

      pairs = concat $ mapM (processContent "def_attr") eContents

      collapsed = collapse pairs
      pairListExpr = intercalate ", " $
                     map pairExpr collapsed
      pairExpr (s, expr) = "(" ++ show s ++ ", " ++ expr ++ ")"

  registerStateType nam $ TyCon "FormattedText" []

  append $ toDoc nam <> text " <- plainText \"\""
  append $ hcat [ text "setTextWithAttrs "
                , toDoc nam
                , text " ["
                , text pairListExpr
                , text "]"
                ]

genFocusGroup :: ElementHandler a
genFocusGroup e nam = do
  append $ toDoc nam <> text " <- newFocusGroup"

  -- For each child element of the focus group, resolve it to a named
  -- value and add the specified widget to the focus group.
  forM_ (elemChildren e) $ \ch ->
      do
        let attr = getAttribute ch "name"
        case attr of
          Nothing -> error "BUG: attribute 'name' missing from child of 'focusGroup'; \
                           \DTD should have disallowed this"
          Just registeredName ->
              do
                result <- lookupName $ RegisteredName registeredName
                case result of
                  Nothing -> error $ "Focus group error: widget name "
                             ++ show registeredName
                             ++ " not found in document"
                  Just valName -> append $ text "addToFocusGroup "
                                  <> toDoc nam
                                  <> text " "
                                  <> toDoc valName