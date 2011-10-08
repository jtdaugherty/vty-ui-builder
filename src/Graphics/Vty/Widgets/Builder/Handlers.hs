module Graphics.Vty.Widgets.Builder.Handlers
    ( elementHandlers

    , genInterface
    , genVBox
    , genHBox
    , genFormattedText
    )
where

import Control.Monad
import Text.PrettyPrint.HughesPJ
import Text.XML.HaXml.Types

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib

elementHandlers :: [(String, ElementHandler)]
elementHandlers = [ ("collection", genCollection)
                  -- Since the 'collection' DTD specifies some global
                  -- entities for the other DTDs, it MUST come first
                  -- since those entities are *parsed* entities.  See
                  -- also http://www.w3.org/TR/xml/#dt-parsedent
                  , ("interface", genInterface)
                  , ("format", genFormat)
                  , ("fText", genFormattedText)
                  , ("vBox", genVBox)
                  , ("hBox", genHBox)
                  , ("hBorder", genHBorder)
                  , ("vBorder", genVBorder)
                  , ("bordered", genBordered)
                  , ("edit", genEdit)
                  , ("button", genButton)
                  , ("vFill", genVFill)
                  , ("hFill", genHFill)
                  , ("centered", genCentered)
                  , ("hCentered", genHCentered)
                  , ("vCentered", genVCentered)
                  , ("progressBar", genProgressBar)
                  , ("dialog", genDialog)
                  , ("dirBrowser", genDirBrowser)
                  -- This should never be invoked by 'gen', but should
                  -- instead be invoked directly by genInterface.
                  -- It's only here so that the DTD loader loads the
                  -- DTD fragment for this tag.
                  , ("focusGroup", genFocusGroup)
                  ]

genCollection :: ElementHandler
genCollection e nam = do
  -- DTD: two children
  let chs = elemChildren e
  append $ text "c <- newCollection"
  append $ text ""

  forM_ chs $ \ch -> do
    nam <- newEntry
    genInterface ch nam
    annotateElement ch nam

  return nam

genInterface :: ElementHandler
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

  let vals = InterfaceValues { topLevelWidgetName = nam
                             , switchActionName = actName
                             , focusGroupName = fgName
                             }
  registerInterface ifName vals

  return nam

genDirBrowser :: ElementHandler
genDirBrowser e nam = do
  let skin = case getAttribute e "skin" of
               Nothing -> "defaultBrowserSkin"
               Just s -> s

  browserName <- newEntry
  fgName <- newEntry
  append $ hcat [ text "("
                , toDoc browserName
                , text ", "
                , toDoc fgName
                , text ") <- newDirBrowser "
                , text skin
                ]

  append $ hcat [ text "let "
                , toDoc nam
                , text " = dirBrowserWidget "
                , toDoc browserName
                ]

  registerWidgetStateType nam $ TyCon "DirBrowserWidgetType" []
  registerCustomType browserName "DirBrowser"
  setFocusMethod nam $ Merge fgName

  return browserName

genDialog :: ElementHandler
genDialog e nam = do
  let [ch] = elemChildren e
      Just title = getAttribute e "title"

  chNam <- newEntry
  gen ch chNam

  dlgName <- newEntry
  fgName <- newEntry
  append $ hcat [ text "("
                , toDoc dlgName
                , text ", "
                , toDoc fgName
                , text ") <- newDialog "
                , toDoc chNam
                , text " "
                , text $ show title
                ]

  append $ hcat [ text "let "
                , toDoc nam
                , text " = dialogWidget "
                , toDoc dlgName
                ]

  registerWidgetStateType nam $ TyCon "Padded" []
  registerCustomType dlgName "Dialog"
  setFocusMethod nam $ Merge fgName

  return dlgName

genCentered :: ElementHandler
genCentered e nam = do
  let [ch] = elemChildren e

  chNam <- newEntry
  gen ch chNam

  append $ hcat [ toDoc nam
                , text " <- centered "
                , toDoc chNam
                ]

  chType <- getStateType chNam
  registerWidgetStateType nam $ TyCon "Centered" [chType]

  return nam

genHCentered :: ElementHandler
genHCentered e nam = do
  let [ch] = elemChildren e

  chNam <- newEntry
  gen ch chNam

  append $ hcat [ toDoc nam
                , text " <- hCentered "
                , toDoc chNam
                ]

  chType <- getStateType chNam
  registerWidgetStateType nam $ TyCon "HCentered" [chType]

  return nam

genVCentered :: ElementHandler
genVCentered e nam = do
  let [ch] = elemChildren e

  chNam <- newEntry
  gen ch chNam

  append $ hcat [ toDoc nam
                , text " <- vCentered "
                , toDoc chNam
                ]

  chType <- getStateType chNam
  registerWidgetStateType nam $ TyCon "VCentered" [chType]

  return nam

genVFill :: ElementHandler
genVFill e nam = do
  let Just ch = getAttribute e "char"

  when (null ch) $ error "Error: 'char' for 'vFill' must be non-empty"

  append $ hcat [ toDoc nam
                , text $ " <- vFill " ++ (show $ head ch)
                ]

  registerWidgetStateType nam $ TyCon "VFill" []
  return nam

genHFill :: ElementHandler
genHFill e nam = do
  let Just ch = getAttribute e "char"
      Just heightStr = getAttribute e "height"

  when (null ch) $ error "Error: 'char' for 'hFill' must be non-empty"

  height <- case reads heightStr of
              [] -> error "Error: 'height' of 'hFill' must be an integer"
              ((i,_):_) -> return i

  append $ hcat [ toDoc nam
                , text $ " <- hFill " ++ (show $ head ch)
                , text " "
                , text $ show (height :: Int)
                ]

  registerWidgetStateType nam $ TyCon "HFill" []
  return nam

genProgressBar :: ElementHandler
genProgressBar e nam = do
  let Just compColor = getAttribute e "completeColor"
      Just incompColor = getAttribute e "incompleteColor"

  barName <- newEntry

  append $ hcat [ toDoc barName
                , text " <- newProgressBar "
                , text compColor
                , text " "
                , text incompColor
                ]

  append $ hcat [ text "let "
                , toDoc nam
                , text " = progressBarWidget "
                , toDoc barName
                ]

  -- The state type is 'Padded' because buttons are implemented as
  -- composite widgets; see the 'Button' type in
  -- Graphics.Vty.Widgets.Button.
  registerCustomType barName "ProgressBar"
  registerWidgetStateType nam $ TyCon "Box" [ TyCon "HFill" []
                                            , TyCon "HFill" []
                                            ]

  return barName

genButton :: ElementHandler
genButton e nam = do
  let Just label = getAttribute e "label"

  buttonName <- newEntry

  append $ hcat [ toDoc buttonName
                , text " <- newButton "
                , text $ show label
                ]

  append $ hcat [ text "let "
                , toDoc nam
                , text " = buttonWidget "
                , toDoc buttonName
                ]

  -- The state type is 'Padded' because buttons are implemented as
  -- composite widgets; see the 'Button' type in
  -- Graphics.Vty.Widgets.Button.
  registerCustomType buttonName "Button"
  registerWidgetStateType nam $ TyCon "Padded" []

  return buttonName

genEdit :: ElementHandler
genEdit e nam = do
  append $ hcat [ toDoc nam
                , text " <- editWidget"
                ]
  case getAttribute e "contents" of
    Nothing -> return ()
    Just s -> append $ hcat [ text "setEditText "
                            , toDoc nam
                            , text " "
                            , text $ show s
                            ]

  registerWidgetStateType nam $ TyCon "Edit" []

  return nam

genHBorder :: ElementHandler
genHBorder _ nam = do
  append $ hcat [ toDoc nam
                , text " <- hBorder"
                ]
  registerWidgetStateType nam $ TyCon "HBorder" []
  return nam

genVBorder :: ElementHandler
genVBorder _ nam = do
  append $ hcat [ toDoc nam
                , text " <- vBorder"
                ]
  registerWidgetStateType nam $ TyCon "VBorder" []
  return nam

genBordered :: ElementHandler
genBordered e nam = do
  let [ch] = elemChildren e

  chNam <- newEntry
  gen ch chNam

  append $ hcat [ toDoc nam
                , text " <- bordered "
                , toDoc chNam
                ]

  chType <- getStateType chNam
  registerWidgetStateType nam $ TyCon "Bordered" [chType]
  return nam

genVBox :: ElementHandler
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

                  registerWidgetStateType nextName $ TyCon "Box" [c1Type, c2Type]
                  buildVBox (nextName:rest)

  result <- buildVBox names
  registerWidgetStateType nam =<< getStateType result

  append $ hcat [ text "let "
                , toDoc nam
                , text " = "
                , toDoc result
                ]
  return nam

genHBox :: ElementHandler
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

                  registerWidgetStateType nextName $ TyCon "Box" [c1Type, c2Type]
                  buildHBox (nextName:rest)

  result <- buildHBox names
  registerWidgetStateType nam =<< getStateType result

  append $ hcat [ text "let "
                , toDoc nam
                , text " = "
                , toDoc result
                ]
  return nam

genFormat :: ElementHandler
genFormat e nam = do
  let [ch] = elemChildren e
      Just formatName = getAttribute e "name"

  gen ch nam
  tempNam <- newEntry
  append $ toDoc tempNam <> text " <- getTextFormatter " <> toDoc nam
  append $ hcat [ text "setTextFormatter "
                , toDoc nam
                , text " "
                , parens $ hcat [ toDoc tempNam
                                , text " &.& "
                                , text formatName
                                ]
                ]
  return nam

genFormattedText :: ElementHandler
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
      pairExprList = map pairExpr collapsed
      pairExpr (s, expr) = parens $ hcat [ text $ show s
                                         , text ", "
                                         , text expr
                                         ]

  registerWidgetStateType nam $ TyCon "FormattedText" []

  append $ toDoc nam <> text " <- plainText \"\""
  append $ hcat [ text "setTextWithAttrs "
                , toDoc nam
                , text " "
                , vcat [ addCommas pairExprList "[ "
                       , text "]"
                       ]
                ]
  return nam

genFocusGroup :: ElementHandler
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
                result <- lookupWidgetValueName $ RegisteredName registeredName
                case result of
                  Nothing -> error $ "Focus group error: widget name "
                             ++ show registeredName
                             ++ " not found in document"
                  Just valName -> do
                              -- Get the focus method for this value.
                              m <- lookupFocusMethod valName
                              case m of
                                Just (Merge fgName) -> do
                                       append $ text "appendFocusGroup "
                                                  <> toDoc nam
                                                  <> text " "
                                                  <> toDoc fgName
                                -- Covers the Just Direct and Nothing
                                -- cases (default is Direct so
                                -- handlers don't have to register
                                -- focus method unless it's Merge)
                                _ -> do
                                    append $ text "addToFocusGroup "
                                               <> toDoc nam
                                               <> text " "
                                               <> toDoc valName

  return nam
