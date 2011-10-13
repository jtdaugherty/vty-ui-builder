module Graphics.Vty.Widgets.Builder.Handlers
    ( elementHandlers
    )
where

import Control.Applicative
import Control.Monad
import Data.List (intercalate)
import Data.Maybe
import Text.PrettyPrint.HughesPJ
import Text.XML.HaXml.Types

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import Graphics.Vty.Widgets.Builder.Util
import Graphics.Vty.Widgets.Builder.ValidateLib

elementHandlers :: [ElementHandler]
elementHandlers =
    [ handleCollection
    -- Since the 'collection' DTD specifies some global entities for
    -- the other DTDs, it MUST come first since those entities are
    -- *parsed* entities.  See also
    -- http://www.w3.org/TR/xml/#dt-parsedent
    , handleInterface
    , handleCommon
    , handleFormat
    , handleFormattedText
    , handleVBox
    , handleHBox
    , handleHBorder
    , handleVBorder
    , handleBordered
    , handleEdit
    , handleButton
    , handleVFill
    , handleHFill
    , handleCentered
    , handleHCentered
    , handleVCentered
    , handleProgressBar
    , handleDialog
    , handleDirBrowser
    , handlePad
    , handleRef
    , handleCheckBox
    , handleFocusGroup
    ]

collectionName :: ValueName
collectionName = ValueName "c"

handleCollection :: ElementHandler
handleCollection =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = False
                   , elementName = "collection"
                   , validator = Nothing
                   }
        where
          genSrc e _ = do
            let chs = elemChildren e
            append $ hcat [ toDoc collectionName
                          , text " <- newCollection"
                          ]
            append $ text ""

            forM_ chs $ \ch -> do
                        nam <- newEntry $ elemName ch
                        gen ch nam

            return Nothing

handleInterface :: ElementHandler
handleInterface =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = False
                   , elementName = "interface"
                   , validator = Nothing
                   }
    where
      genSrc e nam = do
        -- DTD: two children
        let [ch, fg] = elemChildren e
            Just ifName = getAttribute e "name"

        gen ch nam
        actName <- newEntry "act"
        fgName <- newEntry "focusGroup"
        gen fg fgName
        append $ hcat [ toDoc actName
                      , text " <- addToCollection "
                      , toDoc collectionName
                      , text " "
                      , toDoc nam
                      , text " "
                      , toDoc fgName
                      ]

        let vals = InterfaceValues { topLevelWidgetName = nam
                                   , switchActionName = actName
                                   , focusGroupName = fgName
                                   }
        registerInterface ifName vals
        return Nothing

handleCommon :: ElementHandler
handleCommon =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = False
                   , elementName = "common"
                   , validator = Just doValidate
                   }
        where
          doValidate e = do
            forM_ (elemChildren e) $ \ch ->
                do
                  when (isNothing $ getAttribute ch "id") $
                       putError e $ "element '" ++ elemName ch
                                    ++ "' missing 'id' attribute"

          genSrc e _ = do
            forM_ (elemChildren e) $ \ch ->
                do
                  chNam <- newEntry $ elemName ch
                  gen ch chNam

            return Nothing

handleCheckBox :: ElementHandler
handleCheckBox =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "checkBox"
                   , validator = Nothing
                   }
        where
          genSrc e nam = do
            let Just label = getAttribute e "label"
            append $ hcat [ toDoc nam
                          , text " <- newCheckbox "
                          , text $ show label
                          ]
            return $ declareWidget nam (TyCon "CheckBox" [TyCon "Bool" []])

handleRef :: ElementHandler
handleRef =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "ref"
                   , validator = Nothing
                   }
        where
          genSrc e nam = do
            let Just target = getAttribute e "target"
            val <- lookupWidgetValueName $ RegisteredName target

            case val of
              Nothing -> error $ "ref: target '" ++ target ++ "' invalid"
              Just valName -> do
                           append $ hcat [ text "let "
                                         , toDoc nam
                                         , text " = "
                                         , toDoc valName
                                         ]
                           (return . declareWidget nam) =<< getStateType valName

handlePad :: ElementHandler
handlePad =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "pad"
                   , validator = Just doValidate
                   }
        where
          doValidate e@(Elem _ attrs _) = do
            when (length attrs == 0) $
                 putError e $ "at least one padding attribute must be specified"

            let attrNames = ["top", "bottom", "left", "right"]

            forM_ attrNames $ \attr -> do
              let val = getAttribute e attr
              case (val, val >>= getIntAttributeValue) of
                (Just _, Nothing) ->
                    putError e $ "attribute '" ++ attr ++ "' must be an integer"
                _ -> return ()

          genSrc e nam = do
            let [ch] = elemChildren e

            chNam <- newEntry (elemName e)
            gen ch chNam

            chType <- getStateType chNam

            let padFunctions = [ ("top", "padTop")
                               , ("bottom", "padBottom")
                               , ("left", "padLeft")
                               , ("right", "padRight")
                               ]

                attrNames = fst <$> padFunctions

                -- Get padding attribute values
                paddingValues = foreach attrNames $ \name ->
                                (name, getAttribute e name >>= getIntAttributeValue)

                -- For set padding attributes, extract padding expressions
                paddingExprs :: [String]
                paddingExprs = catMaybes $ foreach padFunctions $ \(attrName, func) -> do
                                 val <- lookup attrName paddingValues
                                 return $ func ++ " " ++ (show val)

            when (null paddingExprs) $
                 error "'pad' element requires at least one padding attribute"

            -- Construct padding expression from values
            let expr = intercalate " `pad` " paddingExprs

            append $ hcat [ toDoc nam
                          , text " <- padded "
                          , parens $ text expr
                          , text " "
                          , toDoc chNam
                          ]

            return $ declareWidget nam (TyCon "Padded" [chType])

handleDirBrowser :: ElementHandler
handleDirBrowser =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "dirBrowser"
                   , validator = Nothing
                   }
        where
          genSrc e nam = do
            let skin = case getAttribute e "skin" of
                         Nothing -> "defaultBrowserSkin"
                         Just s -> s

            browserName <- newEntry "browser"
            fgName <- newEntry "focusGroup"
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

            setFocusMethod nam $ Merge fgName

            return $ declareWidget nam (TyCon "DirBrowserWidgetType" [])
                       `withField` (browserName, "DirBrowser")

handleDialog :: ElementHandler
handleDialog =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "dialog"
                   , validator = Nothing
                   }
        where
          genSrc e nam = do
            let [ch] = elemChildren e
                Just title = getAttribute e "title"

            chNam <- newEntry $ elemName ch
            gen ch chNam

            dlgName <- newEntry "dialog"
            fgName <- newEntry "focusGroup"
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

            setFocusMethod nam $ Merge fgName

            return $ declareWidget nam (TyCon "Padded" [])
                       `withField` (dlgName, "Dialog")

handleCentered :: ElementHandler
handleCentered =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "centered"
                   , validator = Nothing
                   }
        where
          genSrc e nam = do
            let [ch] = elemChildren e

            chNam <- newEntry $ elemName ch
            gen ch chNam

            append $ hcat [ toDoc nam
                          , text " <- centered "
                          , toDoc chNam
                          ]

            chType <- getStateType chNam
            return $ declareWidget nam (TyCon "Centered" [chType])

handleHCentered :: ElementHandler
handleHCentered =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "hCentered"
                   , validator = Nothing
                   }
        where
          genSrc e nam = do
            let [ch] = elemChildren e

            chNam <- newEntry $ elemName ch
            gen ch chNam

            append $ hcat [ toDoc nam
                          , text " <- hCentered "
                          , toDoc chNam
                          ]

            chType <- getStateType chNam
            return $ declareWidget nam (TyCon "HCentered" [chType])

handleVCentered :: ElementHandler
handleVCentered =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "vCentered"
                   , validator = Nothing
                   }
        where
          genSrc e nam = do
            let [ch] = elemChildren e

            chNam <- newEntry $ elemName ch
            gen ch chNam

            append $ hcat [ toDoc nam
                          , text " <- vCentered "
                          , toDoc chNam
                          ]

            chType <- getStateType chNam
            return $ declareWidget nam (TyCon "VCentered" [chType])

handleVFill :: ElementHandler
handleVFill =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "vFill"
                   , validator = Just doValidate
                   }
        where
          doValidate e = do
            let Just ch = getAttribute e "char"
            when (null ch) $ putError e "attribute 'char' must be non-empty"

          genSrc e nam = do
            let Just ch = getAttribute e "char"

            when (null ch) $ error "Error: 'char' for 'vFill' must be non-empty"

            append $ hcat [ toDoc nam
                          , text $ " <- vFill " ++ (show $ head ch)
                          ]

            return $ declareWidget nam (TyCon "VFill" [])

handleHFill :: ElementHandler
handleHFill =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "hFill"
                   , validator = Just doValidate
                   }
        where
          doValidate e = do
            let Just ch = getAttribute e "char"
                Just h = getAttribute e "height"

            when (null ch) $ putError e "attribute 'char' must be non-empty"

            case getIntAttributeValue h of
              Nothing -> putError e "attribute 'height' must be an integer"
              Just _ -> return ()

          genSrc e nam = do
            let Just ch = getAttribute e "char"
                Just heightStr = getAttribute e "height"

            when (null ch) $ error "Error: 'char' for 'hFill' must be non-empty"

            height <- case getIntAttributeValue heightStr of
                        Nothing -> error "Error: 'height' of 'hFill' must be an integer"
                        Just i -> return i

            append $ hcat [ toDoc nam
                          , text $ " <- hFill " ++ (show $ head ch)
                          , text " "
                          , text $ show (height :: Int)
                          ]

            return $ declareWidget nam (TyCon "HFill" [])

handleProgressBar :: ElementHandler
handleProgressBar =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "progressBar"
                   , validator = Nothing
                   }
        where
          genSrc e nam = do
            let Just compColor = getAttribute e "completeColor"
                Just incompColor = getAttribute e "incompleteColor"

            barName <- newEntry "progressBar"

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

            -- The state type is 'Padded' because buttons are
            -- implemented as composite widgets; see the 'Button' type
            -- in Graphics.Vty.Widgets.Button.
            return $ declareWidget nam (TyCon "Box" [ TyCon "HFill" []
                                                    , TyCon "HFill" []
                                                    ])
                       `withField` (barName, "ProgressBar")

handleButton :: ElementHandler
handleButton =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "button"
                   , validator = Nothing
                   }
        where
          genSrc e nam = do
            let Just label = getAttribute e "label"

            buttonName <- newEntry "button"

            append $ hcat [ toDoc buttonName
                          , text " <- newButton "
                          , text $ show label
                          ]

            append $ hcat [ text "let "
                          , toDoc nam
                          , text " = buttonWidget "
                          , toDoc buttonName
                          ]

            -- The state type is 'Padded' because buttons are
            -- implemented as composite widgets; see the 'Button' type
            -- in Graphics.Vty.Widgets.Button.
            return $ declareWidget nam (TyCon "Padded" [])
                       `withField` (buttonName, "Button")

handleEdit :: ElementHandler
handleEdit =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "edit"
                   , validator = Nothing
                   }
        where
          genSrc e nam = do
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

            return $ declareWidget nam (TyCon "Edit" [])

handleHBorder :: ElementHandler
handleHBorder =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "hBorder"
                   , validator = Nothing
                   }
        where
          genSrc _ nam = do
            append $ hcat [ toDoc nam
                          , text " <- hBorder"
                          ]
            return $ declareWidget nam (TyCon "HBorder" [])

handleVBorder :: ElementHandler
handleVBorder =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "vBorder"
                   , validator = Nothing
                   }
        where
          genSrc _ nam = do
            append $ hcat [ toDoc nam
                          , text " <- vBorder"
                          ]
            return $ declareWidget nam (TyCon "VBorder" [])

handleBordered :: ElementHandler
handleBordered =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "bordered"
                   , validator = Nothing
                   }
        where
          genSrc e nam = do
            let [ch] = elemChildren e

            chNam <- newEntry $ elemName ch
            gen ch chNam

            append $ hcat [ toDoc nam
                          , text " <- bordered "
                          , toDoc chNam
                          ]

            chType <- getStateType chNam
            return $ declareWidget nam (TyCon "Bordered" [chType])

handleVBox :: ElementHandler
handleVBox =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "vBox"
                   , validator = Nothing
                   }
        where
          genSrc e nam = do
            -- DTD: >= 2 children
            names <- forM (elemChildren e) $
                     \child -> do
                           chname <- newEntry $ elemName child
                           gen child chname
                           return chname

            let buildVBox [] = error "BUG: vBox cannot be built from zero children"
                buildVBox [c] = return c
                buildVBox (c1:c2:rest) = do
                           nextName <- newEntry "vBox"
                           append $ hcat [ toDoc nextName
                                         , text " <- vBox "
                                         , toDoc c1
                                         , text " "
                                         , toDoc c2
                                         ]

                           c1Type <- getStateType c1
                           c2Type <- getStateType c2

                           registerType nextName (Widget (TyCon "Box" [c1Type, c2Type]))
                           buildVBox (nextName:rest)

            result <- buildVBox names

            append $ hcat [ text "let "
                          , toDoc nam
                          , text " = "
                          , toDoc result
                          ]

            ty <- getStateType result
            return $ declareWidget nam ty

handleHBox :: ElementHandler
handleHBox =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "hBox"
                   , validator = Nothing
                   }
        where
          genSrc e nam = do
            -- DTD: >= 2 children
            names <- forM (elemChildren e) $
                     \child -> do
                           chname <- newEntry $ elemName child
                           gen child chname
                           return chname

            let buildHBox [] = error "BUG: hBox cannot be built from zero children"
                buildHBox [c] = return c
                buildHBox (c1:c2:rest) = do
                           nextName <- newEntry "hBox"
                           append $ hcat [ toDoc nextName
                                         , text " <- hBox "
                                         , toDoc c1
                                         , text " "
                                         , toDoc c2
                                         ]

                           c1Type <- getStateType c1
                           c2Type <- getStateType c2

                           registerType nextName (Widget (TyCon "Box" [c1Type, c2Type]))
                           buildHBox (nextName:rest)

            result <- buildHBox names

            append $ hcat [ text "let "
                          , toDoc nam
                          , text " = "
                          , toDoc result
                          ]

            ty <- getStateType result
            return $ declareWidget nam ty

handleFormat :: ElementHandler
handleFormat =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = False
                   , elementName = "format"
                   , validator = Nothing
                   }
        where
          genSrc e nam = do
            let [ch] = elemChildren e
                Just formatName = getAttribute e "name"

            gen ch nam
            tempNam <- newEntry "formattedText"
            append $ toDoc tempNam <> text " <- getTextFormatter " <> toDoc nam
            append $ hcat [ text "setTextFormatter "
                          , toDoc nam
                          , text " "
                          , parens $ hcat [ toDoc tempNam
                                          , text " &.& "
                                          , text formatName
                                          ]
                          ]
            return Nothing

handleFormattedText :: ElementHandler
handleFormattedText =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = True
                   , elementName = "fText"
                   , validator = Nothing
                   }
        where
          genSrc (Elem _ _ eContents) nam = do
            -- For each entry in the contents list: If it is a string,
            -- give it the default attribute and put it in a list.  If
            -- it is an Attr element, recurse on it, building another
            -- list of (string, attr) to merge.

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

            append $ toDoc nam <> text " <- plainText \"\""
            append $ hcat [ text "setTextWithAttrs "
                          , toDoc nam
                          , text " "
                          , vcat [ addCommas pairExprList "[ "
                                 , text "]"
                                 ]
                          ]

            return $ declareWidget nam (TyCon "FormattedText" [])

handleFocusGroup :: ElementHandler
handleFocusGroup =
    ElementHandler { generateSource = genSrc
                   , isWidgetElement = False
                   , elementName = "focusGroup"
                   , validator = Nothing
                   }
        where
          genSrc e nam = do
            append $ toDoc nam <> text " <- newFocusGroup"

            -- For each child element of the focus group, resolve it
            -- to a named value and add the specified widget to the
            -- focus group.
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
                                          -- Covers the Just Direct
                                          -- and Nothing cases
                                          -- (default is Direct so
                                          -- handlers don't have to
                                          -- register focus method
                                          -- unless it's Merge)
                                          _ -> append $ text "addToFocusGroup "
                                                 <> toDoc nam
                                                 <> text " "
                                                 <> toDoc valName

            return Nothing
