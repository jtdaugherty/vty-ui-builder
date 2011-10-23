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
    , handleInterface
    , handleImport
    , handleParams
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

collectionName :: String
collectionName = "c"

handleCollection :: ElementHandler
handleCollection =
    StructureElementHandler { generateStructureSource = genSrc
                            , elementName = "collection"
                            , validator = Nothing
                            }
        where
          genSrc e _ = do
            let chs = elemChildren e
            append $ collectionName >- " <- newCollection"
            append $ text ""

            forM_ chs $ \ch -> do
                        nam <- newEntry $ elemName ch
                        gen ch nam

handleInterface :: ElementHandler
handleInterface =
    StructureElementHandler { generateStructureSource = genSrc
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
        append $ actName >- " <- addToCollection "
                   >- collectionName >- " " >- nam
                   >- " " >- fgName

        let vals = InterfaceValues { topLevelWidgetName = nam
                                   , switchActionName = actName
                                   , focusGroupName = fgName
                                   }
        registerInterface ifName vals

handleParams :: ElementHandler
handleParams =
    StructureElementHandler { generateStructureSource = genSrc
                            , elementName = "params"
                            , validator = Nothing
                            }
        where
          genSrc e _ = do
            forM_ (elemChildren e) $ \ch ->
                do
                  let Just paramId = getAttribute ch "name"
                      Just paramTyp = getAttribute ch "type"
                  registerParam paramId paramTyp

handleImport :: ElementHandler
handleImport =
    StructureElementHandler { generateStructureSource = genSrc
                            , elementName = "import"
                            , validator = Nothing
                            }
        where
          genSrc e _ = do
            let Just name = getAttribute e "module"
            addImport name

handleCommon :: ElementHandler
handleCommon =
    StructureElementHandler { generateStructureSource = genSrc
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

handleCheckBox :: ElementHandler
handleCheckBox =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "checkBox"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            let Just label = getAttribute e "label"
            append $ nam >- " <- newCheckbox " >- show label
            return $ declareWidget nam (TyCon "CheckBox" [TyCon "Bool" []])

handleRef :: ElementHandler
handleRef =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "ref"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            let Just target = getAttribute e "target"
            val <- lookupWidgetName target

            case val of
              Nothing -> do
                       result <- isValidParamName target
                       case result of
                         False -> error $ "ref: target '" ++ target ++ "' invalid"
                         True -> do
                           typ <- getParamType target
                           append $ hcat [ text "let "
                                         , toDoc nam
                                         , text " = "
                                         , text target
                                         ]
                           return $ declareWidget nam $ mkTyCon typ
              Just valName -> do
                           append $ "let " >- nam >- " = " >- valName
                           typ <- getWidgetStateType $ widgetName valName
                           return $ declareWidget nam typ

handlePad :: ElementHandler
handlePad =
    WidgetElementHandler { generateWidgetSource = genSrc
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

            chType <- getWidgetStateType chNam

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

            append $ nam >- " <- padded " >- (parens $ text expr)
                       >- " " >- chNam

            return $ declareWidget nam (TyCon "Padded" [chType])

handleDirBrowser :: ElementHandler
handleDirBrowser =
    WidgetElementHandler { generateWidgetSource = genSrc
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
            append $ "(" >- browserName >- ", " >- fgName
                       >- ") <- newDirBrowser " >- skin

            append $ "let " >- nam >- " = dirBrowserWidget "
                       >- browserName

            mergeFocus nam fgName

            return $ declareWidget nam (TyCon "DirBrowserWidgetType" [])
                       `withField` (browserName, "DirBrowser")

handleDialog :: ElementHandler
handleDialog =
    WidgetElementHandler { generateWidgetSource = genSrc
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

            append $ parens (dlgName >- ", " >- fgName)
                       >- " <- newDialog " >- chNam >- " " >- show title
            append $ "let " >- nam >- " = dialogWidget " >- dlgName

            mergeFocus nam fgName

            return $ declareWidget nam (TyCon "Padded" [])
                       `withField` (dlgName, "Dialog")

handleCentered :: ElementHandler
handleCentered =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "centered"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            let [ch] = elemChildren e

            chNam <- newEntry $ elemName ch
            gen ch chNam

            append $ nam >- " <- centered " >- chNam

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (TyCon "Centered" [chType])

handleHCentered :: ElementHandler
handleHCentered =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "hCentered"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            let [ch] = elemChildren e

            chNam <- newEntry $ elemName ch
            gen ch chNam

            append $ nam >- " <- hCentered " >- chNam

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (TyCon "HCentered" [chType])

handleVCentered :: ElementHandler
handleVCentered =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "vCentered"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            let [ch] = elemChildren e

            chNam <- newEntry $ elemName ch
            gen ch chNam

            append $ nam >- " <- vCentered " >- chNam

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (TyCon "VCentered" [chType])

handleVFill :: ElementHandler
handleVFill =
    WidgetElementHandler { generateWidgetSource = genSrc
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

            append $ nam >- " <- vFill " >- (show $ head ch)

            return $ declareWidget nam (TyCon "VFill" [])

handleHFill :: ElementHandler
handleHFill =
    WidgetElementHandler { generateWidgetSource = genSrc
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

            append $ nam >- " <- hFill " >- (show $ head ch)
                       >- " " >- (show (height :: Int))

            return $ declareWidget nam (TyCon "HFill" [])

handleProgressBar :: ElementHandler
handleProgressBar =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "progressBar"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            let Just compColor = getAttribute e "completeColor"
                Just incompColor = getAttribute e "incompleteColor"

            barName <- newEntry "progressBar"

            append $ barName >- " <- newProgressBar " >- compColor
                       >- " " >- incompColor
            append $ "let " >- nam >- " = progressBarWidget " >- barName

            -- The state type is 'Padded' because buttons are
            -- implemented as composite widgets; see the 'Button' type
            -- in Graphics.Vty.Widgets.Button.
            return $ declareWidget nam (TyCon "Box" [ TyCon "HFill" []
                                                    , TyCon "HFill" []
                                                    ])
                       `withField` (barName, "ProgressBar")

handleButton :: ElementHandler
handleButton =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "button"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            let Just label = getAttribute e "label"

            buttonName <- newEntry "button"

            append $ buttonName >- " <- newButton " >- show label
            append $ "let " >- nam >- " = buttonWidget " >- buttonName

            -- The state type is 'Padded' because buttons are
            -- implemented as composite widgets; see the 'Button' type
            -- in Graphics.Vty.Widgets.Button.
            return $ declareWidget nam (TyCon "Padded" [])
                       `withField` (buttonName, "Button")

handleEdit :: ElementHandler
handleEdit =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "edit"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            append $ nam >- " <- editWidget"

            case getAttribute e "contents" of
              Nothing -> return ()
              Just s -> append $ "setEditText " >- nam >- " "
                        >- show s

            return $ declareWidget nam (TyCon "Edit" [])

handleHBorder :: ElementHandler
handleHBorder =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "hBorder"
                         , validator = Nothing
                         }
        where
          genSrc _ nam = do
            append $ nam >- " <- hBorder"
            return $ declareWidget nam (TyCon "HBorder" [])

handleVBorder :: ElementHandler
handleVBorder =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "vBorder"
                         , validator = Nothing
                         }
        where
          genSrc _ nam = do
            append $ nam >- " <- vBorder"
            return $ declareWidget nam (TyCon "VBorder" [])

handleBordered :: ElementHandler
handleBordered =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "bordered"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            let [ch] = elemChildren e

            chNam <- newEntry $ elemName ch
            gen ch chNam

            append $ nam >- " <- bordered " >- chNam

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (TyCon "Bordered" [chType])

handleVBox :: ElementHandler
handleVBox =
    WidgetElementHandler { generateWidgetSource = genSrc
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
                           append $ nextName >- " <- vBox " >- c1
                                      >- " " >- c2

                           c1Type <- getWidgetStateType c1
                           c2Type <- getWidgetStateType c2

                           registerWidgetName $ WidgetName { widgetName = nextName
                                                           , widgetType = TyCon "Box" [c1Type, c2Type]
                                                           }
                           buildVBox (nextName:rest)

            result <- buildVBox names
            append $ "let " >- nam >- " = " >- result

            ty <- getWidgetStateType result
            return $ declareWidget nam ty

handleHBox :: ElementHandler
handleHBox =
    WidgetElementHandler { generateWidgetSource = genSrc
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
                           append $ nextName >- " <- hBox " >- c1
                                      >- " " >- c2

                           c1Type <- getWidgetStateType c1
                           c2Type <- getWidgetStateType c2

                           registerWidgetName $ WidgetName { widgetName = nextName
                                                           , widgetType = TyCon "Box" [c1Type, c2Type]
                                                           }
                           buildHBox (nextName:rest)

            result <- buildHBox names
            append $ "let " >- nam >- " = " >- result

            ty <- getWidgetStateType result
            return $ declareWidget nam ty

handleFormat :: ElementHandler
handleFormat =
    StructureElementHandler { generateStructureSource = genSrc
                            , elementName = "format"
                            , validator = Nothing
                            }
        where
          genSrc e nam = do
            let [ch] = elemChildren e
                Just formatName = getAttribute e "name"

            gen ch nam
            tempNam <- newEntry "formattedText"
            append $ tempNam >- " <- getTextFormatter " >- nam
            append $ "setTextFormatter " >- nam >- " "
                       >- parens (tempNam >- " &.& " >- formatName)

handleFormattedText :: ElementHandler
handleFormattedText =
    WidgetElementHandler { generateWidgetSource = genSrc
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
                pairExpr (s, expr) = parens $ (show s) >- ", " >- expr

            append $ nam >- " <- plainText \"\""
            append $ "setTextWithAttrs " >- nam >- " "
                       >- vcat [ addCommas pairExprList "[ "
                               , text "]"
                               ]

            return $ declareWidget nam (TyCon "FormattedText" [])

handleFocusGroup :: ElementHandler
handleFocusGroup =
    StructureElementHandler { generateStructureSource = genSrc
                            , elementName = "focusGroup"
                            , validator = Nothing
                            }
        where
          genSrc e nam = do
            append $ nam >- " <- newFocusGroup"

            -- For each child element of the focus group, resolve it
            -- to a named value and add the specified widget to the
            -- focus group.
            forM_ (elemChildren e) $ \ch ->
                do
                  let attr = getAttribute ch "name"
                  case attr of
                    Nothing -> error "BUG: attribute 'name' missing from child of 'focusGroup'; \
                                     \DTD should have disallowed this"
                    Just entryName ->
                        do
                          result <- lookupFocusValue entryName
                          case result of
                            Nothing -> error $ "Focus group error: widget name "
                                       ++ show entryName
                                       ++ " not found in document"
                            Just wName -> do
                                        -- Get the focus method for this value.
                                        m <- lookupFocusMethod $ widgetName wName
                                        case m of
                                          Just (Merge fgName) ->
                                              append $ "appendFocusGroup " >- nam >- " " >- fgName
                                          -- Covers the Just Direct
                                          -- and Nothing cases
                                          -- (default is Direct so
                                          -- handlers don't have to
                                          -- register focus method
                                          -- unless it's Merge)
                                          _ -> append $ "addToFocusGroup " >- nam >- " " >- wName
