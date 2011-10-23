module Graphics.Vty.Widgets.Builder.Handlers
    ( elementHandlers
    )
where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import Graphics.Vty.Widgets.Builder.Util
import Graphics.Vty.Widgets.Builder.ValidateLib

import Graphics.Vty.Widgets.Box
    ( ChildSizePolicy(..)
    , IndividualPolicy(..)
    )

import qualified Language.Haskell.Exts as Hs

elementHandlers :: [ElementHandler]
elementHandlers =
    [ handleCollection
    , handleInterface
    , handleImport
    , handleParams
    , handleShared
    , handleFormat
    , handleFormattedText
    , handleVBox
    , handleHBox
    , handleVBoxSized
    , handleHBoxSized
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
    , handleStringList
    , handleList
    , handleVLimit
    , handleHLimit
    , handleBoxLimit
    ]

handleCollection :: ElementHandler
handleCollection =
    StructureElementHandler { generateStructureSource = genSrc
                            , elementName = "collection"
                            , validator = Nothing
                            }
        where
          genSrc e _ = do
            let chs = elemChildren e
            append $ bind collectionName "newCollection" []

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
        -- The focus group is optional, but even if the input document
        -- doesn't specify one, we need to create one for the
        -- interface and register it as an interface element field.
        let [ch, fg] = case elemChildren e of
                         [e1, e2] -> [e1, e2]
                         [e1] -> [e1, Elem (N "focusGroup") [] []]
                         _ -> error "BUG: invalid interface children"
            Just ifName = getAttribute e "name"

        if' <- lookupInterface ifName
        when (isJust if') $
             error $ "Interface '" ++ ifName ++ "' defined more than once"

        gen ch nam

        actName <- newEntry "act"
        fgName <- newEntry "focusGroup"
        gen fg fgName
        append $ bind actName "addToCollection" [ expr collectionName
                                                , expr nam
                                                , expr fgName
                                                ]

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
                  registerParam (mkName paramId) (parseType paramTyp)

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

handleShared :: ElementHandler
handleShared =
    StructureElementHandler { generateStructureSource = genSrc
                            , elementName = "shared"
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
                rg = getAttribute e "radioGroup"

            append $ bind nam "newCheckbox" [mkString label]

            case rg of
              Nothing -> return ()
              Just rgName -> do
                           -- Ensure that we have a radio group by
                           -- this name.
                           fieldValName <- getFieldValueName $ mkName rgName
                           rgValName <- case fieldValName of
                                          Nothing -> do
                                            rgValName <- newEntry "radioGroup"
                                            append $ bind rgValName "newRadioGroup" []
                                            registerFieldValueName (mkName rgName)
                                                                       (VName $ ValueName rgValName $ parseType "RadioGroup")
                                            return rgValName
                                          Just (VName rgv) -> return $ valueName rgv
                                          Just (WName _) -> error "BUG: radio group field value is a widget value!"

                           -- Generate a statement to add the checkbox
                           -- to the radio group.
                           append $ act $ call "addToRadioGroup" [expr rgValName, expr nam]

            return $ declareWidget nam (mkTyp "CheckBox" [mkTyp "Bool" []])

handleStringList :: ElementHandler
handleStringList =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "stringList"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            let strs = map getElementStringContent $ elemChildren e
                attrResult = ( getAttribute e "cursorFg"
                             , getAttribute e "cursorBg"
                             )
                attrExpr = case attrsToExpr attrResult of
                             Nothing -> defAttr
                             Just ex -> ex

            append $ bind nam "newStringList" [attrExpr, mkList $ map mkString strs]
            return $ declareWidget nam $ parseType "List String FormattedText"

handleList :: ElementHandler
handleList =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "list"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            let attrResult = ( getAttribute e "cursorFg"
                             , getAttribute e "cursorBg"
                             )
                attrExpr = case attrsToExpr attrResult of
                             Nothing -> defAttr
                             Just ex -> ex
                Just keyType = getAttribute e "keyType"
                Just elemType = getAttribute e "elemType"

            append $ bind nam "newList" [attrExpr]
            return $ declareWidget nam $ parseType $ "List (" ++ keyType ++ ") (" ++ elemType ++ ")"

handleVLimit :: ElementHandler
handleVLimit =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "vLimit"
                         , validator = Just doValidate
                         }
        where
          doValidate e = do
            case getAttribute e "height" of
              Nothing -> putError e "'height' attribute required"
              Just val -> do
                           case getIntAttributeValue val of
                             Nothing -> putError e $ "'height' attribute must be an integer"
                             _ -> return ()

          genSrc e nam = do
            let Just val = getIntAttributeValue =<< getAttribute e "height"

            let [ch] = elemChildren e

            chNam <- newEntry (elemName e)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "vLimit" [mkInt val, expr chNam]
            return $ declareWidget nam $
                   parseType $ "VLimit (" ++ Hs.prettyPrint chType ++ ")"

handleHLimit :: ElementHandler
handleHLimit =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "hLimit"
                         , validator = Just doValidate
                         }
        where
          doValidate e = do
            case getAttribute e "width" of
              Nothing -> putError e "'width' attribute required"
              Just val -> do
                           case getIntAttributeValue val of
                             Nothing -> putError e $ "'width' attribute must be an integer"
                             _ -> return ()

          genSrc e nam = do
            let Just val = getIntAttributeValue =<< getAttribute e "width"

            let [ch] = elemChildren e

            chNam <- newEntry (elemName e)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "hLimit" [mkInt val, expr chNam]
            return $ declareWidget nam $
                   parseType $ "HLimit (" ++ Hs.prettyPrint chType ++ ")"

handleBoxLimit :: ElementHandler
handleBoxLimit =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "boxLimit"
                         , validator = Just doValidate
                         }
        where
          doValidate e = do
            case getAttribute e "width" of
              Nothing -> putError e "'width' attribute required"
              Just val -> do
                           case getIntAttributeValue val of
                             Nothing -> putError e $ "'width' attribute must be an integer"
                             _ -> return ()

            case getAttribute e "height" of
              Nothing -> putError e "'height' attribute required"
              Just val -> do
                           case getIntAttributeValue val of
                             Nothing -> putError e $ "'height' attribute must be an integer"
                             _ -> return ()

          genSrc e nam = do
            let Just w = getIntAttributeValue =<< getAttribute e "width"
                Just h = getIntAttributeValue =<< getAttribute e "height"

            let [ch] = elemChildren e

            chNam <- newEntry (elemName e)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "boxLimit" [mkInt w, mkInt h, expr chNam]
            return $ declareWidget nam $
                   parseType $ "VLimit (HLimit (" ++ Hs.prettyPrint chType ++ "))"

handleRef :: ElementHandler
handleRef =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "ref"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            let Just tgt = getAttribute e "target"
                target = mkName tgt
            val <- getFieldValueName target

            case val of
              Nothing -> do
                       result <- isValidParamName target
                       case result of
                         False -> error $ "ref: target '" ++ tgt ++ "' invalid"
                         True -> do
                           typ <- getParamType target
                           append $ mkLet [(nam, expr target)]
                           return $ declareWidget nam typ
              Just (WName valName) -> do
                           append $ mkLet [(nam, expr $ widgetName valName)]
                           typ <- getWidgetStateType $ widgetName valName
                           return $ declareWidget nam typ
              Just (VName _) -> error $ "ref: target '" ++ tgt
                                ++ "' references non-widget type"

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

            let attrNames = ["top", "bottom", "left", "right", "topBottom", "leftRight", "all"]

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

            let padFunctions = [ ("top", "padTop")
                               , ("bottom", "padBottom")
                               , ("left", "padLeft")
                               , ("right", "padRight")
                               , ("leftRight", "padLeftRight")
                               , ("topBottom", "padTopBottom")
                               , ("all", "padAll")
                               ]

                attrNames = fst <$> padFunctions

                -- Get padding attribute values
                paddingValues = foreach attrNames $ \name ->
                                (name, getAttribute e name >>= getIntAttributeValue)

                -- For set padding attributes, extract padding expressions
                paddingExprs :: [Hs.Exp]
                paddingExprs = catMaybes $ foreach padFunctions $ \(attrName, func) -> do
                                 val <- lookup attrName paddingValues
                                 realVal <- val -- since lookup is Maybe (Maybe Int)
                                 return $ call func [mkInt realVal]

            when (null paddingExprs) $
                 error "'pad' element requires at least one padding attribute"

            -- Construct padding expression from values
            let ex = foldl (\e1 e2 -> opApp e1 (mkName "pad") e2)
                     (head paddingExprs) (tail paddingExprs)

            append $ bind nam "padded" [ expr chNam
                                       , parens ex
                                       ]

            return $ declareWidget nam (mkTyp "Padded" [])

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
            bData <- newEntry "browserData"
            append $ bind bData "newDirBrowser" [expr $ mkName skin]

            append $ mkLet [ (nam, call "dirBrowserWidget" [expr browserName])
                           , (browserName, call "fst" [expr bData])
                           , (fgName, call "snd" [expr bData])
                           ]

            mergeFocus nam fgName

            return $ declareWidget nam (mkTyp "DirBrowserWidgetType" [])
                       `withField` (browserName, parseType "DirBrowser")

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

            dlgData <- newEntry "dialogData"
            append $ bind dlgData "newDialog" [ expr chNam
                                              , mkString title
                                              ]
            append $ mkLet [ (nam, call "dialogWidget" [expr dlgName])
                           , (dlgName, call "fst" [expr dlgData])
                           , (fgName, call "snd" [expr dlgData])
                           ]

            mergeFocus nam fgName

            return $ declareWidget nam (parseType "Bordered Padded")
                       `withField` (dlgName, parseType "Dialog")

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

            append $ bind nam "centered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (parseType $ "VCentered (HCentered (" ++ Hs.prettyPrint chType ++ "))")

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

            append $ bind nam "hCentered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (mkTyp "HCentered" [chType])

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

            append $ bind nam "vCentered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (mkTyp "VCentered" [chType])

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

            append $ bind nam "vFill" [mkChar $ head ch]

            return $ declareWidget nam (mkTyp "VFill" [])

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

            append $ bind nam "hFill" [ mkChar $ head ch
                                      , mkInt height
                                      ]

            return $ declareWidget nam (mkTyp "HFill" [])

handleProgressBar :: ElementHandler
handleProgressBar =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "progressBar"
                         , validator = Just doValidate
                         }
        where
          doValidate e = do
            let prog = getAttribute e "progress"

            case prog of
              Nothing -> return ()
              Just val -> case getIntAttributeValue val of
                            Nothing -> putError e "'progress' attribute must be an integer"
                            Just _ -> return ()

          genSrc e nam = do
            let Just compColor = getAttribute e "completeColor"
                Just incompColor = getAttribute e "incompleteColor"
                prog = getAttribute e "progress"

            barName <- newEntry "progressBar"

            append $ bind barName "newProgressBar" [ expr $ mkName compColor
                                                   , expr $ mkName incompColor
                                                   ]
            append $ mkLet [(nam, call "progressBarWidget" [expr barName])]

            case prog of
              Nothing -> return ()
              Just val -> do
                           let Just p = getIntAttributeValue val
                           append $ act $ call "setProgress" [expr barName, mkInt p]

            -- The state type is 'Padded' because buttons are
            -- implemented as composite widgets; see the 'Button' type
            -- in Graphics.Vty.Widgets.Button.
            return $ declareWidget nam (mkTyp "Box" [ mkTyp "HFill" []
                                                    , mkTyp "HFill" []
                                                    ])
                       `withField` (barName, parseType "ProgressBar")

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

            append $ bind buttonName "newButton" [mkString label]
            append $ mkLet [(nam, call "buttonWidget" [expr buttonName])]

            -- The state type is 'Padded' because buttons are
            -- implemented as composite widgets; see the 'Button' type
            -- in Graphics.Vty.Widgets.Button.
            return $ declareWidget nam (mkTyp "Padded" [])
                       `withField` (buttonName, parseType "Button")

handleEdit :: ElementHandler
handleEdit =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "edit"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            append $ bind nam "editWidget" []

            case getAttribute e "contents" of
              Nothing -> return ()
              Just s -> append $ act $ call "setEditText" [ expr nam
                                                          , mkString s
                                                          ]

            return $ declareWidget nam (mkTyp "Edit" [])

handleHBorder :: ElementHandler
handleHBorder =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "hBorder"
                         , validator = Nothing
                         }
        where
          genSrc _ nam = do
            append $ bind nam "hBorder" []
            return $ declareWidget nam (mkTyp "HBorder" [])

handleVBorder :: ElementHandler
handleVBorder =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "vBorder"
                         , validator = Nothing
                         }
        where
          genSrc _ nam = do
            append $ bind nam "vBorder" []
            return $ declareWidget nam (mkTyp "VBorder" [])

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

            append $ bind nam "bordered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (mkTyp "Bordered" [chType])

genBox :: [Element Posn] -> String -> Maybe Int -> Hs.Name -> GenM Hs.Name
genBox es typ spacing rootName = do
  names <- forM es $
           \child -> do
              chname <- newEntry $ elemName child
              gen child chname
              return chname

  let buildBox [] = error "BUG: box cannot be built from zero children"
      buildBox [c] = return c
      buildBox (c1:c2:rest) = do
              nextName <- newEntry typ
              append $ bind nextName typ [ expr c1
                                         , expr c2
                                         ]

              case spacing of
                Nothing -> return ()
                Just val ->
                    append $ act $ call "setBoxSpacing" [ expr nextName
                                                        , mkInt val
                                                        ]

              c1Type <- getWidgetStateType c1
              c2Type <- getWidgetStateType c2

              registerWidgetName $ WidgetName { widgetName = nextName
                                              , widgetType = mkTyp "Box" [c1Type, c2Type]
                                              }
              buildBox (nextName:rest)

  resultName <- buildBox names
  append $ mkLet [(rootName, expr resultName)]
  return resultName

handleVBox :: ElementHandler
handleVBox =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "vBox"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            let spacing = getIntAttributeValue =<< getAttribute e "spacing"
            resultName <- genBox (elemChildren e) "vBox" spacing nam
            ty <- getWidgetStateType resultName
            return $ declareWidget nam ty

handleBoxSized :: String -> ElementHandler
handleBoxSized typ =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = typ ++ "-sized"
                         , validator = Just checkBoxSize
                         }
        where
          genSrc e nam = do
            let Just boxSize = getBoxSize e
                Hs.ParseOk parsedSizeExpr = Hs.parse $ show boxSize
                spacing = getIntAttributeValue =<< getAttribute e "spacing"

            resultName <- genBox (elemChildren e) typ spacing nam
            append $ act $ call "setBoxChildSizePolicy" [ expr nam
                                                        , parsedSizeExpr
                                                        ]
            ty <- getWidgetStateType resultName
            return $ declareWidget nam ty

handleHBoxSized :: ElementHandler
handleHBoxSized = handleBoxSized "hBox"

handleVBoxSized :: ElementHandler
handleVBoxSized = handleBoxSized "vBox"

getBoxSize :: Element Posn -> Maybe ChildSizePolicy
getBoxSize e = getPercentSize <|> getDualSize
    where
      getPercentSize = do
        p <- getAttribute e "percent"
        val <- getIntAttributeValue p
        return $ Percentage val

      getDualSize = do
        firstSize <- getAttribute e "first"
        secondSize <- getAttribute e "second"

        f <- case firstSize of
               "auto" -> return BoxAuto
               val -> BoxFixed <$> getIntAttributeValue val

        s <- case secondSize of
               "auto" -> return BoxAuto
               val -> BoxFixed <$> getIntAttributeValue val

        return $ PerChild f s

checkBoxSize :: ElementValidator
checkBoxSize e = do
  case getBoxSize e of
    Nothing -> do
      putError e "No box size set; set 'percent' or both 'first' and 'second' to integer values"
    Just (Percentage v) -> do
                   when (v >= 100 || v <= 0) $
                        putError e $ "Box percentage value '" ++ show v
                                     ++ "' is invalid, must be between 1 and 99 inclusive"
    _ -> return ()

handleHBox :: ElementHandler
handleHBox =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "hBox"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            let spacing = getIntAttributeValue =<< getAttribute e "spacing"
            resultName <- genBox (elemChildren e) "hBox" spacing nam
            ty <- getWidgetStateType resultName
            return $ declareWidget nam ty

handleFormat :: ElementHandler
handleFormat =
    WidgetElementHandler { generateWidgetSource = genSrc
                         , elementName = "format"
                         , validator = Nothing
                         }
        where
          genSrc e nam = do
            let [ch] = elemChildren e
                Just formatName = getAttribute e "name"

            gen ch nam
            tempNam <- newEntry "formattedText"
            append $ bind tempNam "getTextFormatter" [expr nam]
            append $ act $ call "setTextFormatter" [ expr nam
                                                   , parens (opApp (expr tempNam) (mkSym "&.&") (expr $ mkName formatName))
                                                   ]

            -- NB: this is a no-op because the child element handler
            -- will have already registered a type for 'nam'.
            -- However, we are required to return something from this
            -- handler because it is a widget element handler, so we
            -- just return the same thing the child would have
            -- returned.  Ultimately the name and type declared here
            -- will be ignored because they'll be appended onto the
            -- list of registered widget names and won't be reached by
            -- 'lookup' calls.
            ty <- getWidgetStateType nam
            return $ declareWidget nam ty

defAttr :: Hs.Exp
defAttr = expr $ mkName "def_attr"

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

            -- Left: do not strip whitespace
            -- Right: do strip whitespace
            let processContent :: Hs.Exp -> Content t -> [(Either String String, Hs.Exp)]
                processContent ex c =
                    case c of
                      CString _ cd _ -> [(Right $ stripWhitespace cd, ex)]
                      CElem (Elem (N "br") _ _) _ -> [(Left "\n", ex)]
                      CElem attr@(Elem (N "attr") _ _) _ -> processAttr attr
                      _ -> error "BUG: got unsupported content, should \
                                 \have been disallowed by DTD"

                processAttr :: Element a -> [(Either String String, Hs.Exp)]
                processAttr attr@(Elem _ _ contents) =
                    let attrResult = ( getAttribute attr "fg"
                                     , getAttribute attr "bg"
                                     )
                        attrExpr = case attrsToExpr attrResult of
                                     Nothing -> defAttr
                                     Just ex -> ex
                    in concat $ map (processContent attrExpr) contents

                collapse :: [(Either String String, Hs.Exp)] -> [(Either String String, Hs.Exp)]
                collapse [] = []
                collapse [e] = [e]
                collapse ((Right s1, e1):(Right s2, e2):es) =
                    if e1 == e2
                    then collapse ((Right $ s1 ++ s2, e1) : es)
                    else (Right s1, e1) : collapse ((Right s2, e2):es)
                collapse (e:es) = e : collapse es

                pairs :: [(Either String String, Hs.Exp)]
                pairs = concat $ map (processContent defAttr) eContents

                collapsed :: [(Either String String, Hs.Exp)]
                collapsed = collapse pairs

                pairExpr :: (String, Hs.Exp) -> Hs.Exp
                pairExpr (s, ex) = mkTup [ mkString s
                                         , ex
                                         ]

                isWhitespace = (`elem` " \t\n")
                headTrimmed ((Right s, attr):rest) = (Right $ dropWhile isWhitespace s, attr) : rest
                headTrimmed es = es

                tailTrimmed [] = []
                tailTrimmed ls =
                    init ls ++ case last ls of
                                 (Right s, attr) -> [(Right $ reverse $ dropWhile isWhitespace $ reverse s, attr)]
                                 other -> [other]

                normStrings ls = map (\(s, attr) -> (either id id s, attr)) ls

                pairExprList = map pairExpr $ normStrings $ tailTrimmed $ headTrimmed collapsed

                stripWhitespace :: [Char] -> [Char]
                stripWhitespace (c1:c2:cs) = if isWhitespace c1 && isWhitespace c2
                                             then stripWhitespace (c2:cs)
                                             else c1 : (stripWhitespace (c2:cs))
                stripWhitespace ls = ls

            append $ bind nam "plainTextWithAttrs" [mkList pairExprList]
            return $ declareWidget nam (mkTyp "FormattedText" [])

handleFocusGroup :: ElementHandler
handleFocusGroup =
    StructureElementHandler { generateStructureSource = genSrc
                            , elementName = "focusGroup"
                            , validator = Nothing
                            }
        where
          genSrc e nam = do
            append $ bind nam "newFocusGroup" []

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
                          result <- lookupFocusValue (mkName entryName)
                          case result of
                            Nothing -> error $ "Focus group error: widget name "
                                       ++ show entryName
                                       ++ " not found in document"
                            Just wName -> do
                                        -- Get the focus method for this value.
                                        m <- lookupFocusMethod $ widgetName wName
                                        case m of
                                          Just (Merge fgName) ->
                                              append $ act $ call "appendFocusGroup" [ expr nam
                                                                                     , expr fgName
                                                                                     ]
                                          -- Covers the Just Direct
                                          -- and Nothing cases
                                          -- (default is Direct so
                                          -- handlers don't have to
                                          -- register focus method
                                          -- unless it's Merge)
                                          _ -> append $ act $ call "addToFocusGroup" [ expr nam
                                                                                     , expr $ widgetName wName
                                                                                     ]
