module Graphics.Vty.Widgets.Builder.Handlers
    ( handleDoc
    , elementHandlers
    )
where

import Control.Applicative
import Control.Monad
import Data.Maybe

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import Graphics.Vty.Widgets.Builder.Util
import qualified Graphics.Vty.Widgets.Builder.AST as A

import Graphics.Vty.Widgets.Box
    ( ChildSizePolicy(..)
    , IndividualPolicy(..)
    )

import qualified Language.Haskell.Exts as Hs

elementHandlers :: [WidgetSpecHandler]
elementHandlers =
    [ handleFormat
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
    , handleCheckBox
    , handleStringList
    , handleList
    , handleVLimit
    , handleHLimit
    , handleBoxLimit
    , handleVFixed
    , handleHFixed
    , handleBoxFixed
    ]

handleDoc :: A.Doc -> GenM ()
handleDoc doc = do
  mapM_ handleParam $ A.documentParams doc
  forM_ (A.documentSharedWidgets doc) $ \spec ->
      do
        nam <- newEntry $ A.widgetType spec
        gen (A.Widget spec) nam

  append $ bind collectionName "newCollection" []

  forM_ (A.documentInterfaces doc) $ \iface ->
      do
        nam <- newEntry "interface"
        handleInterface iface nam

handleInterface :: A.Interface -> Hs.Name -> GenM ()
handleInterface iface nam = do
  gen (A.interfaceContent iface) nam

  actName <- newEntry "act"
  fgName <- newEntry "focusGroup"

  forM_ (A.interfaceFocusEntries iface) $ \w ->
      handleFocusEntry iface w fgName

  append $ bind actName "addToCollection" [ expr collectionName
                                          , expr nam
                                          , expr fgName
                                          ]

  let vals = InterfaceValues { topLevelWidgetName = nam
                             , switchActionName = actName
                             , focusGroupName = fgName
                             }
  registerInterface (A.interfaceName iface) vals

handleParam :: A.Param -> GenM ()
handleParam p =
    registerParam (mkName $ A.paramName p) (parseType $ A.paramType p)

handleCheckBox :: WidgetSpecHandler
handleCheckBox =
    WidgetSpecHandler { generateWidgetSource = genSrc
                         , specType = "checkBox"
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

handleStringList :: WidgetSpecHandler
handleStringList =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "stringList"
                      }
        where
          genSrc e nam = do
            let strs = map getSpecStringContent $ specChildWidgets e
                attrResult = ( getAttribute e "cursorFg"
                             , getAttribute e "cursorBg"
                             )
                attrExpr = case attrsToExpr attrResult of
                             Nothing -> defAttr
                             Just ex -> ex

            append $ bind nam "newStringList" [attrExpr, mkList $ map mkString strs]
            return $ declareWidget nam $ parseType "List String FormattedText"

handleList :: WidgetSpecHandler
handleList =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "list"
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

handleVLimit :: WidgetSpecHandler
handleVLimit =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "vLimit"
                      }
        where
          genSrc e nam = do
            let Just val = getIntAttributeValue =<< getAttribute e "height"

            let [ch] = specChildren e

            chNam <- newEntry (widgetLikeType ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "vLimit" [mkInt val, expr chNam]
            return $ declareWidget nam $
                   parseType $ "VLimit (" ++ Hs.prettyPrint chType ++ ")"

handleHLimit :: WidgetSpecHandler
handleHLimit =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "hLimit"
                      }
        where
          genSrc e nam = do
            let Just val = getIntAttributeValue =<< getAttribute e "width"

            let [ch] = specChildren e

            chNam <- newEntry (widgetLikeType ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "hLimit" [mkInt val, expr chNam]
            return $ declareWidget nam $
                   parseType $ "HLimit (" ++ Hs.prettyPrint chType ++ ")"

handleBoxLimit :: WidgetSpecHandler
handleBoxLimit =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "boxLimit"
                      }
        where
          genSrc e nam = do
            let Just w = getIntAttributeValue =<< getAttribute e "width"
                Just h = getIntAttributeValue =<< getAttribute e "height"

            let [ch] = specChildren e

            chNam <- newEntry (widgetLikeType ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "boxLimit" [mkInt w, mkInt h, expr chNam]
            return $ declareWidget nam $
                   parseType $ "VLimit (HLimit (" ++ Hs.prettyPrint chType ++ "))"

handleVFixed :: WidgetSpecHandler
handleVFixed =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "vFixed"
                      }
        where
          genSrc e nam = do
            let Just val = getIntAttributeValue =<< getAttribute e "height"

            let [ch] = specChildren e

            chNam <- newEntry (widgetLikeType ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "vFixed" [mkInt val, expr chNam]
            return $ declareWidget nam $
                   parseType $ "VFixed (" ++ Hs.prettyPrint chType ++ ")"

handleHFixed :: WidgetSpecHandler
handleHFixed =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "hFixed"
                      }
        where
          genSrc e nam = do
            let Just val = getIntAttributeValue =<< getAttribute e "width"

            let [ch] = specChildren e

            chNam <- newEntry (widgetLikeType ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "hFixed" [mkInt val, expr chNam]
            return $ declareWidget nam $
                   parseType $ "HFixed (" ++ Hs.prettyPrint chType ++ ")"

handleBoxFixed :: WidgetSpecHandler
handleBoxFixed =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "boxFixed"
                      }
        where
          genSrc e nam = do
            let Just w = getIntAttributeValue =<< getAttribute e "width"
                Just h = getIntAttributeValue =<< getAttribute e "height"

            let [ch] = specChildren e

            chNam <- newEntry (widgetLikeType ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "boxFixed" [mkInt w, mkInt h, expr chNam]
            return $ declareWidget nam $
                   parseType $ "VFixed (HFixed (" ++ Hs.prettyPrint chType ++ "))"

handlePad :: WidgetSpecHandler
handlePad =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "pad"
                      }
        where
          genSrc e nam = do
            let [ch] = specChildren e

            chNam <- newEntry (widgetLikeType ch)
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

handleDirBrowser :: WidgetSpecHandler
handleDirBrowser =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "dirBrowser"
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

handleDialog :: WidgetSpecHandler
handleDialog =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "dialog"
                      }
        where
          genSrc e nam = do
            let [ch] = specChildren e
                Just title = getAttribute e "title"

            chNam <- newEntry $ widgetLikeType ch
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

handleCentered :: WidgetSpecHandler
handleCentered =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "centered"
                      }
        where
          genSrc e nam = do
            let [ch] = specChildren e

            chNam <- newEntry $ widgetLikeType ch
            gen ch chNam

            append $ bind nam "centered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (parseType $ "VCentered (HCentered (" ++ Hs.prettyPrint chType ++ "))")

handleHCentered :: WidgetSpecHandler
handleHCentered =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "hCentered"
                      }
        where
          genSrc e nam = do
            let [ch] = specChildren e

            chNam <- newEntry $ widgetLikeType ch
            gen ch chNam

            append $ bind nam "hCentered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (mkTyp "HCentered" [chType])

handleVCentered :: WidgetSpecHandler
handleVCentered =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "vCentered"
                      }
        where
          genSrc e nam = do
            let [ch] = specChildren e

            chNam <- newEntry $ widgetLikeType ch
            gen ch chNam

            append $ bind nam "vCentered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (mkTyp "VCentered" [chType])

handleVFill :: WidgetSpecHandler
handleVFill =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "vFill"
                      }
        where
          genSrc e nam = do
            let Just ch = getAttribute e "char"

            when (null ch) $ error "Error: 'char' for 'vFill' must be non-empty"

            append $ bind nam "vFill" [mkChar $ head ch]

            return $ declareWidget nam (mkTyp "VFill" [])

handleHFill :: WidgetSpecHandler
handleHFill =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "hFill"
                      }
        where
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

handleProgressBar :: WidgetSpecHandler
handleProgressBar =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "progressBar"
                      }
        where
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

handleButton :: WidgetSpecHandler
handleButton =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "button"
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

handleEdit :: WidgetSpecHandler
handleEdit =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "edit"
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

handleHBorder :: WidgetSpecHandler
handleHBorder =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "hBorder"
                      }
        where
          genSrc _ nam = do
            append $ bind nam "hBorder" []
            return $ declareWidget nam (mkTyp "HBorder" [])

handleVBorder :: WidgetSpecHandler
handleVBorder =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "vBorder"
                      }
        where
          genSrc _ nam = do
            append $ bind nam "vBorder" []
            return $ declareWidget nam (mkTyp "VBorder" [])

handleBordered :: WidgetSpecHandler
handleBordered =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "bordered"
                      }
        where
          genSrc e nam = do
            let [ch] = specChildren e

            chNam <- newEntry $ widgetLikeType ch
            gen ch chNam

            append $ bind nam "bordered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (mkTyp "Bordered" [chType])

genBox :: [A.WidgetLike] -> String -> Maybe Int -> Hs.Name -> GenM Hs.Name
genBox es typ spacing rootName = do
  names <- forM es $
           \child -> do
              chname <- newEntry $ widgetLikeType child
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

handleVBox :: WidgetSpecHandler
handleVBox =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "vBox"
                      }
        where
          genSrc e nam = do
            let spacing = getIntAttributeValue =<< getAttribute e "spacing"
            resultName <- genBox (specChildren e) "vBox" spacing nam
            ty <- getWidgetStateType resultName
            return $ declareWidget nam ty

handleBoxSized :: String -> WidgetSpecHandler
handleBoxSized typ =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = typ ++ "-sized"
                      }
        where
          genSrc e nam = do
            let Just boxSize = getBoxSize e
                Hs.ParseOk parsedSizeExpr = Hs.parse $ show boxSize
                spacing = getIntAttributeValue =<< getAttribute e "spacing"

            resultName <- genBox (specChildren e) typ spacing nam
            append $ act $ call "setBoxChildSizePolicy" [ expr nam
                                                        , parsedSizeExpr
                                                        ]
            ty <- getWidgetStateType resultName
            return $ declareWidget nam ty

handleHBoxSized :: WidgetSpecHandler
handleHBoxSized = handleBoxSized "hBox"

handleVBoxSized :: WidgetSpecHandler
handleVBoxSized = handleBoxSized "vBox"

getBoxSize :: A.WidgetSpec -> Maybe ChildSizePolicy
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

handleHBox :: WidgetSpecHandler
handleHBox =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "hBox"
                      }
        where
          genSrc e nam = do
            let spacing = getIntAttributeValue =<< getAttribute e "spacing"
            resultName <- genBox (specChildren e) "hBox" spacing nam
            ty <- getWidgetStateType resultName
            return $ declareWidget nam ty

handleFormat :: WidgetSpecHandler
handleFormat =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "format"
                      }
        where
          genSrc e nam = do
            let [ch] = specChildren e
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

handleFormattedText :: WidgetSpecHandler
handleFormattedText =
    WidgetSpecHandler { generateWidgetSource = genSrc
                      , specType = "fText"
                      }
        where
          genSrc e nam = do
            -- For each entry in the contents list: If it is a string,
            -- give it the default attribute and put it in a list.  If
            -- it is an Attr element, recurse on it, building another
            -- list of (string, attr) to merge.

            -- Left: do not strip whitespace
            -- Right: do strip whitespace
            let processContent :: Hs.Exp -> A.WidgetSpecContent -> [(Either String String, Hs.Exp)]
                processContent ex c =
                    case c of
                      A.Text s _ -> [(Right $ stripWhitespace s, ex)]
                      A.Child (A.Widget w) ->
                          case A.widgetType w of
                            "br" -> [(Left "\n", ex)]
                            "attr" -> processAttr w
                            badName -> error $ "Got unsupported child of attr: " ++ badName
                      A.Child (A.Ref _) -> error "Got unsupported child of attr: reference"

                processAttr :: A.WidgetSpec -> [(Either String String, Hs.Exp)]
                processAttr attr =
                    let attrResult = ( getAttribute attr "fg"
                                     , getAttribute attr "bg"
                                     )
                        attrExpr = case attrsToExpr attrResult of
                                     Nothing -> defAttr
                                     Just ex -> ex
                    in concat $ map (processContent attrExpr) $ A.widgetSpecContents attr

                collapse :: [(Either String String, Hs.Exp)] -> [(Either String String, Hs.Exp)]
                collapse [] = []
                collapse [p] = [p]
                collapse ((Right s1, e1):(Right s2, e2):es) =
                    if e1 == e2
                    then collapse ((Right $ s1 ++ s2, e1) : es)
                    else (Right s1, e1) : collapse ((Right s2, e2):es)
                collapse (p:ps) = p : collapse ps

                pairs :: [(Either String String, Hs.Exp)]
                pairs = concat $ map (processContent defAttr) $ A.widgetSpecContents e

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

handleFocusEntry :: A.Interface -> A.WidgetId -> Hs.Name -> GenM ()
handleFocusEntry iface entryName fgName = do
  case entryName `elem` (getNamedWidgetNames iface) of
      False -> error $ "Focus group error: widget name "
               ++ show entryName
               ++ " not found in interface"
      True -> do
        -- Since we know the name is valid for this interface, this
        -- lookup should always succeed.
        Just wName <- lookupFocusValue (mkName entryName)
        -- Get the focus method for this value.
        m <- lookupFocusMethod $ widgetName wName
        case m of
          Just (Merge fgName') ->
               append $ act $ call "appendFocusGroup" [ expr fgName
                                                      , expr fgName'
                                                      ]
          -- Covers the Just Direct and Nothing cases (default is
          -- Direct so handlers don't have to register focus method
          -- unless it's Merge)
          _ -> append $ act $ call "addToFocusGroup" [ expr fgName
                                                     , expr $ widgetName wName
                                                     ]
