{-# OPTIONS_GHC -Werror  #-}
module Graphics.Vty.Widgets.Builder.Handlers
    ( handleDoc
    , coreSpecHandlers
    )
where

import Control.Applicative hiding (optional)
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

coreSpecHandlers :: [WidgetSpecHandler]
coreSpecHandlers =
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
        handleInterface iface doc nam

handleInterface :: A.Interface -> A.Doc -> Hs.Name -> GenM ()
handleInterface iface doc nam = do
  gen (A.interfaceContent iface) nam

  actName <- newEntry "act"
  fgName <- newEntry "focusGroup"

  append $ bind fgName "newFocusGroup" []

  forM_ (A.interfaceFocusEntries iface) $ \w ->
      handleFocusEntry iface doc w fgName

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
    WidgetSpecHandler genSrc doValidate "checkBox"

        where
          doValidate s = (,)
                         <$> required s "label"
                         <*> optional s "radioGroup"

          genSrc _ nam (label, rg) = do
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
    WidgetSpecHandler genSrc doValidate "stringList"

        where
          doValidate s = (,)
                         <$> optional s "cursorFg"
                         <*> optional s "cursorBg"

          genSrc e nam (cursorFg, cursorBg) = do
            let strs = map getSpecStringContent $ specChildWidgets e
                attrExpr = case attrsToExpr (cursorFg, cursorBg) of
                             Nothing -> defAttr
                             Just ex -> ex

            append $ bind nam "newStringList" [attrExpr, mkList $ map mkString strs]
            return $ declareWidget nam $ parseType "List String FormattedText"

handleList :: WidgetSpecHandler
handleList =
    WidgetSpecHandler genSrc doValidate "list"

        where
          doValidate s = (,,,)
                         <$> optional s "cursorFg"
                         <*> optional s "cursorBg"
                         <*> required s "keyType"
                         <*> required s "elemType"

          genSrc _ nam (cursorFg, cursorBg, keyType, elemType) = do
            let attrExpr = case attrsToExpr (cursorFg, cursorBg) of
                             Nothing -> defAttr
                             Just ex -> ex

            append $ bind nam "newList" [attrExpr]
            return $ declareWidget nam $ parseType $ "List (" ++ keyType ++
                       ") (" ++ elemType ++ ")"

handleVLimit :: WidgetSpecHandler
handleVLimit =
    WidgetSpecHandler genSrc doValidate "vLimit"

        where
          doValidate s = (,)
                         <$> requiredInt s "height"
                         <*> firstChild s

          genSrc _ nam (height, ch) = do
            chNam <- newEntry (widgetLikeType ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "vLimit" [mkInt height, expr chNam]
            return $ declareWidget nam $
                   parseType $ "VLimit (" ++ Hs.prettyPrint chType ++ ")"

handleHLimit :: WidgetSpecHandler
handleHLimit =
    WidgetSpecHandler genSrc doValidate "hLimit"
        where
          doValidate s = (,)
                         <$> requiredInt s "width"
                         <*> firstChild s

          genSrc _ nam (width, ch) = do
            chNam <- newEntry (widgetLikeType ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "hLimit" [mkInt width, expr chNam]
            return $ declareWidget nam $
                   parseType $ "HLimit (" ++ Hs.prettyPrint chType ++ ")"

handleBoxLimit :: WidgetSpecHandler
handleBoxLimit =
    WidgetSpecHandler genSrc doValidate "boxLimit"
        where
          doValidate s = (,,)
                         <$> (requiredInt s "width")
                         <*> (requiredInt s "height")
                         <*> firstChild s

          genSrc _ nam (width, height, ch) = do
            chNam <- newEntry (widgetLikeType ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "boxLimit" [ mkInt width
                                         , mkInt height
                                         , expr chNam
                                         ]
            return $ declareWidget nam $
                   parseType $ "VLimit (HLimit (" ++ Hs.prettyPrint chType ++ "))"

handleVFixed :: WidgetSpecHandler
handleVFixed =
    WidgetSpecHandler genSrc doValidate "vFixed"
        where
          doValidate s = (,)
                         <$> requiredInt s "height"
                         <*> firstChild s

          genSrc _ nam (height, ch) = do
            chNam <- newEntry (widgetLikeType ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "vFixed" [mkInt height, expr chNam]
            return $ declareWidget nam $
                   parseType $ "VFixed (" ++ Hs.prettyPrint chType ++ ")"

handleHFixed :: WidgetSpecHandler
handleHFixed =
    WidgetSpecHandler genSrc doValidate "hFixed"
        where
          doValidate s = (,)
                         <$> requiredInt s "width"
                         <*> firstChild s

          genSrc _ nam (width, ch) = do
            chNam <- newEntry (widgetLikeType ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "hFixed" [mkInt width, expr chNam]
            return $ declareWidget nam $
                   parseType $ "HFixed (" ++ Hs.prettyPrint chType ++ ")"

handleBoxFixed :: WidgetSpecHandler
handleBoxFixed =
    WidgetSpecHandler genSrc doValidate "boxFixed"
        where
          doValidate s = (,,)
                         <$> requiredInt s "width"
                         <*> requiredInt s "height"
                         <*> firstChild s

          genSrc _ nam (width, height, ch) = do
            chNam <- newEntry (widgetLikeType ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "boxFixed" [mkInt width, mkInt height, expr chNam]
            return $ declareWidget nam $
                   parseType $ "VFixed (HFixed (" ++ Hs.prettyPrint chType ++ "))"

data PadInfo = PadInfo { padTop :: Maybe Int
                       , padBottom :: Maybe Int
                       , padLeft :: Maybe Int
                       , padRight :: Maybe Int
                       , padLeftRight :: Maybe Int
                       , padTopBottom :: Maybe Int
                       , padAll :: Maybe Int
                       }
             deriving (Eq)

noPadding :: PadInfo
noPadding = PadInfo Nothing Nothing Nothing Nothing Nothing Nothing Nothing

handlePad :: WidgetSpecHandler
handlePad =
    WidgetSpecHandler genSrc doValidate "pad"
        where
          doValidate s =
              let result = PadInfo
                           <$> optionalInt s "top"
                           <*> optionalInt s "bottom"
                           <*> optionalInt s "left"
                           <*> optionalInt s "right"
                           <*> optionalInt s "leftRight"
                           <*> optionalInt s "topBottom"
                           <*> optionalInt s "all"
              in case result of
                   Left e -> Left e
                   Right pInfo ->
                       if pInfo == noPadding
                       then Left "element requires at least one padding attribute"
                       else (,) <$> pure pInfo <*> firstChild s

          genSrc _ nam (padding, ch) = do
            chNam <- newEntry (widgetLikeType ch)
            gen ch chNam

            -- Mapping of projections and the function names we should
            -- use when generating source
            let padFunctions = [ (padTop, "padTop")
                               , (padBottom, "padBottom")
                               , (padLeft, "padLeft")
                               , (padRight, "padRight")
                               , (padLeftRight, "padLeftRight")
                               , (padTopBottom, "padTopBottom")
                               , (padAll, "padAll")
                               ]

            -- For set padding attributes, extract padding expressions
            let paddingExprs :: [Hs.Exp]
                paddingExprs = catMaybes $ foreach padFunctions $ \(field, funcName) -> do
                                 realVal <- field padding
                                 return $ call funcName [mkInt realVal]

            -- Construct padding expression from values
            let ex = foldl (\e1 e2 -> opApp e1 (mkName "pad") e2)
                     (head paddingExprs) (tail paddingExprs)

            append $ bind nam "padded" [ expr chNam
                                       , parens ex
                                       ]

            return $ declareWidget nam (mkTyp "Padded" [])

handleDirBrowser :: WidgetSpecHandler
handleDirBrowser =
    WidgetSpecHandler genSrc doValidation "dirBrowser"
        where
          doValidation s = optional s "skin"

          genSrc _ nam skin = do
            let Just skinName = skin <|> Just "defaultBrowserSkin"

            browserName <- newEntry "browser"
            fgName <- newEntry "focusGroup"
            bData <- newEntry "browserData"
            append $ bind bData "newDirBrowser" [expr $ mkName skinName]

            append $ mkLet [ (nam, call "dirBrowserWidget" [expr browserName])
                           , (browserName, call "fst" [expr bData])
                           , (fgName, call "snd" [expr bData])
                           ]

            mergeFocus nam fgName

            return $ declareWidget nam (mkTyp "DirBrowserWidgetType" [])
                       `withField` (browserName, parseType "DirBrowser")

handleDialog :: WidgetSpecHandler
handleDialog =
    WidgetSpecHandler genSrc doValidation "dialog"
        where
          doValidation s = (,)
                           <$> required s "title"
                           <*> firstChild s

          genSrc _ nam (title, ch) = do
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
    WidgetSpecHandler genSrc doValidation "centered"
        where
          doValidation = firstChild

          genSrc _ nam ch = do
            chNam <- newEntry $ widgetLikeType ch
            gen ch chNam

            append $ bind nam "centered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (parseType $ "VCentered (HCentered (" ++ Hs.prettyPrint chType ++ "))")

handleHCentered :: WidgetSpecHandler
handleHCentered =
    WidgetSpecHandler genSrc doValidation "hCentered"
        where
          doValidation = firstChild

          genSrc _ nam ch = do
            chNam <- newEntry $ widgetLikeType ch
            gen ch chNam

            append $ bind nam "hCentered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (mkTyp "HCentered" [chType])

handleVCentered :: WidgetSpecHandler
handleVCentered =
    WidgetSpecHandler genSrc doValidation "vCentered"
        where
          doValidation = firstChild

          genSrc _ nam ch = do
            chNam <- newEntry $ widgetLikeType ch
            gen ch chNam

            append $ bind nam "vCentered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (mkTyp "VCentered" [chType])

handleVFill :: WidgetSpecHandler
handleVFill =
    WidgetSpecHandler genSrc doValidation "vFill"
        where
          doValidation s = requiredChar s "char"

          genSrc _ nam ch = do
            append $ bind nam "vFill" [mkChar ch]
            return $ declareWidget nam (mkTyp "VFill" [])

handleHFill :: WidgetSpecHandler
handleHFill =
    WidgetSpecHandler genSrc doValidation "hFill"
        where
          doValidation s = (,)
                           <$> requiredChar s "char"
                           <*> requiredInt s "height"

          genSrc _ nam (ch, height) = do
            append $ bind nam "hFill" [ mkChar ch
                                      , mkInt height
                                      ]

            return $ declareWidget nam (mkTyp "HFill" [])

handleProgressBar :: WidgetSpecHandler
handleProgressBar =
    WidgetSpecHandler genSrc doValidation "progressBar"
        where
          doValidation s = (,,)
                           <$> required s "completeColor"
                           <*> required s "incompleteColor"
                           <*> optionalInt s "progress"

          genSrc _ nam (compColor, incompColor, prog) = do
            barName <- newEntry "progressBar"
            append $ bind barName "newProgressBar" [ expr $ mkName compColor
                                                   , expr $ mkName incompColor
                                                   ]
            append $ mkLet [(nam, call "progressBarWidget" [expr barName])]

            case prog of
              Nothing -> return ()
              Just p -> append $ act $ call "setProgress" [expr barName, mkInt p]

            -- The state type is 'Padded' because buttons are
            -- implemented as composite widgets; see the 'Button' type
            -- in Graphics.Vty.Widgets.Button.
            return $ declareWidget nam (mkTyp "Box" [ mkTyp "HFill" []
                                                    , mkTyp "HFill" []
                                                    ])
                       `withField` (barName, parseType "ProgressBar")

handleButton :: WidgetSpecHandler
handleButton =
    WidgetSpecHandler genSrc doValidation "button"
        where
          doValidation s = required s "label"

          genSrc _ nam label = do
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
    WidgetSpecHandler genSrc doValidation "edit"
        where
          doValidation e = optional e "contents"

          genSrc _ nam contents = do
            append $ bind nam "editWidget" []

            case contents of
              Nothing -> return ()
              Just s -> append $ act $ call "setEditText" [ expr nam
                                                          , mkString s
                                                          ]

            return $ declareWidget nam (mkTyp "Edit" [])

handleHBorder :: WidgetSpecHandler
handleHBorder =
    WidgetSpecHandler genSrc (const $ return ()) "hBorder"
        where
          genSrc _ nam _ = do
            append $ bind nam "hBorder" []
            return $ declareWidget nam (mkTyp "HBorder" [])

handleVBorder :: WidgetSpecHandler
handleVBorder =
    WidgetSpecHandler genSrc (const $ return ()) "vBorder"
        where
          genSrc _ nam _ = do
            append $ bind nam "vBorder" []
            return $ declareWidget nam (mkTyp "VBorder" [])

handleBordered :: WidgetSpecHandler
handleBordered =
    WidgetSpecHandler genSrc doValidation "bordered"
        where
          doValidation = firstChild

          genSrc _ nam ch = do
            chNam <- newEntry $ widgetLikeType ch
            gen ch chNam

            append $ bind nam "bordered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (mkTyp "Bordered" [chType])

genBox :: [A.WidgetLike]
       -> String
       -> Maybe Int
       -> Hs.Name
       -> GenM Hs.Name
genBox es typ spacing rootName = do
  names <- forM es $
           \child -> do
              chname <- newEntry $ widgetLikeType child
              gen child chname
              return chname

  let buildBox [] =
          error "BUG: unexpected buildBox input (validation should have caught this)"
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
    WidgetSpecHandler genSrc doValidation "vBox"
        where
          doValidation s = optionalInt s "spacing"

          genSrc e nam spacing = do
            resultName <- genBox (specChildren e) "vBox" spacing nam
            ty <- getWidgetStateType resultName
            return $ declareWidget nam ty

handleBoxSized :: String -> WidgetSpecHandler
handleBoxSized typ =
    WidgetSpecHandler genSrc doValidation (typ ++ "-sized")
        where
          doValidation s = (,)
                           <$> optionalInt s "spacing"
                           <*> boxSize s

          genSrc e nam (spacing, boxSz) = do
            let Hs.ParseOk parsedSizeExpr = Hs.parse $ show boxSz

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

boxSize :: A.WidgetSpec -> Either String ChildSizePolicy
boxSize s = getPercentSize
            <|> getDualSize
            <|> Left "Either a percentage or first/second size policy must be specified for this box"
    where
      getPercentSize = Percentage <$> requiredInt s "percent"

      getDualSize = PerChild
                    <$> (BoxFixed <$> requiredInt s "first"
                         <|> requiredEqual s "first" "auto" *> (pure BoxAuto))
                    <*> (BoxFixed <$> requiredInt s "second"
                         <|> requiredEqual s "second" "auto" *> (pure BoxAuto))

handleHBox :: WidgetSpecHandler
handleHBox =
    WidgetSpecHandler genSrc doValidation "hBox"
        where
          doValidation s = optionalInt s "spacing"

          genSrc e nam spacing = do
            resultName <- genBox (specChildren e) "hBox" spacing nam
            ty <- getWidgetStateType resultName
            return $ declareWidget nam ty

handleFormat :: WidgetSpecHandler
handleFormat =
    WidgetSpecHandler genSrc doValidation "format"
        where
          doValidation s = (,)
                           <$> required s "name"
                           <*> firstChild s

          genSrc _ nam (formatName, ch) = do
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
    WidgetSpecHandler genSrc (const $ return ()) "fText"
        where
          genSrc e nam _ = do
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

handleFocusEntry :: A.Interface -> A.Doc -> A.WidgetId -> Hs.Name -> GenM ()
handleFocusEntry iface doc entryName fgName = do
  let ws = concat [ getNamedWidgetNames (A.interfaceContent iface)
                  , sharedNames
                  ]
      sharedNames = concat $ map (getNamedWidgetNames . A.Widget) shared
      shared = A.documentSharedWidgets doc
  case entryName `elem` ws of
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
