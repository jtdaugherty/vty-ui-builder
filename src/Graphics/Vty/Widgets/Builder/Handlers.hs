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
    [ handleWrap
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
  forM_ (A.documentSharedWidgets doc) $ \(_, spec) ->
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

  forM_ (A.interfaceFocusEntries iface) $ \info ->
      handleFocusEntry iface doc info fgName

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

          genSrc nam (label, rg) = do
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

          genSrc nam (cursorFg, cursorBg) = do
            let attrExpr = case attrsToExpr (cursorFg, cursorBg) of
                             Nothing -> defAttr
                             Just ex -> ex

            append $ bind nam "newStringList" [attrExpr, mkList []]
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

          genSrc nam (cursorFg, cursorBg, keyType, elemType) = do
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
                         <*> firstChildWidget s

          genSrc nam (height, ch) = do
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
                         <*> firstChildWidget s

          genSrc nam (width, ch) = do
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
                         <*> firstChildWidget s

          genSrc nam (width, height, ch) = do
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
                         <*> firstChildWidget s

          genSrc nam (height, ch) = do
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
                         <*> firstChildWidget s

          genSrc nam (width, ch) = do
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
                         <*> firstChildWidget s

          genSrc nam (width, height, ch) = do
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
          doValidate s = do
            pInfo <- PadInfo
                     <$> optionalInt s "top"
                     <*> optionalInt s "bottom"
                     <*> optionalInt s "left"
                     <*> optionalInt s "right"
                     <*> optionalInt s "leftRight"
                     <*> optionalInt s "topBottom"
                     <*> optionalInt s "all"
            if pInfo == noPadding then
                failValidation $ Error (A.widgetLocation s)
                                   "element requires at least one padding attribute" else
                (,) <$> pure pInfo <*> firstChildWidget s

          genSrc nam (padding, ch) = do
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

          genSrc nam skin = do
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
                           <*> firstChildWidget s

          genSrc nam (title, ch) = do
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
          doValidation = firstChildWidget

          genSrc nam ch = do
            chNam <- newEntry $ widgetLikeType ch
            gen ch chNam

            append $ bind nam "centered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (parseType $ "VCentered (HCentered (" ++ Hs.prettyPrint chType ++ "))")

handleHCentered :: WidgetSpecHandler
handleHCentered =
    WidgetSpecHandler genSrc doValidation "hCentered"
        where
          doValidation = firstChildWidget

          genSrc nam ch = do
            chNam <- newEntry $ widgetLikeType ch
            gen ch chNam

            append $ bind nam "hCentered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (mkTyp "HCentered" [chType])

handleVCentered :: WidgetSpecHandler
handleVCentered =
    WidgetSpecHandler genSrc doValidation "vCentered"
        where
          doValidation = firstChildWidget

          genSrc nam ch = do
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

          genSrc nam ch = do
            append $ bind nam "vFill" [mkChar ch]
            return $ declareWidget nam (mkTyp "VFill" [])

handleHFill :: WidgetSpecHandler
handleHFill =
    WidgetSpecHandler genSrc doValidation "hFill"
        where
          doValidation s = (,)
                           <$> requiredChar s "char"
                           <*> requiredInt s "height"

          genSrc nam (ch, height) = do
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

          genSrc nam (compColor, incompColor, prog) = do
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

          genSrc nam label = do
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

          genSrc nam contents = do
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
          genSrc nam _ = do
            append $ bind nam "hBorder" []
            return $ declareWidget nam (mkTyp "HBorder" [])

handleVBorder :: WidgetSpecHandler
handleVBorder =
    WidgetSpecHandler genSrc (const $ return ()) "vBorder"
        where
          genSrc nam _ = do
            append $ bind nam "vBorder" []
            return $ declareWidget nam (mkTyp "VBorder" [])

handleBordered :: WidgetSpecHandler
handleBordered =
    WidgetSpecHandler genSrc doValidation "bordered"
        where
          doValidation = firstChildWidget

          genSrc nam ch = do
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

boxChildWidgets :: A.WidgetSpec -> ValidateM [A.WidgetLike]
boxChildWidgets s =
    case specChildWidgets s of
      es@(_:_:_) -> return es
      _ -> failValidation $ Error (A.widgetLocation s) "Box must have at least two children"

sizedBoxChildWidgets :: A.WidgetSpec -> ValidateM [A.WidgetLike]
sizedBoxChildWidgets s =
    case specChildWidgets s of
      es@[_,_] -> return es
      _ -> failValidation $ Error (A.widgetLocation s) "Sized box must have exactly two children"

handleVBox :: WidgetSpecHandler
handleVBox =
    WidgetSpecHandler genSrc doValidation "vBox"
        where
          doValidation s = (,)
                           <$> optionalInt s "spacing"
                           <*> boxChildWidgets s

          genSrc nam (spacing, chs) = do
            resultName <- genBox chs "vBox" spacing nam
            ty <- getWidgetStateType resultName
            return $ declareWidget nam ty

handleBoxSized :: String -> WidgetSpecHandler
handleBoxSized typ =
    WidgetSpecHandler genSrc doValidation (typ ++ "-sized")
        where
          doValidation s = (,,)
                           <$> optionalInt s "spacing"
                           <*> boxSize s
                           <*> sizedBoxChildWidgets s

          genSrc nam (spacing, boxSz, chs) = do
            let Hs.ParseOk parsedSizeExpr = Hs.parse $ show boxSz

            resultName <- genBox chs typ spacing nam
            append $ act $ call "setBoxChildSizePolicy" [ expr nam
                                                        , parsedSizeExpr
                                                        ]
            ty <- getWidgetStateType resultName
            return $ declareWidget nam ty

handleHBoxSized :: WidgetSpecHandler
handleHBoxSized = handleBoxSized "hBox"

handleVBoxSized :: WidgetSpecHandler
handleVBoxSized = handleBoxSized "vBox"

boxSize :: A.WidgetSpec -> ValidateM ChildSizePolicy
boxSize s = getPercentSize
            <|> getDualSize
            <|> (failValidation (Error (A.widgetLocation s)
                 "Either a percentage or first/second size policy must be specified for this box"))
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
          doValidation s = (,)
                           <$> optionalInt s "spacing"
                           <*> boxChildWidgets s

          genSrc nam (spacing, chs) = do
            resultName <- genBox chs "hBox" spacing nam
            ty <- getWidgetStateType resultName
            return $ declareWidget nam ty

handleWrap :: WidgetSpecHandler
handleWrap =
    WidgetSpecHandler genSrc doValidation "wrap"
        where
          doValidation s = requireWidgetType "fText" =<< firstChildWidget s

          genSrc nam ch = do
            gen ch nam
            tempNam <- newEntry "formattedText"
            append $ bind tempNam "getTextFormatter" [expr nam]
            append $ act $ call "setTextFormatter" [ expr nam
                                                   , parens (opApp (expr tempNam) (mkName "mappend") (expr $ mkName "wrap"))
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
    WidgetSpecHandler genSrc doValidation "fText"
        where
          isWhitespace = (`elem` " \t\n")

          doValidation s = pairs
              where
                -- For each entry in the contents list: If it is a
                -- string, give it the default attribute and put it in
                -- a list.  If it is an Attr element, recurse on it,
                -- building another list of (string, attr) to merge.

                -- Left: do not strip whitespace
                -- Right: do strip whitespace
                processSpecContent :: Hs.Exp -> A.WidgetSpecContent -> ValidateM [(Either String String, Hs.Exp)]
                processSpecContent ex c =
                    case c of
                      A.Text str _ -> return [(Right $ stripWhitespace str, ex)]
                      A.ChildElement elm ->
                          case A.elementType elm of
                            "br" -> return [(Left "\n", ex)]
                            "attr" -> processAttr elm
                            badName -> failValidation $ Error (A.widgetLocation s) $ "got unsupported child of attr: " ++ badName
                      A.ChildWidgetLike _ -> failValidation $ Error (A.widgetLocation s) "got unsupported child of attr: widget-like"

                processElemContent :: Hs.Exp -> A.ElementContent -> ValidateM [(Either String String, Hs.Exp)]
                processElemContent ex c =
                    case c of
                      A.ElemText str _ -> return [(Right $ stripWhitespace str, ex)]
                      A.ElemChild elm ->
                          case A.elementType elm of
                            "br" -> return [(Left "\n", ex)]
                            "attr" -> processAttr elm
                            badName -> failValidation $ Error (A.widgetLocation s) $ "got unsupported child of attr: " ++ badName

                processAttr :: A.Element -> ValidateM [(Either String String, Hs.Exp)]
                processAttr elm = do
                  let loc = A.elementLocation elm
                  attrResult <- (,)
                                <$> (requireValidColor loc $ elemAttribute elm "fg")
                                <*> (requireValidColor loc $ elemAttribute elm "bg")

                  let attrExpr = case attrsToExpr attrResult of
                                   Nothing -> defAttr
                                   Just ex -> ex

                  results <- mapM (processElemContent attrExpr) $ A.elementContents elm

                  return $ concat $ results

                stripWhitespace :: [Char] -> [Char]
                stripWhitespace (c1:c2:cs) = if isWhitespace c1 && isWhitespace c2
                                             then stripWhitespace (c2:cs)
                                             else c1 : (stripWhitespace (c2:cs))
                stripWhitespace ls = ls

                pairs :: ValidateM [(Either String String, Hs.Exp)]
                pairs = do
                  results <- mapM (processSpecContent defAttr) $ A.widgetSpecContents s
                  return $ concat $ results

          genSrc nam pairs = do
            let collapse :: [(Either String String, Hs.Exp)] -> [(Either String String, Hs.Exp)]
                collapse [] = []
                collapse [p] = [p]
                collapse ((Right s1, e1):(Right s2, e2):es) =
                    if e1 == e2
                    then collapse ((Right $ s1 ++ s2, e1) : es)
                    else (Right s1, e1) : collapse ((Right s2, e2):es)
                collapse (p:ps) = p : collapse ps

                collapsed :: [(Either String String, Hs.Exp)]
                collapsed = collapse pairs

                pairExpr :: (String, Hs.Exp) -> Hs.Exp
                pairExpr (s, ex) = mkTup [ mkString s
                                         , ex
                                         ]

                headTrimmed ((Right s, attr):rest) = (Right $ dropWhile isWhitespace s, attr) : rest
                headTrimmed es = es

                tailTrimmed [] = []
                tailTrimmed ls =
                    init ls ++ case last ls of
                                 (Right s, attr) -> [(Right $ reverse $ dropWhile isWhitespace $ reverse s, attr)]
                                 other -> [other]

                normStrings ls = map (\(s, attr) -> (either id id s, attr)) ls

                pairExprList = map pairExpr $ normStrings $ tailTrimmed $ headTrimmed collapsed

            append $ bind nam "plainTextWithAttrs" [mkList pairExprList]
            return $ declareWidget nam (mkTyp "FormattedText" [])

handleFocusEntry :: A.Interface -> A.Doc -> (A.WidgetId, A.SourceLocation)
                 -> Hs.Name
                 -> GenM ()
handleFocusEntry iface doc (entryName, loc) fgName = do
  let ws = concat [ getNamedWidgetNames (A.interfaceContent iface)
                  , sharedNames
                  , map A.paramName $ A.documentParams doc
                  ]
      sharedNames = concat $ map (getNamedWidgetNames . A.Widget) shared
      shared = map snd $ A.documentSharedWidgets doc
  case entryName `elem` ws of
      False -> putError loc $ "Focus group error: widget name "
               ++ show entryName
               ++ " not found in interface " ++ show (A.interfaceName iface)
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
