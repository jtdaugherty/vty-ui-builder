{-# OPTIONS_GHC -Werror  #-}
module Graphics.Vty.Widgets.Builder.Handlers
    ( handleDoc
    , coreSpecHandlers
    )
where

import Control.Applicative hiding (optional)
import Control.Monad
import Data.Maybe
import Data.Traversable (for)
import Data.List (nub)

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import Graphics.Vty.Widgets.Builder.Util
import qualified Graphics.Vty.Widgets.Builder.AST as A
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Graphics.Vty.Widgets.Builder.Names as Names

import qualified Graphics.Vty.Widgets.Builder.Handlers.Box as Box
import qualified Graphics.Vty.Widgets.Builder.Handlers.Wrap as Wrap
import qualified Graphics.Vty.Widgets.Builder.Handlers.Borders as Borders
import qualified Graphics.Vty.Widgets.Builder.Handlers.Fills as Fills
import qualified Graphics.Vty.Widgets.Builder.Handlers.Centered as Centered

import qualified Language.Haskell.Exts as Hs

import Graphics.Vty.Widgets.Table
    ( BorderStyle(..)
    , BorderFlag(..)
    , ColumnSpec(..)
    , ColumnSize(..)
    , column
    )

coreSpecHandlers :: [WidgetElementHandler]
coreSpecHandlers =
    [ handleFormattedText
    , handleEdit
    , handleButton
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
    , handleTable
    ] ++ Box.handlers
    ++ Wrap.handlers
    ++ Borders.handlers
    ++ Fills.handlers
    ++ Centered.handlers

handleDoc :: A.Doc -> GenM ()
handleDoc doc = do
  mapM_ handleParam $ A.documentParams doc
  forM_ (A.documentSharedWidgets doc) $ \(_, spec) ->
      do
        nam <- newEntry $ widgetElementName spec
        gen (A.Widget spec) nam

  append $ bind Names.collectionName "newCollection" []

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

  append $ bind actName "addToCollection" [ expr Names.collectionName
                                          , expr nam
                                          , expr fgName
                                          ]

  let vals = InterfaceValues { topLevelWidgetName = nam
                             , switchActionName = actName
                             , focusGroupName = fgName
                             }
  registerInterface (A.interfaceName iface) vals

handleParam :: A.Param -> GenM ()
handleParam p = do
  let paramName = mkName $ A.paramName p
      typ = parseType $ A.paramType p
  registerReferenceTarget paramName paramName typ
  setFocusValue paramName $ WidgetName { widgetName = paramName
                                       , widgetType = typ
                                       }

handleCheckBox :: WidgetElementHandler
handleCheckBox =
    WidgetElementHandler genSrc doValidate "checkBox"

        where
          doValidate s = (,)
                         <$> V.required s "label"
                         <*> V.optional s "radioGroup"

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

handleStringList :: WidgetElementHandler
handleStringList =
    WidgetElementHandler genSrc doValidate "stringList"

        where
          loc = A.sourceLocation

          doValidate s = (,)
                         <$> (V.requireValidColor (loc s) =<<
                              V.optional s "cursorFg")
                         <*> (V.requireValidColor (loc s) =<<
                              V.optional s "cursorBg")

          genSrc nam (cursorFg, cursorBg) = do
            let attrExpr = case attrsToExpr (cursorFg, cursorBg) of
                             Nothing -> defAttr
                             Just ex -> ex

            append $ bind nam "newStringList" [attrExpr, mkList []]
            return $ declareWidget nam $ parseType "List String FormattedText"

handleList :: WidgetElementHandler
handleList =
    WidgetElementHandler genSrc doValidate "list"

        where
          loc = A.sourceLocation

          doValidate s = (,,,)
                         <$> (V.requireValidColor (loc s) =<<
                              V.optional s "cursorFg")
                         <*> (V.requireValidColor (loc s) =<<
                              V.optional s "cursorBg")
                         <*> V.required s "keyType"
                         <*> V.required s "elemType"

          genSrc nam (cursorFg, cursorBg, keyType, elemType) = do
            let attrExpr = case attrsToExpr (cursorFg, cursorBg) of
                             Nothing -> defAttr
                             Just ex -> ex

            append $ bind nam "newList" [attrExpr]
            return $ declareWidget nam $ parseType $ "List (" ++ keyType ++
                       ") (" ++ elemType ++ ")"

handleVLimit :: WidgetElementHandler
handleVLimit =
    WidgetElementHandler genSrc doValidate "vLimit"

        where
          doValidate s = (,)
                         <$> V.requiredInt s "height"
                         <*> V.firstChildWidget s

          genSrc nam (height, ch) = do
            chNam <- newEntry (widgetLikeName ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "vLimit" [mkInt height, expr chNam]
            return $ declareWidget nam $
                   parseType $ "VLimit (" ++ Hs.prettyPrint chType ++ ")"

handleHLimit :: WidgetElementHandler
handleHLimit =
    WidgetElementHandler genSrc doValidate "hLimit"
        where
          doValidate s = (,)
                         <$> V.requiredInt s "width"
                         <*> V.firstChildWidget s

          genSrc nam (width, ch) = do
            chNam <- newEntry (widgetLikeName ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "hLimit" [mkInt width, expr chNam]
            return $ declareWidget nam $
                   parseType $ "HLimit (" ++ Hs.prettyPrint chType ++ ")"

handleBoxLimit :: WidgetElementHandler
handleBoxLimit =
    WidgetElementHandler genSrc doValidate "boxLimit"
        where
          doValidate s = (,,)
                         <$> (V.requiredInt s "width")
                         <*> (V.requiredInt s "height")
                         <*> V.firstChildWidget s

          genSrc nam (width, height, ch) = do
            chNam <- newEntry (widgetLikeName ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "boxLimit" [ mkInt width
                                         , mkInt height
                                         , expr chNam
                                         ]
            return $ declareWidget nam $
                   parseType $ "VLimit (HLimit (" ++ Hs.prettyPrint chType ++ "))"

handleVFixed :: WidgetElementHandler
handleVFixed =
    WidgetElementHandler genSrc doValidate "vFixed"
        where
          doValidate s = (,)
                         <$> V.requiredInt s "height"
                         <*> V.firstChildWidget s

          genSrc nam (height, ch) = do
            chNam <- newEntry (widgetLikeName ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "vFixed" [mkInt height, expr chNam]
            return $ declareWidget nam $
                   parseType $ "VFixed (" ++ Hs.prettyPrint chType ++ ")"

handleHFixed :: WidgetElementHandler
handleHFixed =
    WidgetElementHandler genSrc doValidate "hFixed"
        where
          doValidate s = (,)
                         <$> V.requiredInt s "width"
                         <*> V.firstChildWidget s

          genSrc nam (width, ch) = do
            chNam <- newEntry (widgetLikeName ch)
            gen ch chNam
            chType <- getWidgetStateType chNam

            append $ bind nam "hFixed" [mkInt width, expr chNam]
            return $ declareWidget nam $
                   parseType $ "HFixed (" ++ Hs.prettyPrint chType ++ ")"

handleBoxFixed :: WidgetElementHandler
handleBoxFixed =
    WidgetElementHandler genSrc doValidate "boxFixed"
        where
          doValidate s = (,,)
                         <$> V.requiredInt s "width"
                         <*> V.requiredInt s "height"
                         <*> V.firstChildWidget s

          genSrc nam (width, height, ch) = do
            chNam <- newEntry (widgetLikeName ch)
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

handlePad :: WidgetElementHandler
handlePad =
    WidgetElementHandler genSrc doValidate "pad"
        where
          doValidate s = do
            pInfo <- PadInfo
                     <$> V.optionalInt s "top"
                     <*> V.optionalInt s "bottom"
                     <*> V.optionalInt s "left"
                     <*> V.optionalInt s "right"
                     <*> V.optionalInt s "leftRight"
                     <*> V.optionalInt s "topBottom"
                     <*> V.optionalInt s "all"
            if pInfo == noPadding then
                failValidation $ Error (A.sourceLocation s)
                                   "element requires at least one padding attribute" else
                (,) <$> pure pInfo <*> V.firstChildWidget s

          genSrc nam (padding, ch) = do
            chNam <- newEntry (widgetLikeName ch)
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

handleDirBrowser :: WidgetElementHandler
handleDirBrowser =
    WidgetElementHandler genSrc doValidation "dirBrowser"
        where
          doValidation s = V.optional s "skin"

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

handleDialog :: WidgetElementHandler
handleDialog =
    WidgetElementHandler genSrc doValidation "dialog"
        where
          doValidation s = (,)
                           <$> V.required s "title"
                           <*> V.firstChildWidget s

          genSrc nam (title, ch) = do
            chNam <- newEntry $ widgetLikeName ch
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

handleProgressBar :: WidgetElementHandler
handleProgressBar =
    WidgetElementHandler genSrc doValidation "progressBar"
        where
          doValidation s = (,,)
                           <$> V.required s "completeColor"
                           <*> V.required s "incompleteColor"
                           <*> V.optionalInt s "progress"

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

handleButton :: WidgetElementHandler
handleButton =
    WidgetElementHandler genSrc doValidation "button"
        where
          doValidation s = V.required s "label"

          genSrc nam label = do
            buttonName <- newEntry "button"

            append $ bind buttonName "newButton" [mkString label]
            append $ mkLet [(nam, call "buttonWidget" [expr buttonName])]

            -- The state type is 'Padded' because buttons are
            -- implemented as composite widgets; see the 'Button' type
            -- in Graphics.Vty.Widgets.Button.
            return $ declareWidget nam (mkTyp "Padded" [])
                       `withField` (buttonName, parseType "Button")

handleEdit :: WidgetElementHandler
handleEdit =
    WidgetElementHandler genSrc doValidation "edit"
        where
          doValidation e = V.optional e "contents"

          genSrc nam contents = do
            append $ bind nam "editWidget" []

            case contents of
              Nothing -> return ()
              Just s -> append $ act $ call "setEditText" [ expr nam
                                                          , mkString s
                                                          ]

            return $ declareWidget nam (mkTyp "Edit" [])

defAttr :: Hs.Exp
defAttr = expr $ mkName "def_attr"

handleFormattedText :: WidgetElementHandler
handleFormattedText =
    WidgetElementHandler genSrc doValidation "fText"
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
                processSpecContent :: Hs.Exp -> A.ElementContent -> ValidateM [(Either String String, Hs.Exp)]
                processSpecContent ex c =
                    case c of
                      A.Text str _ -> return [(Right $ stripWhitespace str, ex)]
                      A.ChildElement elm ->
                          case A.elementName elm of
                            "br" -> return [(Left "\n", ex)]
                            "attr" -> processAttr elm
                            badName -> failValidation $ Error (A.sourceLocation s) $ "got unsupported child of fText: " ++ badName
                      A.ChildWidgetLike _ -> failValidation $ Error (A.sourceLocation s) "got unsupported child of fText: widget-like"

                processElemContent :: Hs.Exp -> A.ElementContent -> ValidateM [(Either String String, Hs.Exp)]
                processElemContent ex c =
                    case c of
                      A.ChildWidgetLike _ -> failValidation $ Error (A.sourceLocation s) "got unsupported child of attr: widget-like"
                      A.Text str _ -> return [(Right $ stripWhitespace str, ex)]
                      A.ChildElement elm ->
                          case A.elementName elm of
                            "br" -> return [(Left "\n", ex)]
                            "attr" -> processAttr elm
                            badName -> failValidation $ Error (A.sourceLocation s) $ "got unsupported child of attr: " ++ badName

                processAttr :: A.Element -> ValidateM [(Either String String, Hs.Exp)]
                processAttr elm = do
                  let loc = A.elementLocation elm
                  attrResult <- (,)
                                <$> (V.requireValidColor loc $ getAttribute elm "fg")
                                <*> (V.requireValidColor loc $ getAttribute elm "bg")

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
                  results <- mapM (processSpecContent defAttr) $ A.elementContents s
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

data TableInfo =
    TableInfo { rows :: [RowInfo]
              , borderStyle :: BorderStyle
              , columnSpecs :: [ColumnSpec]
              }
    deriving (Show)

data RowInfo =
    RowInfo { cells :: [A.WidgetLike]
            }
    deriving (Show)

handleTable :: WidgetElementHandler
handleTable =
    WidgetElementHandler genSrc doValidation "table"
        where
          doValidation s = do
            specs <- getColumnSpecs s
            rs <- getRows s

            when (null specs) $
                 failValidation $ Error (A.sourceLocation s)
                                    "table must have at least one column specification"

            when (null rs) $
                 failValidation $ Error (A.sourceLocation s)
                                    "table must have at least one row"

            when (length specs /= (length $ cells $ head rs)) $
                 failValidation $ Error (A.sourceLocation s) $
                                    "number of column specifications must match the number of columns (" ++ (show $ length $ cells $ head rs) ++ ")"

            TableInfo <$> pure rs
                          <*> getBorderStyle s
                          <*> pure specs

          getColumnSpecs s =
              concat <$> (for (V.elementsByName "columns" $ getChildElements s)
                              validateColumnSpecs)

          validateColumnSpecs e =
              for (V.elementsByName "column" $ getChildElements e)
                  validateColumnSpec

          validateColumnSpec e =
              column <$> ((V.requiredEqual e "size" "auto" *> pure ColAuto)
                          <|> (ColFixed <$> V.requiredInt e "size"))

          getBorderStyle s = do
            attr <- V.optional s "borderStyle"
            case attr of
              Nothing -> return BorderFull
              Just val ->
                  case val of
                    "none" -> return BorderNone
                    "full" -> return BorderFull
                    flagStr -> BorderPartial <$> (parsedFlags s $ splitOn ',' flagStr)

          parsedFlags s flgs =
              forM flgs $ \f ->
                  case lookup f borderFlags of
                    Just val -> return val
                    Nothing -> failValidation $ Error (A.sourceLocation s) $
                               "invalid border style flag " ++ show f ++ ", valid choices are 'none', "
                              ++ "'full', or a comma-separated list of " ++ show (map fst borderFlags)

          borderFlags = [ ("rows", Rows)
                        , ("columns", Columns)
                        , ("edges", Edges)
                        ]

          getRows s = do
            rs <- validateRows s
            if (length $ nub (map (length . cells) rs)) /= 1 then
                failValidation $ Error (A.sourceLocation s) $ "all rows must have the same number of cells" else
                if nub (map (length . cells) rs) == [0] then
                    failValidation $ Error (A.sourceLocation s) "all rows must contain at least one cell" else
                    return rs

          validateRows s =
              for (V.elementsByName "row" $ getChildElements s)
                  validateRow

          validateRow e =
              RowInfo <$> validateCells e

          validateCells e =
              for (getChildElements e) $ \c ->
                  V.elemName c "cell" *> V.firstChildWidget c

          genSrc nam table = do
            let parsedBorderStyle = toAST $ borderStyle table

                colSpecExprs :: [Hs.Exp]
                colSpecExprs = foreach (columnSpecs table) $ \spec ->
                               call "column" [toAST $ columnSize spec]

            append $ bind nam "newTable" [mkList colSpecExprs, parsedBorderStyle]

            let buildRow [] = error "BUG: validation should prevent empty table rows"
                buildRow [w] = mkCell w
                buildRow (w:ws) = opApp (mkCell w) (mkName "mappend") $ buildRow ws

                mkCell w = parens $ call "mkRow" [call "customCell" [expr w]]

            -- Since the types of the cells can vary, we have to use .|.
            forM_ (rows table) $ \row ->
                do
                  cellWidgetNames <- forM (cells row) $ \cell -> do
                                       cellName <- newEntry "cell"
                                       gen cell cellName
                                       return cellName
                  append $ act $ call "addRow" [expr nam, buildRow cellWidgetNames]

            return $ declareWidget nam (mkTyp "Table" [])