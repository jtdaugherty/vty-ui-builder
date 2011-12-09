module Graphics.Vty.Widgets.Builder.Handlers.Table
    ( handlers
    )
where

import Control.Monad (when, forM_, forM)
import Data.Traversable (for)
import Data.List (nub, intercalate)
import Control.Applicative ((<$>), (<*>), pure, (<|>), (*>))
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import Graphics.Vty.Widgets.Builder.Util (foreach, splitOn)
import qualified Graphics.Vty.Widgets.Builder.AST as A
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S
import qualified Language.Haskell.Exts as Hs

import Graphics.Vty.Widgets.Table
    ( BorderStyle(..)
    , BorderFlag(..)
    , ColumnSpec(..)
    , ColumnSize(..)
    , Alignment(..)
    , column
    , align
    )
import Graphics.Vty.Widgets.Builder.Handlers.Pad
    ( PadInfo
    , paddingFromAttributes
    , paddingExprFromPadInfo
    )

handlers :: [WidgetElementHandler]
handlers = [handleTable]

data TableInfo =
    TableInfo { rows :: [RowInfo]
              , borderStyle :: BorderStyle
              , columnSpecs :: [ColumnSpec]
              }
    deriving (Show)

data RowInfo =
    RowInfo { cells :: [Cell]
            }
    deriving (Show)

data Cell =
    Cell { content :: A.WidgetLike
         , padding :: PadInfo
         , alignment :: Maybe Alignment
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
                 failValidation s "table must have at least one column specification"

            when (null rs) $
                 failValidation s "table must have at least one row"

            when (length specs /= (length $ cells $ head rs)) $
                 failValidation s $ "number of column specifications must match the number of columns ("
                                    ++ (show $ length $ cells $ head rs) ++ ")"

            TableInfo <$> pure rs
                          <*> getBorderStyle s
                          <*> pure specs

          getColumnSpecs s =
              concat <$> (for (V.elementsByName "columns" $ A.getChildElements s)
                              validateColumnSpecs)

          validateColumnSpecs e =
              for (V.elementsByName "column" $ A.getChildElements e)
                  validateColumnSpec

          validateColumnSpec e = do
            let col = column <$> ((V.requiredEqual e "size" "auto" *> pure ColAuto)
                                  <|> (ColFixed <$> V.requiredInt e "size"))
            val <- V.optional e "align"
            case val of
              Nothing -> col
              Just a -> align <$> col <*> parseAlignment e a

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
                    Nothing -> failValidation s $
                               "invalid border style flag " ++ show f ++ ", valid choices are 'none', "
                              ++ "'full', or a comma-separated list of " ++ show (map fst borderFlags)

          borderFlags = [ ("rows", Rows)
                        , ("columns", Columns)
                        , ("edges", Edges)
                        ]

          getRows s = do
            rs <- validateRows s
            if (length $ nub (map (length . cells) rs)) /= 1 then
                failValidation s $ "all rows must have the same number of cells" else
                if nub (map (length . cells) rs) == [0] then
                    failValidation s "all rows must contain at least one cell" else
                    return rs

          validateRows s =
              for (V.elementsByName "row" $ A.getChildElements s)
                  validateRow

          validateRow e =
              RowInfo <$> validateCells e

          validateCells e =
              for (A.getChildElements e) $ \c ->
                  V.elemName c "cell" *>
                       (Cell
                        <$> V.firstChildWidget c
                        <*> paddingFromAttributes c
                        <*> getAlignment c)

          getAlignment e =
              do
                val <- V.optional e "align"
                case val of
                  Nothing -> return Nothing
                  Just a -> Just <$> parseAlignment e a

          validAlignments = [ ("left", AlignLeft)
                            , ("center", AlignCenter)
                            , ("right", AlignRight)
                            ]

          parseAlignment :: A.Element -> String -> ValidateM Alignment
          parseAlignment e alignStr = do
            case lookup alignStr validAlignments of
              Nothing -> failValidation e $ "'align' attribute must be one of " ++
                         (intercalate ", " $ map (show . fst) validAlignments)
              Just a -> return a

          genSrc nam table = do
            let parsedBorderStyle = S.toAST $ borderStyle table

                colSpecExprs :: [Hs.Exp]
                colSpecExprs = foreach (columnSpecs table) $ \spec ->
                               let ex = S.call "column" [S.toAST $ columnSize spec]
                               in case columnAlignment spec of
                                    Nothing -> ex
                                    Just a -> S.call "align" [ S.parens ex
                                                             , S.toAST a
                                                             ]

            append $ S.bind nam "newTable" [S.mkList colSpecExprs, parsedBorderStyle]

            let buildRow :: [(Hs.Name, Cell)] -> Hs.Exp
                buildRow [] = error "BUG: validation should prevent empty table rows"
                buildRow [(w,c)] = mkCell w c
                buildRow ((w,c):rest) = S.opApp (mkCell w c) (S.mkName "mappend") $ buildRow rest

                mkCell w c = S.parens $ S.call "mkRow" [ex]
                    where
                      inner = S.opApp (S.call "customCell" [S.expr w])
                              (S.mkName "pad")
                              (S.parens $ paddingExprFromPadInfo $ padding c)

                      ex = case alignment c of
                             Nothing -> inner
                             Just val -> S.opApp inner (S.mkName "align") (S.toAST val)

            forM_ (rows table) $ \row ->
                do
                  cellWidgetNames <- forM (cells row) $ \cell -> do
                                       cellName <- newEntry "cell"
                                       gen (content cell) cellName
                                       return cellName
                  append $ S.act $ S.call "addRow" [ S.expr nam
                                                   , buildRow $ zip cellWidgetNames $ cells row
                                                   ]

            return $ declareWidget nam (S.mkTyp "Table" [])