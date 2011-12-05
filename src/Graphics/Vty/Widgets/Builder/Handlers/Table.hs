module Graphics.Vty.Widgets.Builder.Handlers.Table
    ( handlers
    )
where

import Control.Monad (when, forM_, forM)
import Data.Traversable (for)
import Data.List (nub)
import Control.Applicative ((<$>), (<*>), pure, (<|>), (*>))
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import Graphics.Vty.Widgets.Builder.Util (foreach, splitOn)
import qualified Graphics.Vty.Widgets.Builder.AST as A
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Language.Haskell.Exts as Hs

import Graphics.Vty.Widgets.Table
    ( BorderStyle(..)
    , BorderFlag(..)
    , ColumnSpec(..)
    , ColumnSize(..)
    , column
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