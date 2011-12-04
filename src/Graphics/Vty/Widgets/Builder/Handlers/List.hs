module Graphics.Vty.Widgets.Builder.Handlers.List
    ( handlers
    )
where

import Control.Applicative
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.AST as A
import qualified Graphics.Vty.Widgets.Builder.Validation as V

handlers :: [WidgetElementHandler]
handlers = [ handleStringList
           , handleList
           ]

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
