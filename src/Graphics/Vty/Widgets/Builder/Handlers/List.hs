module Graphics.Vty.Widgets.Builder.Handlers.List
    ( handlers
    )
where

import Control.Applicative
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.AST as A
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S

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
            let attrExpr = case S.attrsToExpr (cursorFg, cursorBg) of
                             Nothing -> S.defAttr
                             Just ex -> ex

            append $ S.bind nam "newStringList" [attrExpr, S.mkList []]
            return $ declareWidget nam $ S.parseType "List String FormattedText"

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
            let attrExpr = case S.attrsToExpr (cursorFg, cursorBg) of
                             Nothing -> S.defAttr
                             Just ex -> ex

            append $ S.bind nam "newList" [attrExpr]
            return $ declareWidget nam $ S.parseType $ "List (" ++ keyType ++
                       ") (" ++ elemType ++ ")"
