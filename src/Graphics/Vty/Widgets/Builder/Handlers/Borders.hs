module Graphics.Vty.Widgets.Builder.Handlers.Borders
    ( handlers
    )
where

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S

handlers :: [WidgetElementHandler]
handlers = [ handleHBorder
           , handleVBorder
           , handleBordered
           ]

handleHBorder :: WidgetElementHandler
handleHBorder =
    WidgetElementHandler genSrc (const $ return ()) "hBorder"
        where
          genSrc nam _ = do
            append $ S.bind nam "hBorder" []
            return $ declareWidget nam (S.mkTyp "HBorder" [])

handleVBorder :: WidgetElementHandler
handleVBorder =
    WidgetElementHandler genSrc (const $ return ()) "vBorder"
        where
          genSrc nam _ = do
            append $ S.bind nam "vBorder" []
            return $ declareWidget nam (S.mkTyp "VBorder" [])

handleBordered :: WidgetElementHandler
handleBordered =
    WidgetElementHandler genSrc doValidation "bordered"
        where
          doValidation = V.firstChildWidget

          genSrc nam ch = do
            chNam <- newEntry $ widgetLikeName ch
            gen ch chNam

            append $ S.bind nam "bordered" [S.expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (S.mkTyp "Bordered" [chType])
