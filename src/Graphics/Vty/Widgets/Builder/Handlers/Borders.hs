module Graphics.Vty.Widgets.Builder.Handlers.Borders
    ( handlers
    )
where

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V

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
            append $ bind nam "hBorder" []
            return $ declareWidget nam (mkTyp "HBorder" [])

handleVBorder :: WidgetElementHandler
handleVBorder =
    WidgetElementHandler genSrc (const $ return ()) "vBorder"
        where
          genSrc nam _ = do
            append $ bind nam "vBorder" []
            return $ declareWidget nam (mkTyp "VBorder" [])

handleBordered :: WidgetElementHandler
handleBordered =
    WidgetElementHandler genSrc doValidation "bordered"
        where
          doValidation = V.firstChildWidget

          genSrc nam ch = do
            chNam <- newEntry $ widgetLikeName ch
            gen ch chNam

            append $ bind nam "bordered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (mkTyp "Bordered" [chType])
