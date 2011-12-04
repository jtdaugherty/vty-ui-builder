module Graphics.Vty.Widgets.Builder.Handlers.Centered
    ( handlers
    )
where

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Language.Haskell.Exts as Hs

handlers :: [WidgetElementHandler]
handlers = [ handleCentered
           , handleHCentered
           , handleVCentered
           ]

handleCentered :: WidgetElementHandler
handleCentered =
    WidgetElementHandler genSrc doValidation "centered"
        where
          doValidation = V.firstChildWidget

          genSrc nam ch = do
            chNam <- newEntry $ widgetLikeName ch
            gen ch chNam

            append $ bind nam "centered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (parseType $ "VCentered (HCentered (" ++ Hs.prettyPrint chType ++ "))")

handleHCentered :: WidgetElementHandler
handleHCentered =
    WidgetElementHandler genSrc doValidation "hCentered"
        where
          doValidation = V.firstChildWidget

          genSrc nam ch = do
            chNam <- newEntry $ widgetLikeName ch
            gen ch chNam

            append $ bind nam "hCentered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (mkTyp "HCentered" [chType])

handleVCentered :: WidgetElementHandler
handleVCentered =
    WidgetElementHandler genSrc doValidation "vCentered"
        where
          doValidation = V.firstChildWidget

          genSrc nam ch = do
            chNam <- newEntry $ widgetLikeName ch
            gen ch chNam

            append $ bind nam "vCentered" [expr chNam]

            chType <- getWidgetStateType chNam
            return $ declareWidget nam (mkTyp "VCentered" [chType])
