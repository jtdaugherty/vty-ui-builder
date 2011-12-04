module Graphics.Vty.Widgets.Builder.Handlers.Fixed
    ( handlers
    )
where

import Control.Applicative ((<$>), (<*>))
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Language.Haskell.Exts as Hs

handlers :: [WidgetElementHandler]
handlers = [ handleVFixed
           , handleHFixed
           , handleBoxFixed
           ]

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
