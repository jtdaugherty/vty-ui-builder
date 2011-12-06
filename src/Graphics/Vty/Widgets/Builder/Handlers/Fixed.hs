module Graphics.Vty.Widgets.Builder.Handlers.Fixed
    ( handlers
    )
where

import Control.Applicative ((<$>), (<*>))
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S
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

            append $ S.bind nam "vFixed" [S.mkInt height, S.expr chNam]
            return $ declareWidget nam $
                   S.parseType $ "VFixed (" ++ Hs.prettyPrint chType ++ ")"

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

            append $ S.bind nam "hFixed" [S.mkInt width, S.expr chNam]
            return $ declareWidget nam $
                   S.parseType $ "HFixed (" ++ Hs.prettyPrint chType ++ ")"

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

            append $ S.bind nam "boxFixed" [S.mkInt width, S.mkInt height, S.expr chNam]
            return $ declareWidget nam $
                   S.parseType $ "VFixed (HFixed (" ++ Hs.prettyPrint chType ++ "))"
