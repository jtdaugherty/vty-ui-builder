module Graphics.Vty.Widgets.Builder.Handlers.Limits
    ( handlers
    )
where

import Control.Applicative ((<$>), (<*>))
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Language.Haskell.Exts as Hs

handlers :: [WidgetElementHandler]
handlers = [ handleVLimit
           , handleHLimit
           , handleBoxLimit
           ]

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
