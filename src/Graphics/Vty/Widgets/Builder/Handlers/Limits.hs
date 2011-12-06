module Graphics.Vty.Widgets.Builder.Handlers.Limits
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

            append $ S.bind nam "vLimit" [S.mkInt height, S.expr chNam]
            return $ declareWidget nam $
                   S.parseType $ "VLimit (" ++ Hs.prettyPrint chType ++ ")"

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

            append $ S.bind nam "hLimit" [S.mkInt width, S.expr chNam]
            return $ declareWidget nam $
                   S.parseType $ "HLimit (" ++ Hs.prettyPrint chType ++ ")"

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

            append $ S.bind nam "boxLimit" [ S.mkInt width
                                         , S.mkInt height
                                         , S.expr chNam
                                         ]
            return $ declareWidget nam $
                   S.parseType $ "VLimit (HLimit (" ++ Hs.prettyPrint chType ++ "))"
