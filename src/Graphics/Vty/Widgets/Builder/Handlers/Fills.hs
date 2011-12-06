module Graphics.Vty.Widgets.Builder.Handlers.Fills
    ( handlers
    )
where

import Control.Applicative ((<$>), (<*>))
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S

handlers :: [WidgetElementHandler]
handlers = [ handleVFill
           , handleHFill
           ]

handleVFill :: WidgetElementHandler
handleVFill =
    WidgetElementHandler genSrc doValidation "vFill"
        where
          doValidation s = V.requiredChar s "char"

          genSrc nam ch = do
            append $ S.bind nam "vFill" [S.mkChar ch]
            return $ declareWidget nam (S.mkTyp "VFill" [])

handleHFill :: WidgetElementHandler
handleHFill =
    WidgetElementHandler genSrc doValidation "hFill"
        where
          doValidation s = (,)
                           <$> V.requiredChar s "char"
                           <*> V.requiredInt s "height"

          genSrc nam (ch, height) = do
            append $ S.bind nam "hFill" [ S.mkChar ch
                                        , S.mkInt height
                                        ]

            return $ declareWidget nam (S.mkTyp "HFill" [])
