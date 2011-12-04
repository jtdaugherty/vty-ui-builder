module Graphics.Vty.Widgets.Builder.Handlers.Fills
    ( handlers
    )
where

import Control.Applicative ((<$>), (<*>))
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V

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
            append $ bind nam "vFill" [mkChar ch]
            return $ declareWidget nam (mkTyp "VFill" [])

handleHFill :: WidgetElementHandler
handleHFill =
    WidgetElementHandler genSrc doValidation "hFill"
        where
          doValidation s = (,)
                           <$> V.requiredChar s "char"
                           <*> V.requiredInt s "height"

          genSrc nam (ch, height) = do
            append $ bind nam "hFill" [ mkChar ch
                                      , mkInt height
                                      ]

            return $ declareWidget nam (mkTyp "HFill" [])
