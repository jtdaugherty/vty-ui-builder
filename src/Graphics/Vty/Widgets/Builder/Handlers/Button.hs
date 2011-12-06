module Graphics.Vty.Widgets.Builder.Handlers.Button
    ( handlers
    )
where

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S

handlers :: [WidgetElementHandler]
handlers = [handleButton]

handleButton :: WidgetElementHandler
handleButton =
    WidgetElementHandler genSrc doValidation "button"
        where
          doValidation s = V.required s "label"

          genSrc nam label = do
            buttonName <- newEntry "button"

            append $ S.bind buttonName "newButton" [S.mkString label]
            append $ S.mkLet [(nam, S.call "buttonWidget" [S.expr buttonName])]

            -- The state type is 'Padded' because buttons are
            -- implemented as composite widgets; see the 'Button' type
            -- in Graphics.Vty.Widgets.Button.
            return $ declareWidget nam (S.mkTyp "Padded" [])
                       `withField` (buttonName, S.parseType "Button")
