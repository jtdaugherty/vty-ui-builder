module Graphics.Vty.Widgets.Builder.Handlers.Button
    ( handlers
    )
where

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V

handlers :: [WidgetElementHandler]
handlers = [handleButton]

handleButton :: WidgetElementHandler
handleButton =
    WidgetElementHandler genSrc doValidation "button"
        where
          doValidation s = V.required s "label"

          genSrc nam label = do
            buttonName <- newEntry "button"

            append $ bind buttonName "newButton" [mkString label]
            append $ mkLet [(nam, call "buttonWidget" [expr buttonName])]

            -- The state type is 'Padded' because buttons are
            -- implemented as composite widgets; see the 'Button' type
            -- in Graphics.Vty.Widgets.Button.
            return $ declareWidget nam (mkTyp "Padded" [])
                       `withField` (buttonName, parseType "Button")
