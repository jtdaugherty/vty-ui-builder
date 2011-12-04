module Graphics.Vty.Widgets.Builder.Handlers.ProgressBar
    ( handlers
    )
where

import Control.Applicative
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V

handlers :: [WidgetElementHandler]
handlers = [handleProgressBar]

handleProgressBar :: WidgetElementHandler
handleProgressBar =
    WidgetElementHandler genSrc doValidation "progressBar"
        where
          doValidation s = (,,)
                           <$> V.required s "completeColor"
                           <*> V.required s "incompleteColor"
                           <*> V.optionalInt s "progress"

          genSrc nam (compColor, incompColor, prog) = do
            barName <- newEntry "progressBar"
            append $ bind barName "newProgressBar" [ expr $ mkName compColor
                                                   , expr $ mkName incompColor
                                                   ]
            append $ mkLet [(nam, call "progressBarWidget" [expr barName])]

            case prog of
              Nothing -> return ()
              Just p -> append $ act $ call "setProgress" [expr barName, mkInt p]

            -- The state type is 'Padded' because buttons are
            -- implemented as composite widgets; see the 'Button' type
            -- in Graphics.Vty.Widgets.Button.
            return $ declareWidget nam (mkTyp "Box" [ mkTyp "HFill" []
                                                    , mkTyp "HFill" []
                                                    ])
                       `withField` (barName, parseType "ProgressBar")
