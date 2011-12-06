module Graphics.Vty.Widgets.Builder.Handlers.ProgressBar
    ( handlers
    )
where

import Control.Applicative
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S

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
            append $ S.bind barName "newProgressBar" [ S.expr $ S.mkName compColor
                                                     , S.expr $ S.mkName incompColor
                                                     ]
            append $ S.mkLet [(nam, S.call "progressBarWidget" [S.expr barName])]

            case prog of
              Nothing -> return ()
              Just p -> append $ S.act $ S.call "setProgress" [S.expr barName, S.mkInt p]

            -- The state type is 'Padded' because buttons are
            -- implemented as composite widgets; see the 'Button' type
            -- in Graphics.Vty.Widgets.Button.
            return $ declareWidget nam (S.mkTyp "Box" [ S.mkTyp "HFill" []
                                                      , S.mkTyp "HFill" []
                                                      ])
                       `withField` (barName, S.parseType "ProgressBar")
