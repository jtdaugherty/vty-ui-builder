module Graphics.Vty.Widgets.Builder.Handlers.Dialog
    ( handlers
    )
where

import Control.Applicative
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S

handlers :: [WidgetElementHandler]
handlers = [handleDialog]

handleDialog :: WidgetElementHandler
handleDialog =
    WidgetElementHandler genSrc doValidation "dialog"
        where
          doValidation s = (,)
                           <$> V.required s "title"
                           <*> V.firstChildWidget s

          genSrc nam (title, ch) = do
            chNam <- newEntry $ widgetLikeName ch
            gen ch chNam

            dlgName <- newEntry "dialog"
            fgName <- newEntry "focusGroup"

            dlgData <- newEntry "dialogData"
            append $ S.bind dlgData "newDialog" [ S.expr chNam
                                                , S.mkString title
                                                ]
            append $ S.mkLet [ (nam, S.call "dialogWidget" [S.expr dlgName])
                             , (dlgName, S.call "fst" [S.expr dlgData])
                             , (fgName, S.call "snd" [S.expr dlgData])
                             ]

            mergeFocus nam fgName

            return $ declareWidget nam (S.parseType "Bordered Padded")
                       `withField` (dlgName, S.parseType "Dialog")
