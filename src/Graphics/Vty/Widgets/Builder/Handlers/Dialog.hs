module Graphics.Vty.Widgets.Builder.Handlers.Dialog
    ( handlers
    )
where

import Control.Applicative
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V

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
            append $ bind dlgData "newDialog" [ expr chNam
                                              , mkString title
                                              ]
            append $ mkLet [ (nam, call "dialogWidget" [expr dlgName])
                           , (dlgName, call "fst" [expr dlgData])
                           , (fgName, call "snd" [expr dlgData])
                           ]

            mergeFocus nam fgName

            return $ declareWidget nam (parseType "Bordered Padded")
                       `withField` (dlgName, parseType "Dialog")
