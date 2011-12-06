module Graphics.Vty.Widgets.Builder.Handlers.DirBrowser
    ( handlers
    )
where

import Control.Applicative
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S

handlers :: [WidgetElementHandler]
handlers = [handleDirBrowser]

handleDirBrowser :: WidgetElementHandler
handleDirBrowser =
    WidgetElementHandler genSrc doValidation "dirBrowser"
        where
          doValidation s = V.optional s "skin"

          genSrc nam skin = do
            let Just skinName = skin <|> Just "defaultBrowserSkin"

            browserName <- newEntry "browser"
            fgName <- newEntry "focusGroup"
            bData <- newEntry "browserData"
            append $ S.bind bData "newDirBrowser" [S.expr $ S.mkName skinName]

            append $ S.mkLet [ (nam, S.call "dirBrowserWidget" [S.expr browserName])
                             , (browserName, S.call "fst" [S.expr bData])
                             , (fgName, S.call "snd" [S.expr bData])
                             ]

            mergeFocus nam fgName

            return $ declareWidget nam (S.mkTyp "DirBrowserWidgetType" [])
                       `withField` (browserName, S.parseType "DirBrowser")
