module Graphics.Vty.Widgets.Builder.Handlers.DirBrowser
    ( handlers
    )
where

import Control.Applicative
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V

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
            append $ bind bData "newDirBrowser" [expr $ mkName skinName]

            append $ mkLet [ (nam, call "dirBrowserWidget" [expr browserName])
                           , (browserName, call "fst" [expr bData])
                           , (fgName, call "snd" [expr bData])
                           ]

            mergeFocus nam fgName

            return $ declareWidget nam (mkTyp "DirBrowserWidgetType" [])
                       `withField` (browserName, parseType "DirBrowser")
