module Graphics.Vty.Widgets.Builder.Handlers.Wrap
    ( handlers
    )
where

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S

handlers :: [WidgetElementHandler]
handlers = [handleWrap]

handleWrap :: WidgetElementHandler
handleWrap =
    WidgetElementHandler genSrc doValidation "wrap"
        where
          doValidation s = V.requireWidgetName "fText" =<<
                           V.firstChildWidget s

          genSrc nam ch = do
            gen ch nam
            tempNam <- newEntry "formattedText"
            append $ S.bind tempNam "getTextFormatter" [S.expr nam]
            append $ S.act $ S.call "setTextFormatter"
                       [ S.expr nam
                       , S.parens (S.opApp (S.expr tempNam) (S.mkName "mappend") (S.expr $ S.mkName "wrap"))
                       ]

            -- NB: this is a no-op because the child element handler
            -- will have already registered a type for 'nam'.
            -- However, we are required to return something from this
            -- handler because it is a widget element handler, so we
            -- just return the same thing the child would have
            -- returned.  Ultimately the name and type declared here
            -- will be ignored because they'll be appended onto the
            -- list of registered widget names and won't be reached by
            -- 'lookup' calls.
            ty <- getWidgetStateType nam
            return $ declareWidget nam ty
