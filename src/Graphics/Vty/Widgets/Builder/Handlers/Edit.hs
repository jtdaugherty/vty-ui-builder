module Graphics.Vty.Widgets.Builder.Handlers.Edit
    ( handlers
    )
where

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S

handlers :: [WidgetElementHandler]
handlers = [handleEdit]

handleEdit :: WidgetElementHandler
handleEdit =
    WidgetElementHandler genSrc doValidation "edit"
        where
          doValidation e = V.optional e "contents"

          genSrc nam contents = do
            append $ S.bind nam "editWidget" []

            case contents of
              Nothing -> return ()
              Just s -> append $ S.act $ S.call "setEditText" [ S.expr nam
                                                              , S.mkString s
                                                              ]

            return $ declareWidget nam (S.mkTyp "Edit" [])
