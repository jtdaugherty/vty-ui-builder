module Graphics.Vty.Widgets.Builder.Handlers.Edit
    ( handlers
    )
where

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V

handlers :: [WidgetElementHandler]
handlers = [handleEdit]

handleEdit :: WidgetElementHandler
handleEdit =
    WidgetElementHandler genSrc doValidation "edit"
        where
          doValidation e = V.optional e "contents"

          genSrc nam contents = do
            append $ bind nam "editWidget" []

            case contents of
              Nothing -> return ()
              Just s -> append $ act $ call "setEditText" [ expr nam
                                                          , mkString s
                                                          ]

            return $ declareWidget nam (mkTyp "Edit" [])
