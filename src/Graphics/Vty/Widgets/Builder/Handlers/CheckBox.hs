module Graphics.Vty.Widgets.Builder.Handlers.CheckBox
    ( handlers
    )
where

import Control.Applicative
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S

handlers :: [WidgetElementHandler]
handlers = [handleCheckBox]

handleCheckBox :: WidgetElementHandler
handleCheckBox =
    WidgetElementHandler genSrc doValidate "checkBox"

        where
          doValidate s = (,)
                         <$> V.required s "label"
                         <*> V.optional s "radioGroup"

          genSrc nam (label, rg) = do
            append $ S.bind nam "newCheckbox" [S.mkString label]

            case rg of
              Nothing -> return ()
              Just rgName -> do
                           -- Ensure that we have a radio group by
                           -- this name.
                           fieldValName <- getFieldValueName $ S.mkName rgName
                           rgValName <- case fieldValName of
                                          Nothing -> do
                                            rgValName <- newEntry "radioGroup"
                                            append $ S.bind rgValName "newRadioGroup" []
                                            registerFieldValueName (S.mkName rgName)
                                                                       (VName $ ValueName rgValName $ S.parseType "RadioGroup")
                                            return rgValName
                                          Just (VName rgv) -> return $ valueName rgv
                                          Just (WName _) -> error "BUG: radio group field value is a widget value!"

                           -- Generate a statement to add the checkbox
                           -- to the radio group.
                           append $ S.act $ S.call "addToRadioGroup" [S.expr rgValName, S.expr nam]

            return $ declareWidget nam (S.mkTyp "CheckBox" [S.mkTyp "Bool" []])
