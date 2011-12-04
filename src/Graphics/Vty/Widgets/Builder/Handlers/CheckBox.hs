module Graphics.Vty.Widgets.Builder.Handlers.CheckBox
    ( handlers
    )
where

import Control.Applicative
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.Validation as V

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
            append $ bind nam "newCheckbox" [mkString label]

            case rg of
              Nothing -> return ()
              Just rgName -> do
                           -- Ensure that we have a radio group by
                           -- this name.
                           fieldValName <- getFieldValueName $ mkName rgName
                           rgValName <- case fieldValName of
                                          Nothing -> do
                                            rgValName <- newEntry "radioGroup"
                                            append $ bind rgValName "newRadioGroup" []
                                            registerFieldValueName (mkName rgName)
                                                                       (VName $ ValueName rgValName $ parseType "RadioGroup")
                                            return rgValName
                                          Just (VName rgv) -> return $ valueName rgv
                                          Just (WName _) -> error "BUG: radio group field value is a widget value!"

                           -- Generate a statement to add the checkbox
                           -- to the radio group.
                           append $ act $ call "addToRadioGroup" [expr rgValName, expr nam]

            return $ declareWidget nam (mkTyp "CheckBox" [mkTyp "Bool" []])
