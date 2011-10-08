module Graphics.Vty.Widgets.Builder.Validators
    ( validators
    )
where

import Control.Monad
import Text.XML.HaXml.Types

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import Graphics.Vty.Widgets.Builder.ValidateLib

validators :: [(String, ElementValidator)]
validators = [ ("pad", validatePad)
             ]

validatePad :: ElementValidator
validatePad e@(Elem _ attrs _) = do
  when (length attrs == 0) $
       putError e $ "at least one padding attribute must be specified"

  let attrNames = ["top", "bottom", "left", "right"]

  forM_ attrNames $ \attr -> do
    let val = getAttribute e attr
    case (val, val >>= getIntAttributeValue) of
      (Just _, Nothing) ->
          putError e $ "attribute '" ++ attr ++ "' must be an integer"
      _ -> return ()
