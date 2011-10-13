module Graphics.Vty.Widgets.Builder.ValidateLib
    ( doValidation
    , validate
    , putError
    , (&.&)
    )
where

import Control.Monad.State
import Data.Maybe

import Text.XML.HaXml.Posn
import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Util

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.Util

doValidation :: Element Posn -> [ElementHandler] -> IO [String]
doValidation e hs = do
  let initState = ValidationState [] validators
      handlersWithValidators = filter (isJust . validator) hs
      validators = map (\h -> (elementName h, fromJust $ validator h))
                   handlersWithValidators
      elements = map contentElem $ (multi elm) (CElem e noPos)
  (_, st) <- runStateT (mapM validate elements) initState
  return $ errors st

validate :: Element Posn -> ValidateM ()
validate e = do
  vs <- gets theValidators
  case lookup (elemName e) vs of
    Nothing -> return ()
    Just f -> f e

putError :: Element Posn -> String -> ValidateM ()
putError e msg = do
  let fullMsg = elemName e ++ ": " ++ msg

  es <- gets errors
  modify $ \st -> st { errors = es ++ [fullMsg] }

(&.&) :: ElementValidator -> ElementValidator -> ElementValidator
a &.& b = \e -> a e >> b e
