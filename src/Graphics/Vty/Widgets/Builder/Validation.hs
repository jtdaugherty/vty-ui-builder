module Graphics.Vty.Widgets.Builder.Validation
    ( required
    , requiredInt
    , requiredChar
    , optional
    , optionalInt
    , requiredEqual
    , firstChildWidget
    , elementsByName
    , requireWidgetName
    , requireValidColor
    , validColors
    , getInt
    , elemName
    )
where

import Control.Applicative ((<$>))

import Graphics.Vty.Widgets.Builder.Types
import qualified Graphics.Vty.Widgets.Builder.AST as A
import Graphics.Vty.Widgets.Builder.GenLib
    ( getAttribute
    , mkName
    , widgetElementName
    , getChildWidgetLikes
    )

required :: (A.HasSourceLocation a, A.IsElement a) =>
            a -> String -> ValidateM String
required thing attrName =
    case getAttribute thing attrName of
      Nothing -> failValidation $ Error (A.sourceLocation thing) $
                 "attribute " ++ show attrName ++ " required"
      Just val -> return val

optional :: (A.IsElement a) => a -> String -> ValidateM (Maybe String)
optional val attr = return $ getAttribute val attr

requiredEqual :: (A.HasSourceLocation a, A.IsElement a) =>
                 a -> String -> String -> ValidateM String
requiredEqual spec attrName expected = do
  v <- required spec attrName
  if v == expected then
      return v else
      failValidation $ Error (A.sourceLocation spec) $
                         "Attribute value for attribute " ++
                         show attrName ++ " must be " ++ show expected

requireWidgetName :: String -> A.WidgetLike -> ValidateM A.WidgetLike
requireWidgetName typ wl@(A.Ref (A.Reference nam loc)) = do
  refs <- getResolvedRefs
  -- Assume the lookup will succeed, since all references are valid in
  -- this monad.
  let Just targetSpec = lookup (mkName nam) refs
  if widgetElementName targetSpec == typ then
      return wl else
      failValidation $ Error loc $ "Expected a reference to a widget of type " ++
                     show typ ++ ", got a reference to type " ++
                              (show $ widgetElementName targetSpec)
requireWidgetName typ wl@(A.Widget spec) =
    if widgetElementName spec == typ then
        return wl else
        failValidation $ Error (A.sourceLocation spec) $ "Expected a widget of type " ++
                       show typ ++ ", got a widget of type " ++
                                (show $ widgetElementName spec)

requireValidColor :: A.SourceLocation -> Maybe String -> ValidateM (Maybe String)
requireValidColor loc s =
    case s of
      Nothing -> return Nothing
      Just c -> if c `elem` validColors
                then return $ Just c
                else failValidation $ Error loc $ "Color " ++ show c ++
                         " invalid, must be one of " ++ show validColors

validColors :: [String]
validColors =
    [ "red"
    , "green"
    , "yellow"
    , "blue"
    , "magenta"
    , "cyan"
    , "white"
    , "black"
    , "bright_red"
    , "bright_green"
    , "bright_yellow"
    , "bright_black"
    , "bright_magenta"
    , "bright_cyan"
    , "bright_white"
    , "bright_blue"
    ]

firstChildWidget :: (A.IsElement a, A.HasSourceLocation a) =>
                    a -> ValidateM A.WidgetLike
firstChildWidget val =
    case getChildWidgetLikes val of
      (ch:_) -> return ch
      _ -> failValidation $ Error (A.sourceLocation val) "required first child widget is missing"

requiredInt :: (A.HasSourceLocation a, A.IsElement a) => a -> String -> ValidateM Int
requiredInt thing attrName = required thing attrName >>= getInt thing attrName

requiredChar :: (A.HasSourceLocation a, A.IsElement a) => a -> String -> ValidateM Char
requiredChar val attrName = do
  s <- required val attrName
  case null s of
    True -> failValidation $ Error (A.sourceLocation val) $
            "Attribute " ++ show attrName ++ " must be non-empty"
    False -> if length s == 1 then
                 return $ head s else
                 failValidation $ Error (A.sourceLocation val) $
                   "Attribute " ++ show attrName ++ " must be one character in length"

optionalInt :: (A.IsElement a, A.HasSourceLocation a) =>
               a -> String -> ValidateM (Maybe Int)
optionalInt spec attrName =
    case getAttribute spec attrName of
      Just v -> Just <$> getInt spec attrName v
      Nothing -> return Nothing

elementsByName :: String -> [A.Element] -> [A.Element]
elementsByName n es = filter ((== n) . A.elementName) es

elemName :: A.Element -> String -> ValidateM A.Element
elemName e s =
    if A.elementName e == s
    then return e
    else failValidation $ Error (A.elementLocation e) $ "expected element of type "
             ++ show s ++ ", got " ++ show (A.elementName e)

getInt :: (A.HasSourceLocation a) => a -> String -> String -> ValidateM Int
getInt thing attrName val =
    case reads val of
      [] -> failValidation $ Error (A.sourceLocation thing) $
            "Attribute " ++ show attrName ++
            " value must be an integer"
      ((v,_):_) -> return v
