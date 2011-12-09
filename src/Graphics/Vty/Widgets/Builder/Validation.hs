module Graphics.Vty.Widgets.Builder.Validation
    (
    validateDocument

    -- Validation functions
    , required
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
import Control.Monad (forM_)
import Data.List (nub)
import Data.Maybe

import Graphics.Vty.Widgets.Builder.Types
import qualified Graphics.Vty.Widgets.Builder.AST as A
import Graphics.Vty.Widgets.Builder.GenLib
    ( getAttribute
    )

validateDocument :: A.Doc
                 -> [WidgetElementHandler]
                 -> Either [Error] ()
validateDocument doc theHandlers =
    case validateReferences doc of
      Left es -> Left es
      Right _ -> let msgs = concat $ getMsgs <$> A.documentInterfaces doc
                 in if null msgs
                    then Right ()
                    else Left msgs
          where
            -- Match up widget specs in the document with handlers
            handlersBySpecType = map (\s -> (specType s, s)) theHandlers

            mkMapping iface = map (\s -> (s, Just iface, lookup (A.widgetElementName s) handlersBySpecType)) $
                              (A.interfaceWidgetElements iface)

            mkMsg s = "unknown widget type " ++ show (A.widgetElementName s)

            process (s, _, Nothing) = Just $ Error (A.sourceLocation s) $ mkMsg s
            process (s, iface, Just h) = doSpecValidation h s $ ValidationState iface doc

            getMsgs iface = catMaybes $ process <$> mkMapping iface

validateReferences :: A.Doc -> Either [Error] ()
validateReferences doc = do
  checkValidRefTargets
  checkFocusGroups
  forM_ (A.documentInterfaces doc) $ \i ->
      forM_ (A.interfaceWidgetReferences i) $ \ref ->
          case resolveRef doc i ref of
            Left e -> Left [e]
            Right _ -> Right ()

        where
          -- Group each valid reference target (widget ID) with the
          -- list of specs which claimed the ID.  Right now the IDs
          -- must be unique over the whole document, so if any ID maps
          -- to more than one widget spec, that's illegal.
          mapping iface = [ (wId, snd <$> filter ((== wId) . fst) named)
                            | wId <- nub $ map fst named
                          ]
              where
                named = A.interfaceNamedWidgets iface

          duplicateRefs = filter ((> 1) . length . snd)

          checkValidRefTargets :: Either [Error] ()
          checkValidRefTargets =
              case concat $ duplicateRefs <$> mapping <$> A.documentInterfaces doc of
                [] -> Right ()
                dups -> Left $ concat $ map mkErrors dups

          -- Check that each focus groups' entries reference valid
          -- targets for the interface in which they are found.
          checkFocusGroups :: Either [Error] ()
          checkFocusGroups = forM_ (A.documentInterfaces doc) $ \iface ->
                             forM_ (A.interfaceFocusEntries iface) $ \(A.FocusReference wId loc) ->
                             case wId `elem` (A.validFocusNames iface) of
                               False -> Left [Error loc $ "Focus group entry " ++ show wId ++
                                                        " not referenced or defined in interface " ++
                                                        A.interfaceName iface]
                               True -> Right ()

          mkErrors (wId, specs) =
              [ Error (A.sourceLocation s) $ "Duplicate widget ID " ++ show wId ++ " defined"
                | s <- specs
              ]

data ResolvedReference = ResolvedWidget A.WidgetElement
                       | ResolvedParameter A.WidgetId

resolveRef :: A.Doc -> A.Interface -> A.WidgetReference -> Either Error ResolvedReference
resolveRef doc iface (A.WidgetReference nam loc refType) =
    case refType of
      A.InterfaceWidgetRef ->
          case lookup nam $ A.interfaceNamedWidgets iface of
            Nothing -> Left $ Error loc $ "Widget reference for " ++ show nam
                             ++ " is invalid; no widget with that ID defined in interface "
                             ++ (show $ A.interfaceName iface)
            Just e -> return $ ResolvedWidget e
      A.ParameterRef ->
          case nam `elem` A.paramNames doc of
            True -> return $ ResolvedParameter nam
            False ->
                Left $ Error loc $ "Parameter reference named " ++ show nam
                      ++ " is invalid; no such parameter"
      A.SharedWidgetRef ->
          case lookup nam $ A.documentSharedWidgets doc of
            Nothing -> Left $ Error loc $ "Shared widget reference for " ++ show nam
                             ++ " is invalid; no shared widget with that ID defined in document"
            Just e -> return $ ResolvedWidget e

doSpecValidation :: WidgetElementHandler
                 -> A.WidgetElement
                 -> ValidationState
                 -> Maybe Error
doSpecValidation (WidgetElementHandler _ validator _) spec st =
    case runValidation (validator $ A.getElement spec) st of
      ValidationError e -> Just e
      Valid _ -> Nothing

required :: (A.HasSourceLocation a, A.IsElement a) =>
            a -> String -> ValidateM String
required thing attrName =
    case getAttribute thing attrName of
      Nothing -> failValidation thing $ "attribute " ++ show attrName ++ " required"
      Just val -> return val

optional :: (A.IsElement a) => a -> String -> ValidateM (Maybe String)
optional val attr = return $ getAttribute val attr

requiredEqual :: (A.HasSourceLocation a, A.IsElement a) =>
                 a -> String -> String -> ValidateM String
requiredEqual spec attrName expected = do
  v <- required spec attrName
  if v == expected then
      return v else
      failValidation spec $ "Attribute value for attribute " ++
                     show attrName ++ " must be " ++ show expected

requireWidgetName :: String -> A.WidgetLike -> ValidateM A.WidgetLike
requireWidgetName typ wl@(A.WidgetRef ref) = do
  doc <- getValidatingDocument
  iface' <- getValidatingInterface

  case iface' of
    Nothing -> failValidation ref $
               "Widget references not permitted outside of an interface"
    Just iface -> do
      case resolveRef doc iface ref of
        Left e -> failValidation' e
        Right (ResolvedParameter _) ->
            failValidation ref $ "Expected widget or reference to widget of type " ++ (show typ)
                               ++ ", got parameter reference instead"
        Right (ResolvedWidget e) -> do
                    if A.widgetElementName e == typ then
                        return wl else
                        failValidation ref $ "Expected a reference to a widget of type " ++
                                       show typ ++ ", got a reference to type " ++
                                       (show $ A.widgetElementName e) ++
                                       " (defined at " ++ (show $ A.sourceLocation e) ++ ")"
requireWidgetName typ wl@(A.Widget e) =
    if A.widgetElementName e == typ then
        return wl else
        failValidation e $ "Expected a widget of type " ++
                       show typ ++ ", got a widget of type " ++
                                (show $ A.widgetElementName e)

requireValidColor :: A.SourceLocation -> Maybe String -> ValidateM (Maybe String)
requireValidColor loc s =
    case s of
      Nothing -> return Nothing
      Just c -> if c `elem` validColors
                then return $ Just c
                else failValidation loc $ "Color " ++ show c ++
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
    case A.getChildWidgetLikes val of
      (ch:_) -> return ch
      _ -> failValidation val "required first child widget is missing"

requiredInt :: (A.HasSourceLocation a, A.IsElement a) => a -> String -> ValidateM Int
requiredInt thing attrName = required thing attrName >>= getInt thing attrName

requiredChar :: (A.HasSourceLocation a, A.IsElement a) => a -> String -> ValidateM Char
requiredChar val attrName = do
  s <- required val attrName
  case null s of
    True -> failValidation val $
            "Attribute " ++ show attrName ++ " must be non-empty"
    False -> if length s == 1 then
                 return $ head s else
                 failValidation val $
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
    else failValidation e $ "expected element of type "
             ++ show s ++ ", got " ++ show (A.elementName e)

getInt :: (A.HasSourceLocation a) => a -> String -> String -> ValidateM Int
getInt thing attrName val =
    case reads val of
      [] -> failValidation thing $ "Attribute " ++ show attrName ++
            " value must be an integer"
      ((v,_):_) -> return v
