module Graphics.Vty.Widgets.Builder.AST
    ( Doc(..)
    , Param(..)
    , Interface(..)
    , ModuleImport(..)
    , WidgetLike(..)
    , WidgetSpec(..)
    , WidgetSpecContent(..)
    , Reference(..)
    , SourceLocation(..)
    , Element(..)
    , ElementContent(..)
    , WidgetId
    , HasAttributes(..)
    , HasChildElements(..)
    , HasChildWidgetLikes(..)
    , HasSourceLocation(..)
    , noLoc
    )
where

import Data.Maybe (catMaybes)

data SourceLocation =
    SourceLocation { srcFile :: FilePath
                   , srcLine :: Int
                   , srcColumn :: Int
                   }
    deriving (Eq, Read)

instance Show SourceLocation where
    show loc = concat [ srcFile loc
                      , ":"
                      , show $ srcLine loc
                      , ":"
                      , show $ srcColumn loc
                      ]

noLoc :: SourceLocation
noLoc = SourceLocation "" 0 0

data Doc =
    Doc { documentInterfaces :: [Interface]
        , documentParams :: [Param]
        , documentSharedWidgets :: [(WidgetId, WidgetSpec)]
        , documentImports :: [ModuleImport]
        }
    deriving (Eq, Read, Show)

data Param =
    Param { paramName :: WidgetId
          , paramType :: String
          , paramLocation :: SourceLocation
          }
    deriving (Eq, Read, Show)

data ModuleImport =
    ModuleImport { importModuleName :: String
                 }
    deriving (Eq, Read, Show)

data Interface =
    Interface { interfaceName :: String
              , interfaceContent :: WidgetLike
              , interfaceFocusEntries :: [(WidgetId, SourceLocation)]
              , interfaceLocation :: SourceLocation
              }
    deriving (Eq, Read, Show)

data WidgetLike = Ref Reference
                | Widget WidgetSpec
                  deriving (Eq, Read, Show)

data WidgetSpec =
    WidgetSpec { widgetType :: String
               , widgetId :: Maybe String
               , widgetSpecAttributes :: [(String, String)]
               , widgetSpecContents :: [WidgetSpecContent]
               , widgetLocation :: SourceLocation
               }
    deriving (Eq, Read, Show)

data Element =
    Element { elementType :: String
            , elementAttributes :: [(String, String)]
            , elementContents :: [ElementContent]
            , elementLocation :: SourceLocation
            }
    deriving (Eq, Read, Show)

data ElementContent = ElemText String SourceLocation
                    | ElemChild Element
                    | ElemChildWidgetLike WidgetLike
                      deriving (Eq, Read, Show)

data WidgetSpecContent = Text String SourceLocation
                       | ChildWidgetLike WidgetLike
                       | ChildElement Element
                         deriving (Eq, Read, Show)

data Reference = Reference WidgetId SourceLocation
                 deriving (Eq, Read, Show)

type WidgetId = String

class HasAttributes a where
    getAttributes :: a -> [(String, String)]

class HasChildWidgetLikes a where
    getChildWidgetLikes :: a -> [WidgetLike]

class HasChildElements a where
    getChildElements :: a -> [Element]

class HasSourceLocation a where
    getSourceLocation :: a -> SourceLocation

instance HasAttributes Element where
    getAttributes = elementAttributes

instance HasAttributes WidgetSpec where
    getAttributes = widgetSpecAttributes

instance HasChildWidgetLikes Element where
    getChildWidgetLikes e = catMaybes $ map getWL $ elementContents e
        where
          getWL (ElemChildWidgetLike w) = Just w
          getWL _ = Nothing

instance HasChildWidgetLikes WidgetSpec where
    getChildWidgetLikes e = catMaybes $ map getWL $ widgetSpecContents e
        where
          getWL (ChildWidgetLike w) = Just w
          getWL _ = Nothing

instance HasChildElements Element where
    getChildElements e = catMaybes $ map getWL $ elementContents e
        where
          getWL (ElemChild el) = Just el
          getWL _ = Nothing

instance HasChildElements WidgetSpec where
    getChildElements e = catMaybes $ map getWL $ widgetSpecContents e
        where
          getWL (ChildElement el) = Just el
          getWL _ = Nothing

instance HasSourceLocation Element where
    getSourceLocation = elementLocation

instance HasSourceLocation WidgetSpec where
    getSourceLocation = widgetLocation