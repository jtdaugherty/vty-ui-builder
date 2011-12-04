module Graphics.Vty.Widgets.Builder.AST
    ( Doc(..)
    , Param(..)
    , Interface(..)
    , ModuleImport(..)
    , WidgetLike(..)
    , WidgetElement(..)
    , Reference(..)
    , SourceLocation(..)
    , Element(..)
    , ElementContent(..)
    , WidgetId
    , IsElement(..)
    , HasSourceLocation(..)
    , noLoc
    )
where

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
        , documentSharedWidgets :: [(WidgetId, WidgetElement)]
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
                | Widget WidgetElement
                  deriving (Eq, Read, Show)

data WidgetElement =
    WidgetElement { widgetId :: Maybe String
                  , widgetElement :: Element
                  }
    deriving (Eq, Read, Show)

data Element =
    Element { elementName :: String
            , elementAttributes :: [(String, String)]
            , elementContents :: [ElementContent]
            , elementLocation :: SourceLocation
            }
    deriving (Eq, Read, Show)

data ElementContent = Text String SourceLocation
                    | ChildElement Element
                    | ChildWidgetLike WidgetLike
                      deriving (Eq, Read, Show)

data Reference = Reference WidgetId SourceLocation
                 deriving (Eq, Read, Show)

type WidgetId = String

class IsElement a where
    getElement :: a -> Element

instance IsElement Element where
    getElement = id

instance IsElement WidgetElement where
    getElement = widgetElement

class HasSourceLocation a where
    sourceLocation :: a -> SourceLocation

instance HasSourceLocation Element where
    sourceLocation = elementLocation

instance HasSourceLocation WidgetElement where
    sourceLocation = sourceLocation . getElement

instance HasSourceLocation Param where
    sourceLocation = paramLocation

instance HasSourceLocation Interface where
    sourceLocation = interfaceLocation

instance HasSourceLocation Reference where
    sourceLocation (Reference _ loc) = loc