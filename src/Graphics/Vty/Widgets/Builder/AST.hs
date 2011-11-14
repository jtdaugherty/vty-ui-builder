module Graphics.Vty.Widgets.Builder.AST
    ( Doc(..)
    , Param(..)
    , Interface(..)
    , WidgetLike(..)
    , WidgetSpec(..)
    , WidgetSpecContent(..)
    , Reference(..)
    , SourceLocation(..)
    , WidgetId
    )
where

data SourceLocation =
    SourceLocation { srcFile :: FilePath
                   , srcLine :: Int
                   , srcColumn :: Int
                   }
    deriving (Eq, Read, Show)

data Doc =
    Doc { documentInterfaces :: [Interface]
        , documentParams :: [Param]
        , documentSharedElements :: [WidgetSpec]
        }
    deriving (Eq, Read, Show)

data Param =
    Param { paramName :: WidgetId
          , paramType :: String
          , paramLocation :: SourceLocation
          }
    deriving (Eq, Read, Show)

data Interface =
    Interface { interfaceName :: String
              , interfaceContent :: WidgetLike
              , interfaceFocusEntries :: [WidgetId]
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

data WidgetSpecContent = Plain String SourceLocation
                       | Child WidgetLike
                         deriving (Eq, Read, Show)

data Reference = Reference WidgetId
                 deriving (Eq, Read, Show)

type WidgetId = String