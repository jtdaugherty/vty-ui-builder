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
    , WidgetId
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
        , documentSharedWidgets :: [WidgetSpec]
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

data WidgetSpecContent = Text String SourceLocation
                       | Child WidgetLike
                         deriving (Eq, Read, Show)

data Reference = Reference WidgetId
                 deriving (Eq, Read, Show)

type WidgetId = String