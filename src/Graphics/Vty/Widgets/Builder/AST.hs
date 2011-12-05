module Graphics.Vty.Widgets.Builder.AST
    ( Doc(..)
    , Param(..)
    , Interface(..)
    , ModuleImport(..)
    , WidgetLike(..)
    , WidgetElement(..)
    , WidgetReference(..)
    , ReferenceType(..)
    , FocusReference(..)
    , SourceLocation(..)
    , Element(..)
    , ElementContent(..)
    , WidgetId
    , IsElement(..)
    , HasSourceLocation(..)
    , noLoc

    -- Functions for AST traversal and inspection
    , interfaceWidgetLikes
    , interfaceWidgetElements
    , interfaceNamedWidgets
    , interfaceWidgetReferences
    , validFocusNames
    , getChildElements
    , getChildWidgetLikes
    , widgetElementName
    , getAttributes
    , paramNames
    )
where

import Data.Maybe (catMaybes, fromJust, isJust)
import Control.Applicative ((<$>))

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
              , interfaceFocusEntries :: [FocusReference]
              , interfaceLocation :: SourceLocation
              }
    deriving (Eq, Read, Show)

data WidgetLike = WidgetRef WidgetReference
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

data ReferenceType = ParameterRef
                   | InterfaceWidgetRef
                   | SharedWidgetRef
                     deriving (Eq, Read, Show)

data FocusReference =
    FocusReference { focusReferenceTarget :: WidgetId
                   , focusReferenceLocation :: SourceLocation
                   }
    deriving (Eq, Read, Show)

data WidgetReference =
    WidgetReference { referenceTarget :: WidgetId
                    , referenceLocation :: SourceLocation
                    , referenceType :: ReferenceType
                    }
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

instance HasSourceLocation SourceLocation where
    sourceLocation = id

instance HasSourceLocation Element where
    sourceLocation = elementLocation

instance HasSourceLocation WidgetElement where
    sourceLocation = sourceLocation . getElement

instance HasSourceLocation Param where
    sourceLocation = paramLocation

instance HasSourceLocation Interface where
    sourceLocation = interfaceLocation

instance HasSourceLocation WidgetReference where
    sourceLocation = referenceLocation

instance HasSourceLocation FocusReference where
    sourceLocation = focusReferenceLocation

interfaceWidgetReferences :: Interface -> [WidgetReference]
interfaceWidgetReferences = catMaybes . map getRef . interfaceWidgetLikes
    where
      getRef (WidgetRef r) = Just r
      getRef (Widget _) = Nothing

interfaceWidgetLikes :: Interface -> [WidgetLike]
interfaceWidgetLikes iface = interfaceContent iface :
                       (wLikes $ interfaceContent iface)
    where
      wLikes (WidgetRef _) = []
      wLikes (Widget w) = elementWLs $ getElement w

      elementWLs = concat . map elemWLs . elementContents

      elemWLs (Text _ _) = []
      elemWLs (ChildElement e) = elementWLs e
      elemWLs (ChildWidgetLike w) = w : wLikes w

interfaceWidgetElements :: Interface -> [WidgetElement]
interfaceWidgetElements =
    catMaybes . map getSpec . interfaceWidgetLikes
        where
          getSpec (WidgetRef _) = Nothing
          getSpec (Widget w) = Just w

interfaceNamedWidgets :: Interface -> [(WidgetId, WidgetElement)]
interfaceNamedWidgets iface =
    [ (fromJust $ widgetId s, s)
      | s <- interfaceWidgetElements iface
    , isJust $ widgetId s
    ]

-- A focus entry in an interface can refer to any widgets explicitly
-- named in that interface OR any named widgets explicitly referenced
-- in that interface.  This way, you can never end up with a focus
-- group with entries which refer to valid widgets which aren't
-- actually displayed in the interface.
validFocusNames :: Interface -> [WidgetId]
validFocusNames iface =
    concat [ catMaybes $ widgetId <$> interfaceWidgetElements iface
           , referenceTarget <$> interfaceWidgetReferences iface
           ]

widgetElementName :: WidgetElement -> String
widgetElementName = elementName . getElement

getAttributes :: (IsElement e) => e -> [(String, String)]
getAttributes = elementAttributes . getElement

getChildWidgetLikes :: (IsElement e) => e -> [WidgetLike]
getChildWidgetLikes = catMaybes . map getWL . elementContents . getElement
    where
      getWL (ChildWidgetLike w) = Just w
      getWL _ = Nothing

getChildElements :: (IsElement e) => e -> [Element]
getChildElements = catMaybes . map getEls . elementContents . getElement
    where
      getEls (ChildElement el) = Just el
      getEls _ = Nothing

paramNames :: Doc -> [WidgetId]
paramNames = (paramName <$>) . documentParams