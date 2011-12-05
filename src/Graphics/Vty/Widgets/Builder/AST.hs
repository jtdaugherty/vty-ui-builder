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

    -- Functions for AST traversal and inspection
    , allRefs
    , allWidgetLikes
    , allWidgetElements
    , validFocusEntries
    , widgetNames
    , getChildElements
    , getChildWidgetLikes
    , widgetElementName
    , getAttributes
    )
where

import Data.Maybe (catMaybes)
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

data Reference =
    Reference { referenceTarget :: WidgetId
              , referenceLocation :: SourceLocation
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

instance HasSourceLocation Element where
    sourceLocation = elementLocation

instance HasSourceLocation WidgetElement where
    sourceLocation = sourceLocation . getElement

instance HasSourceLocation Param where
    sourceLocation = paramLocation

instance HasSourceLocation Interface where
    sourceLocation = interfaceLocation

instance HasSourceLocation Reference where
    sourceLocation = referenceLocation

allRefs :: Doc -> [(Interface, [Reference])]
allRefs doc = [ (iface, interfaceRefs iface)
                | iface <- documentInterfaces doc ]

interfaceRefs :: Interface -> [Reference]
interfaceRefs = catMaybes . map getRef . allWidgetLikes
    where
      getRef (Ref r) = Just r
      getRef (Widget _) = Nothing

allWidgetLikes :: Interface -> [WidgetLike]
allWidgetLikes iface = interfaceContent iface :
                       (wLikes $ interfaceContent iface)
    where
      wLikes (Ref _) = []
      wLikes (Widget w) = elementWLs $ getElement w

      elementWLs = concat . map elemWLs . elementContents

      elemWLs (Text _ _) = []
      elemWLs (ChildElement e) = elementWLs e
      elemWLs (ChildWidgetLike w) = w : wLikes w

allWidgetElements :: Doc -> [WidgetElement]
allWidgetElements doc =
    concat [ map snd $ documentSharedWidgets doc
           , catMaybes $ map getSpec $ concat $ map allWidgetLikes $ documentInterfaces doc
           ]
        where
          getSpec (Ref _) = Nothing
          getSpec (Widget w) = Just w

-- A focus entry in an interface can refer to any widgets explicitly
-- named in that interface OR any named widgets explicitly referenced
-- in that interface.  This way, you can never end up with a focus
-- group with entries which refer to valid widgets which aren't
-- actually displayed in the interface.
validFocusEntries :: Interface -> [WidgetId]
validFocusEntries iface =
    concat [ widgetNames (interfaceContent iface)
           , referenceTarget <$> interfaceRefs iface
           ]

widgetNames :: WidgetLike -> [WidgetId]
widgetNames wlike = catMaybes $ getNamedWidgetNames' wlike
    where
      getNamedWidgetNames' (Ref _) = []
      getNamedWidgetNames' (Widget spec) =
          widgetId spec : (concat $ map getNamedWidgetNames' $ getChildWidgetLikes spec)

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
