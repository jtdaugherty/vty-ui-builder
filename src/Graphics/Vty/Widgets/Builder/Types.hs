{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.Vty.Widgets.Builder.Types
    ( GenState(..)
    , GenM
    , AnyName(..)
    , ElementHandler(..)
    , ValueName(..)
    , WidgetName(..)
    , InterfaceValues(..)
    , FocusMethod(..)
    , ValidatedElement(..)
    , WidgetHandlerResult(..)
    , ElementValidator
    , ValidationState(..)
    , ValidateM
    , WidgetElementSourceGenerator
    , StructureElementSourceGenerator
    , AnyElementSourceGenerator(..)
    )
where

import Control.Monad.State
import qualified Data.Map as Map
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn

import qualified Language.Haskell.Exts.Syntax as Hs

-- Representation of general (non-widget) values with custom types.
data ValueName = ValueName { valueName :: Hs.Name
                           , valueType :: Hs.Type
                           }
                 deriving (Eq, Show)

data WidgetName = WidgetName { widgetName :: Hs.Name
                             , widgetType :: Hs.Type
                             }
                  deriving (Eq, Show)

-- Sum type of different names to be used when either type of name is
-- appropriate (focus method instructions, interface element field
-- types, etc).
data AnyName = WName WidgetName
             | VName ValueName
               deriving (Eq, Show)

data ValidatedElement = Validated (Element Posn)

data InterfaceValues =
    InterfaceValues { topLevelWidgetName :: Hs.Name
                    , switchActionName :: Hs.Name
                    , focusGroupName :: Hs.Name
                    }

data GenState =
    GenState { nameCounters :: Map.Map String Int
             , hsStatements :: [Hs.Stmt]
             , handlers :: [(String, AnyElementSourceGenerator)]
             , allWidgetNames :: [(Hs.Name, WidgetName)]
             , registeredFieldNames :: [(Hs.Name, AnyName)]
             , interfaceNames :: [(String, InterfaceValues)]
             , focusMethods :: [(Hs.Name, FocusMethod)]
             , focusValues :: [(Hs.Name, WidgetName)]
             , imports :: [Hs.ImportDecl]
             , paramNames :: [(Hs.Name, Hs.Type)]
             -- Used to build up a list of registered values' names so
             -- that focus group entry names can be resolved to the
             -- current interface being processed, only
             , tempRegisteredNames :: [Hs.Name]
             }

data FocusMethod = Direct WidgetName -- The name of the widget which
                                     -- should be added to the primary
                                     -- focus group.
                 | Merge Hs.Name -- The name of the focus group value
                                 -- that should be merged into the
                                 -- primary focus group.

data ElementHandler =
    WidgetElementHandler { generateWidgetSource :: WidgetElementSourceGenerator
                         , validator :: Maybe ElementValidator
                         , elementName :: String
                         }
        | StructureElementHandler { generateStructureSource :: StructureElementSourceGenerator
                                  , validator :: Maybe ElementValidator
                                  , elementName :: String
                                  }

data AnyElementSourceGenerator = WSrc WidgetElementSourceGenerator
                               | SSrc StructureElementSourceGenerator

type GenM a = State GenState a

type WidgetElementSourceGenerator = Element Posn -> Hs.Name -> GenM WidgetHandlerResult
type StructureElementSourceGenerator = Element Posn -> Hs.Name -> GenM ()

data ValidationState =
    ValidationState { errors :: [String]
                    , theValidators :: [(String, ElementValidator)]
                    }

type ValidateM a = StateT ValidationState IO a

type ElementValidator = Element Posn -> ValidateM ()

data WidgetHandlerResult =
    WidgetHandlerResult { resultWidgetName :: WidgetName
                        , fieldValueName :: Maybe ValueName
                        }
