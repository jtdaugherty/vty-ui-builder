{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.Vty.Widgets.Builder.Types
    ( GenState(..)
    , GenM
    , AnyName(..)
    , ValueName(..)
    , WidgetName(..)
    , InterfaceValues(..)
    , FocusMethod(..)
    , WidgetSpecHandler(..)
    , WidgetHandlerResult(..)
    , WidgetSourceGenerator
    )
where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Language.Haskell.Exts.Syntax as Hs

import Graphics.Vty.Widgets.Builder.AST

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

data InterfaceValues =
    InterfaceValues { topLevelWidgetName :: Hs.Name
                    , switchActionName :: Hs.Name
                    , focusGroupName :: Hs.Name
                    }

data GenState =
    GenState { nameCounters :: Map.Map String Int
             , hsStatements :: [Hs.Stmt]
             , handlers :: [(String, WidgetSpecHandler)]
             , allWidgetNames :: [(Hs.Name, WidgetName)]
             , registeredFieldNames :: [(Hs.Name, AnyName)]
             , interfaceNames :: [(String, InterfaceValues)]
             , focusMethods :: [(Hs.Name, FocusMethod)]
             , focusValues :: [(Hs.Name, WidgetName)]
             , paramNames :: [(Hs.Name, Hs.Type)]
             }

data FocusMethod = Direct WidgetName -- The name of the widget which
                                     -- should be added to the primary
                                     -- focus group.
                 | Merge Hs.Name -- The name of the focus group value
                                 -- that should be merged into the
                                 -- primary focus group.

data WidgetSpecHandler =
    WidgetSpecHandler { generateWidgetSource :: WidgetSourceGenerator
                      , specType :: String
                      }

type GenM a = State GenState a

type WidgetSourceGenerator = WidgetSpec -> Hs.Name -> GenM WidgetHandlerResult

data WidgetHandlerResult =
    WidgetHandlerResult { resultWidgetName :: WidgetName
                        , fieldValueName :: Maybe ValueName
                        }
