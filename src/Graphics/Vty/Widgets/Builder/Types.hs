{-# LANGUAGE GADTs, TypeSynonymInstances #-}
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
    , Error(..)
    )
where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Language.Haskell.Exts.Syntax as Hs
import qualified Control.Monad.Trans.Error as T

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

data Error = Error SourceLocation String

instance Show Error where
    show (Error loc s) = show loc ++ ": " ++ s

instance T.Error Error where
    noMsg = Error noLoc "-"
    strMsg = Error noLoc

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
             , errorMessages :: [Error]
             }

data FocusMethod = Direct WidgetName -- The name of the widget which
                                     -- should be added to the primary
                                     -- focus group.
                 | Merge Hs.Name -- The name of the focus group value
                                 -- that should be merged into the
                                 -- primary focus group.

-- A spec handler provides a validation routine.  The validation
-- routine is responsible for constructing a concrete data value
-- containing the validated widget state, and the state value will be
-- passed to the source generation routine.  This way, it should never
-- have to re-do (and thus skew from) the logic of the validation
-- routine.  Using a GADT allows us to capture this requirement
-- without worrying about the concrete types used by different spec
-- handlers; this just ensures that they are internally consistent.
data WidgetSpecHandler where
    WidgetSpecHandler :: (WidgetSpec -> Hs.Name -> a -> GenM WidgetHandlerResult)
                      -> (WidgetSpec -> Either Error a)
                      -> String
                      -> WidgetSpecHandler

type GenM a = State GenState a

data WidgetHandlerResult =
    WidgetHandlerResult { resultWidgetName :: WidgetName
                        , fieldValueName :: Maybe ValueName
                        }
