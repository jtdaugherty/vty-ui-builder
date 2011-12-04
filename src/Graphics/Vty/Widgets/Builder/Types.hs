{-# LANGUAGE GADTs, TypeSynonymInstances #-}
module Graphics.Vty.Widgets.Builder.Types
    ( GenState(..)
    , GenM
    , AnyName(..)
    , ValueName(..)
    , WidgetName(..)
    , InterfaceValues(..)
    , FocusMethod(..)
    , WidgetElementHandler(..)
    , WidgetHandlerResult(..)
    , Error(..)

    -- Validation
    , ValidateM(..)
    , Validated(..)
    , ValidationState(..)
    , failValidation
    , getResolvedRefs
    , getValidParams

    -- Misc
    , specType
    )
where

import Control.Applicative
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
             , elemHandlers :: [(String, WidgetElementHandler)]
             , allWidgetNames :: [(Hs.Name, WidgetName)]
             , registeredFieldNames :: [(Hs.Name, AnyName)]
             , interfaceNames :: [(String, InterfaceValues)]
             , focusMethods :: [(Hs.Name, FocusMethod)]
             , focusValues :: [(Hs.Name, WidgetName)]
             , errorMessages :: [Error]
             , validationState :: ValidationState

             -- Valid reference targets.  For widgets, the reference
             -- target is its 'id', and the name in the tuple is the
             -- name of the generated value.  For parameters, both
             -- names are the parameter name.
             , validReferenceTargets :: [(Hs.Name, (Hs.Name, Hs.Type))]
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
data WidgetElementHandler where
    WidgetElementHandler :: (Hs.Name -> a -> GenM WidgetHandlerResult)
                         -> (Element -> ValidateM a)
                         -> String
                         -> WidgetElementHandler

type GenM a = State GenState a

data WidgetHandlerResult =
    WidgetHandlerResult { resultWidgetName :: WidgetName
                        , fieldValueName :: Maybe ValueName
                        }

data ValidationState =
    ValidationState { validParams :: [String]
                    , resolvedRefs :: [(Hs.Name, WidgetElement)]
                    }

data ValidateM a =
    ValidateM { runValidation :: ValidationState -> Validated a }

data Validated a = Valid a
                 | ValidationError Error

instance Functor Validated where
    fmap f (Valid a) = Valid $ f a
    fmap _ (ValidationError e) = ValidationError e

instance Functor ValidateM where
    fmap f act = ValidateM $ \st -> f <$> (runValidation act st)

instance Monad ValidateM where
    act >>= f =
        ValidateM $ \st ->
            case runValidation act st of
              ValidationError e -> ValidationError e
              Valid a -> runValidation (f a) st

    return a = ValidateM $ const (Valid a)
    fail = ValidateM . const . ValidationError . Error noLoc

instance Applicative ValidateM where
    pure = return
    (<*>) = ap

instance Alternative ValidateM where
    empty = ValidateM $ const $ ValidationError $ Error noLoc "-"
    a <|> b =
        ValidateM $ \st ->
            case runValidation a st of
              Valid val -> Valid val
              ValidationError _ -> runValidation b st

failValidation :: Error -> ValidateM a
failValidation = ValidateM . const . ValidationError

getValidParams :: ValidateM [String]
getValidParams = ValidateM (Valid . validParams)

getResolvedRefs :: ValidateM [(Hs.Name, WidgetElement)]
getResolvedRefs = ValidateM (Valid . resolvedRefs)

specType :: WidgetElementHandler
         -> String
specType (WidgetElementHandler _ _ t) = t
