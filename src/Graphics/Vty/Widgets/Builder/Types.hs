{-# LANGUAGE TypeSynonymInstances #-}
module Graphics.Vty.Widgets.Builder.Types
    ( GenState(..)
    , GenM
    , AnyName(..)
    , ElementHandler(..)
    , ValueName(..)
    , WidgetName(..)
    , TyCon(..)
    , ToDoc(..)
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
    , (>-)
    )
where

import Control.Monad.State
import Data.List (intersperse)
import qualified Data.Map as Map
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
import Text.PrettyPrint.HughesPJ

-- Representation of general (non-widget) values with custom types.
data ValueName = ValueName { valueName :: String
                           , valueType :: String
                           }
                 deriving (Eq, Show)

data WidgetName = WidgetName { widgetName :: String
                             , widgetType :: TyCon
                             }
                  deriving (Eq, Show)

-- Sum type of different names to be used when either type of name is
-- appropriate (focus method instructions, interface element field
-- types, etc).
data AnyName = WName WidgetName
             | VName ValueName
               deriving (Eq, Show)

class ToDoc a where
    toDoc :: a -> Doc

(>-) :: (ToDoc a, ToDoc b) => a -> b -> Doc
a >- b = toDoc a <> toDoc b

instance ToDoc Doc where
    toDoc = id

instance ToDoc String where
    toDoc = text

instance ToDoc WidgetName where
    toDoc = text . widgetName

instance ToDoc ValueName where
    toDoc = text . valueName

data ValidatedElement = Validated (Element Posn)

data InterfaceValues =
    InterfaceValues { topLevelWidgetName :: String
                    , switchActionName :: String
                    , focusGroupName :: String
                    }

data GenState =
    GenState { nameCounters :: Map.Map String Int
             , genDoc :: Doc
             , handlers :: [(String, AnyElementSourceGenerator)]
             , allWidgetNames :: [(String, WidgetName)]
             , registeredFieldNames :: [(String, AnyName)]
             , interfaceNames :: [(String, InterfaceValues)]
             , focusMethods :: [(String, FocusMethod)]
             , focusValues :: [(String, WidgetName)]
             , imports :: [String]
             , paramNames :: [(String, String)]
             }

data FocusMethod = Direct WidgetName -- The name of the widget which
                                     -- should be added to the primary
                                     -- focus group.
                 | Merge String -- The name of the focus group value
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

type WidgetElementSourceGenerator = Element Posn -> String -> GenM WidgetHandlerResult
type StructureElementSourceGenerator = Element Posn -> String -> GenM ()

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

data TyCon = TyCon String [TyCon]
             deriving (Show, Eq)

instance ToDoc TyCon where
    toDoc (TyCon s []) = text s
    toDoc (TyCon s tcs) = hcat $ intersperse (text " ") $ text s : fields
        where
          conNumFields (TyCon _ ch) = length ch
          fields = map mkField tcs
          mkField tc = f $ toDoc tc
              where f = if conNumFields tc > 0
                        then parens
                        else id
