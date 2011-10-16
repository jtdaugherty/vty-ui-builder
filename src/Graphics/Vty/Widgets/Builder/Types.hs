module Graphics.Vty.Widgets.Builder.Types
    ( GenState(..)
    , GenM
    , ElementHandler(..)
    , ValueName(..)
    , RegisteredName(..)
    , Type(..)
    , TyCon(..)
    , ToDoc(..)
    , InterfaceValues(..)
    , FocusMethod(..)
    , ValidatedElement(..)
    , HandlerResult(..)
    , ElementValidator
    , ValidationState(..)
    , ValidateM
    , ElementSourceGenerator
    )
where

import Control.Monad.State
import Data.List (intersperse)
import qualified Data.Map as Map
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
import Text.PrettyPrint.HughesPJ

newtype RegisteredName = RegisteredName String
    deriving (Eq, Show)

newtype ValueName = ValueName String
    deriving (Eq, Show)

class ToDoc a where
    toDoc :: a -> Doc

instance ToDoc RegisteredName where
    toDoc (RegisteredName s) = text s

instance ToDoc ValueName where
    toDoc (ValueName s) = text s

data ValidatedElement = Validated (Element Posn)

data InterfaceValues =
    InterfaceValues { topLevelWidgetName :: ValueName
                    , switchActionName :: ValueName
                    , focusGroupName :: ValueName
                    }

data GenState =
    GenState { nameCounters :: Map.Map String Int
             , genDoc :: Doc
             , handlers :: [(String, ElementSourceGenerator)]
             , namedValues :: [(RegisteredName, (ValueName, ValueName))]
             , valueTypes :: [(ValueName, Type)]
             , interfaceNames :: [(String, InterfaceValues)]
             , focusMethods :: [(ValueName, FocusMethod)]
             , imports :: [String]
             }

data FocusMethod = Direct | Merge ValueName

data ElementHandler =
    ElementHandler { generateSource :: ElementSourceGenerator
                   , isWidgetElement :: Bool
                   , validator :: Maybe ElementValidator
                   , elementName :: String
                   }

type GenM a = State GenState a

type ElementSourceGenerator = Element Posn -> ValueName -> GenM (Maybe HandlerResult)

data ValidationState =
    ValidationState { errors :: [String]
                    , theValidators :: [(String, ElementValidator)]
                    }

type ValidateM a = StateT ValidationState IO a

type ElementValidator = Element Posn -> ValidateM ()

data HandlerResult =
    HandlerResult { widgetValue :: (ValueName, Type)
                  , fieldValue :: Maybe (ValueName, Type)
                  }

data Type = Widget TyCon
          | Custom String

data TyCon = TyCon String [TyCon]

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
