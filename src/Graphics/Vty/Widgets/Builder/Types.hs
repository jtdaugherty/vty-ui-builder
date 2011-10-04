module Graphics.Vty.Widgets.Builder.Types
    ( GenState(..)
    , GenM
    , ElementHandler
    , ValueName(..)
    , RegisteredName(..)
    , Type(..)
    , TyCon(..)
    , ToDoc(..)
    , InterfaceValues(..)
    , FocusMethod(..)
    )
where

import Control.Monad.State
import Data.List (intersperse)
import Text.XML.HaXml.Types
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

data InterfaceValues =
    InterfaceValues { topLevelWidgetName :: ValueName
                    , switchActionName :: ValueName
                    , focusGroupName :: ValueName
                    }

data GenState a =
    GenState { nameCounter :: Int
             , genDoc :: Doc
             , handlers :: [(String, ElementHandler a)]
             , namedValues :: [(RegisteredName, (ValueName, ValueName))]
             , valueTypes :: [(ValueName, Type)]
             , interfaceNames :: [(String, InterfaceValues)]
             , focusMethods :: [(ValueName, FocusMethod)]
             }

data FocusMethod = Direct | Merge ValueName

type GenM a b = State (GenState a) b

type ElementHandler a = Element a -> ValueName -> GenM a ValueName

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
