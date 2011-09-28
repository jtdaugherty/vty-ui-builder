module Graphics.Vty.Widgets.Builder.Types
    ( GenState(..)
    , GenM
    , ElementHandler
    , ValueName(..)
    , RegisteredName(..)
    , ToDoc(..)
    )
where

import Control.Monad.State
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

data GenState a =
    GenState { nameCounter :: Int
             , genDoc :: Doc
             , handlers :: [(String, ElementHandler a)]
             , namedValues :: [(RegisteredName, ValueName)]
             , valueTypes :: [(ValueName, String)]
             , interfaceNames :: [(String, (ValueName, ValueName))]
             }

type GenM a b = State (GenState a) b

type ElementHandler a = Element a -> ValueName -> GenM a ()
