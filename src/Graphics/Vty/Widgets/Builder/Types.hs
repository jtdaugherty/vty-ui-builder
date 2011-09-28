module Graphics.Vty.Widgets.Builder.Types
    ( GenState(..)
    , GenM
    , ElementHandler
    , ValueName(..)
    , RegisteredName(..)
    , TyCon(..)
    , ToDoc(..)
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

data GenState a =
    GenState { nameCounter :: Int
             , genDoc :: Doc
             , handlers :: [(String, ElementHandler a)]
             , namedValues :: [(RegisteredName, ValueName)]
             , valueTypes :: [(ValueName, TyCon)]
             , interfaceNames :: [(String, (ValueName, ValueName))]
             }

type GenM a b = State (GenState a) b

type ElementHandler a = Element a -> ValueName -> GenM a ()

data TyCon = TyCon String [TyCon]

instance ToDoc TyCon where
    toDoc (TyCon s []) = text s
    toDoc (TyCon s tcs) = hcat $ intersperse (text " ") $ text s : fields
        where
          conNumFields (TyCon _ ch) = length ch
          fields = map mkField tcs
          mkField tc = if conNumFields tc > 0
                       then text "(" <> toDoc tc <> text ")"
                       else toDoc tc
