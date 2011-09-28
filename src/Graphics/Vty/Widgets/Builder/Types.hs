module Graphics.Vty.Widgets.Builder.Types
    ( GenState(..)
    , GenM
    , ElementHandler
    )
where

import Control.Monad.State
import Text.XML.HaXml.Types
import Text.PrettyPrint.HughesPJ

data GenState a = GenState { nameCounter :: Int
                           , genDoc :: Doc
                           , handlers :: [(String, ElementHandler a)]
                           , namedValues :: [(String, String)]
                           , valueTypes :: [(String, String)]
                           , interfaceNames :: [(String, (String, String))]
                           }

type GenM a b = State (GenState a) b

type ElementHandler a = Element a -> String -> GenM a ()
