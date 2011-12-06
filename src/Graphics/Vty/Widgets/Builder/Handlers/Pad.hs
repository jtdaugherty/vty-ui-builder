module Graphics.Vty.Widgets.Builder.Handlers.Pad
    ( handlers
    )
where

import Data.Maybe (catMaybes)
import Control.Applicative ((<$>), (<*>), pure)
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import Graphics.Vty.Widgets.Builder.Util (foreach)
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S
import qualified Language.Haskell.Exts as Hs

handlers :: [WidgetElementHandler]
handlers = [handlePad]

data PadInfo = PadInfo { padTop :: Maybe Int
                       , padBottom :: Maybe Int
                       , padLeft :: Maybe Int
                       , padRight :: Maybe Int
                       , padLeftRight :: Maybe Int
                       , padTopBottom :: Maybe Int
                       , padAll :: Maybe Int
                       }
             deriving (Eq)

noPadding :: PadInfo
noPadding = PadInfo Nothing Nothing Nothing Nothing Nothing Nothing Nothing

handlePad :: WidgetElementHandler
handlePad =
    WidgetElementHandler genSrc doValidate "pad"
        where
          doValidate s = do
            pInfo <- PadInfo
                     <$> V.optionalInt s "top"
                     <*> V.optionalInt s "bottom"
                     <*> V.optionalInt s "left"
                     <*> V.optionalInt s "right"
                     <*> V.optionalInt s "leftRight"
                     <*> V.optionalInt s "topBottom"
                     <*> V.optionalInt s "all"
            if pInfo == noPadding then
                failValidation s "element requires at least one padding attribute" else
                (,) <$> pure pInfo <*> V.firstChildWidget s

          genSrc nam (padding, ch) = do
            chNam <- newEntry (widgetLikeName ch)
            gen ch chNam

            -- Mapping of projections and the function names we should
            -- use when generating source
            let padFunctions = [ (padTop, "padTop")
                               , (padBottom, "padBottom")
                               , (padLeft, "padLeft")
                               , (padRight, "padRight")
                               , (padLeftRight, "padLeftRight")
                               , (padTopBottom, "padTopBottom")
                               , (padAll, "padAll")
                               ]

            -- For set padding attributes, extract padding expressions
            let paddingExprs :: [Hs.Exp]
                paddingExprs = catMaybes $ foreach padFunctions $ \(field, funcName) -> do
                                 realVal <- field padding
                                 return $ S.call funcName [S.mkInt realVal]

            -- Construct padding expression from values
            let ex = foldl (\e1 e2 -> S.opApp e1 (S.mkName "pad") e2)
                     (head paddingExprs) (tail paddingExprs)

            append $ S.bind nam "padded" [ S.expr chNam
                                         , S.parens ex
                                         ]

            return $ declareWidget nam (S.mkTyp "Padded" [])
