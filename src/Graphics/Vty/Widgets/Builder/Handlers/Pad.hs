module Graphics.Vty.Widgets.Builder.Handlers.Pad
    ( handlers
    , noPadding
    , PadInfo
    , paddingFromAttributes
    , paddingExprFromPadInfo
    )
where

import Data.Maybe (catMaybes)
import Control.Applicative ((<$>), (<*>), pure)
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import Graphics.Vty.Widgets.Builder.Util (foreach)
import qualified Graphics.Vty.Widgets.Builder.AST as A
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
               deriving (Eq, Show)

noPadding :: PadInfo
noPadding = PadInfo Nothing Nothing Nothing Nothing Nothing Nothing Nothing

paddingFromAttributes :: A.Element -> ValidateM PadInfo
paddingFromAttributes s =
    PadInfo
    <$> V.optionalInt s "padTop"
    <*> V.optionalInt s "padBottom"
    <*> V.optionalInt s "padLeft"
    <*> V.optionalInt s "padRight"
    <*> V.optionalInt s "padLeftRight"
    <*> V.optionalInt s "padTopBottom"
    <*> V.optionalInt s "padAll"

paddingExprFromPadInfo :: PadInfo -> Hs.Exp
paddingExprFromPadInfo padding =
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

        paddingExprs = catMaybes $ foreach padFunctions $ \(field, funcName) -> do
                         realVal <- field padding
                         return $ S.call funcName [S.mkInt realVal]
        e = case paddingExprs of
              [] -> S.expr $ S.mkName "padNone"
              [ex] -> ex
              es -> foldl (\e1 e2 -> S.opApp e1 (S.mkName "pad") e2)
                    (head es) (tail es)
    in e

handlePad :: WidgetElementHandler
handlePad =
    WidgetElementHandler genSrc doValidate "pad"
        where
          doValidate s = do
            pInfo <- paddingFromAttributes s
            if pInfo == noPadding then
                failValidation s "element requires at least one padding attribute" else
                (,) <$> pure pInfo <*> V.firstChildWidget s

          genSrc nam (padding, ch) = do
            chNam <- newEntry (widgetLikeName ch)
            gen ch chNam

            append $ S.bind nam "padded" [ S.expr chNam
                                         , S.parens $ paddingExprFromPadInfo padding
                                         ]

            return $ declareWidget nam (S.mkTyp "Padded" [])
