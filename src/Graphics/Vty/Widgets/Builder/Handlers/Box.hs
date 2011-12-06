module Graphics.Vty.Widgets.Builder.Handlers.Box
    ( handlers
    )
where

import Control.Applicative hiding (optional)
import Control.Monad

import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.AST as A
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Graphics.Vty.Widgets.Builder.SrcHelpers as S
import qualified Language.Haskell.Exts as Hs

import Graphics.Vty.Widgets.Box
    ( ChildSizePolicy(..)
    , IndividualPolicy(..)
    )

handlers :: [WidgetElementHandler]
handlers = [ handleVBox
           , handleHBox
           , handleVBoxSized
           , handleHBoxSized
           ]

handleVBox :: WidgetElementHandler
handleVBox =
    WidgetElementHandler genSrc doValidation "vBox"
        where
          doValidation s = (,)
                           <$> V.optionalInt s "spacing"
                           <*> boxChildWidgets s

          genSrc nam (spacing, chs) = do
            resultName <- genBox chs "vBox" spacing nam
            ty <- getWidgetStateType resultName
            return $ declareWidget nam ty

handleBoxSized :: String -> WidgetElementHandler
handleBoxSized typ =
    WidgetElementHandler genSrc doValidation (typ ++ "-sized")
        where
          doValidation s = (,,)
                           <$> V.optionalInt s "spacing"
                           <*> boxSize s
                           <*> sizedBoxChildWidgets s

          genSrc nam (spacing, boxSz, chs) = do
            let parsedSizeExpr = S.toAST boxSz

            resultName <- genBox chs typ spacing nam
            append $ S.act $ S.call "setBoxChildSizePolicy" [ S.expr nam
                                                            , parsedSizeExpr
                                                            ]
            ty <- getWidgetStateType resultName
            return $ declareWidget nam ty

handleHBoxSized :: WidgetElementHandler
handleHBoxSized = handleBoxSized "hBox"

handleVBoxSized :: WidgetElementHandler
handleVBoxSized = handleBoxSized "vBox"

boxSize :: A.Element -> ValidateM ChildSizePolicy
boxSize s = getPercentSize
            <|> getDualSize
            <|> (failValidation s
                 "Either a percentage or first/second size policy must be specified for this box")
    where
      getPercentSize = Percentage <$> V.requiredInt s "percent"

      getDualSize = PerChild
                    <$> (BoxFixed <$> V.requiredInt s "first"
                         <|> V.requiredEqual s "first" "auto" *> (pure BoxAuto))
                    <*> (BoxFixed <$> V.requiredInt s "second"
                         <|> V.requiredEqual s "second" "auto" *> (pure BoxAuto))

handleHBox :: WidgetElementHandler
handleHBox =
    WidgetElementHandler genSrc doValidation "hBox"
        where
          doValidation s = (,)
                           <$> V.optionalInt s "spacing"
                           <*> boxChildWidgets s

          genSrc nam (spacing, chs) = do
            resultName <- genBox chs "hBox" spacing nam
            ty <- getWidgetStateType resultName
            return $ declareWidget nam ty

genBox :: [A.WidgetLike]
       -> String
       -> Maybe Int
       -> Hs.Name
       -> GenM Hs.Name
genBox es typ spacing rootName = do
  names <- forM es $
           \child -> do
              chname <- newEntry $ widgetLikeName child
              gen child chname
              return chname

  let buildBox [] =
          error "BUG: unexpected buildBox input (validation should have caught this)"
      buildBox [c] = return c
      buildBox (c1:c2:rest) = do
              nextName <- newEntry typ
              append $ S.bind nextName typ [ S.expr c1
                                           , S.expr c2
                                           ]

              case spacing of
                Nothing -> return ()
                Just val ->
                    append $ S.act $ S.call "setBoxSpacing" [ S.expr nextName
                                                            , S.mkInt val
                                                            ]

              c1Type <- getWidgetStateType c1
              c2Type <- getWidgetStateType c2

              registerWidgetName $ WidgetName { widgetName = nextName
                                              , widgetType = S.mkTyp "Box" [c1Type, c2Type]
                                              }
              buildBox (nextName:rest)

  resultName <- buildBox names
  append $ S.mkLet [(rootName, S.expr resultName)]
  return resultName

boxChildWidgets :: A.Element -> ValidateM [A.WidgetLike]
boxChildWidgets s =
    case A.getChildWidgetLikes s of
      es@(_:_:_) -> return es
      _ -> failValidation s "Box must have at least two children"

sizedBoxChildWidgets :: A.Element -> ValidateM [A.WidgetLike]
sizedBoxChildWidgets s =
    case A.getChildWidgetLikes s of
      es@[_,_] -> return es
      _ -> failValidation s "Sized box must have exactly two children"
