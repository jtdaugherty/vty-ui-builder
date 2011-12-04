module Graphics.Vty.Widgets.Builder.Handlers.Text
    ( handlers
    )
where

import Control.Applicative
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.GenLib
import qualified Graphics.Vty.Widgets.Builder.AST as A
import qualified Graphics.Vty.Widgets.Builder.Validation as V
import qualified Language.Haskell.Exts as Hs

handlers :: [WidgetElementHandler]
handlers = [ handleFormattedText
           ]

isWhitespace :: Char -> Bool
isWhitespace = (`elem` " \t\n")

handleFormattedText :: WidgetElementHandler
handleFormattedText =
    WidgetElementHandler genSrc doValidation "fText"

doValidation :: A.Element -> ValidateM [(Either String String, Hs.Exp)]
doValidation s = pairs
    where
      -- For each entry in the contents list: If it is a string, give
      -- it the default attribute and put it in a list.  If it is an
      -- Attr element, recurse on it, building another list of
      -- (string, attr) to merge.

      -- Left: do not strip whitespace
      -- Right: do strip whitespace
      processSpecContent :: Hs.Exp -> A.ElementContent -> ValidateM [(Either String String, Hs.Exp)]
      processSpecContent ex c =
          case c of
            A.Text str _ -> return [(Right $ stripWhitespace str, ex)]
            A.ChildElement elm ->
                case A.elementName elm of
                  "br" -> return [(Left "\n", ex)]
                  "attr" -> processAttr elm
                  badName -> failValidation $ Error (A.sourceLocation s) $ "got unsupported child of fText: " ++ badName
            A.ChildWidgetLike _ -> failValidation $ Error (A.sourceLocation s) "got unsupported child of fText: widget-like"

      processElemContent :: Hs.Exp -> A.ElementContent -> ValidateM [(Either String String, Hs.Exp)]
      processElemContent ex c =
          case c of
            A.ChildWidgetLike _ -> failValidation $ Error (A.sourceLocation s) "got unsupported child of attr: widget-like"
            A.Text str _ -> return [(Right $ stripWhitespace str, ex)]
            A.ChildElement elm ->
                case A.elementName elm of
                  "br" -> return [(Left "\n", ex)]
                  "attr" -> processAttr elm
                  badName -> failValidation $ Error (A.sourceLocation s) $ "got unsupported child of attr: " ++ badName

      processAttr :: A.Element -> ValidateM [(Either String String, Hs.Exp)]
      processAttr elm = do
        let loc = A.elementLocation elm
        attrResult <- (,)
                      <$> (V.requireValidColor loc $ getAttribute elm "fg")
                      <*> (V.requireValidColor loc $ getAttribute elm "bg")

        let attrExpr = case attrsToExpr attrResult of
                         Nothing -> defAttr
                         Just ex -> ex

        results <- mapM (processElemContent attrExpr) $ A.elementContents elm
        return $ concat $ results

      stripWhitespace :: [Char] -> [Char]
      stripWhitespace (c1:c2:cs) = if isWhitespace c1 && isWhitespace c2
                                   then stripWhitespace (c2:cs)
                                   else c1 : (stripWhitespace (c2:cs))
      stripWhitespace ls = ls

      pairs :: ValidateM [(Either String String, Hs.Exp)]
      pairs = do
        results <- mapM (processSpecContent defAttr) $ A.elementContents s
        return $ concat $ results

genSrc :: Hs.Name -> [(Either String String, Hs.Exp)] -> GenM WidgetHandlerResult
genSrc nam pairs = do
  let collapse :: [(Either String String, Hs.Exp)] -> [(Either String String, Hs.Exp)]
      collapse [] = []
      collapse [p] = [p]
      collapse ((Right s1, e1):(Right s2, e2):es) =
          if e1 == e2
          then collapse ((Right $ s1 ++ s2, e1) : es)
          else (Right s1, e1) : collapse ((Right s2, e2):es)
      collapse (p:ps) = p : collapse ps

      collapsed :: [(Either String String, Hs.Exp)]
      collapsed = collapse pairs

      pairExpr :: (String, Hs.Exp) -> Hs.Exp
      pairExpr (s, ex) = mkTup [ mkString s
                               , ex
                               ]

      headTrimmed ((Right s, attr):rest) = (Right $ dropWhile isWhitespace s, attr) : rest
      headTrimmed es = es

      tailTrimmed [] = []
      tailTrimmed ls =
          init ls ++ case last ls of
                       (Right s, attr) -> [(Right $ reverse $ dropWhile isWhitespace $ reverse s, attr)]
                       other -> [other]

      normStrings ls = map (\(s, attr) -> (either id id s, attr)) ls

      pairExprList = map pairExpr $ normStrings $ tailTrimmed $ headTrimmed collapsed

  append $ bind nam "plainTextWithAttrs" [mkList pairExprList]
  return $ declareWidget nam (mkTyp "FormattedText" [])
