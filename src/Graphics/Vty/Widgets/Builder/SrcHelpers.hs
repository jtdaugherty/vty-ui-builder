module Graphics.Vty.Widgets.Builder.SrcHelpers
    ( defAttr
    , toAST
    , call
    , bind
    , tBind
    , noLoc
    , act
    , expr
    , mkTyp
    , parseType
    , mkList
    , parens
    , mkName
    , mkString
    , mkInt
    , mkChar
    , mkTup
    , opApp
    , mkLet
    , mkImportDecl
    , nameStr
    , attrsToExpr
    )
where

import qualified Language.Haskell.Exts as Hs

defAttr :: Hs.Exp
defAttr = expr $ mkName "def_attr"

toAST :: (Show a) => a -> Hs.Exp
toAST thing = parsed
    where
      Hs.ParseOk parsed = Hs.parse $ show thing

call :: String -> [Hs.Exp] -> Hs.Exp
call func [] = Hs.Var $ Hs.UnQual $ mkName func
call func args =
    mkApp (Hs.Var $ Hs.UnQual $ mkName func) args
        where
          mkApp app [] = app
          mkApp app (arg:rest) = mkApp (Hs.App app arg) rest

bind :: Hs.Name -> String -> [Hs.Exp] -> Hs.Stmt
bind lval func args =
    Hs.Generator noLoc (Hs.PVar lval) $ call func args

tBind :: [Hs.Name] -> String -> [Hs.Exp] -> Hs.Stmt
tBind lvals func args =
    Hs.Generator noLoc (Hs.PTuple (map Hs.PVar lvals)) $ call func args

act :: Hs.Exp -> Hs.Stmt
act = Hs.Qualifier

expr :: Hs.Name -> Hs.Exp
expr = Hs.Var . Hs.UnQual

parens :: Hs.Exp -> Hs.Exp
parens = Hs.Paren

mkString :: String -> Hs.Exp
mkString = Hs.Lit . Hs.String

mkInt :: Int -> Hs.Exp
mkInt = Hs.Lit . Hs.Int . toEnum

mkChar :: Char -> Hs.Exp
mkChar = Hs.Lit . Hs.Char

mkTup :: [Hs.Exp] -> Hs.Exp
mkTup = Hs.Tuple

mkList :: [Hs.Exp] -> Hs.Exp
mkList = Hs.List

opApp :: Hs.Exp -> Hs.Name -> Hs.Exp -> Hs.Exp
opApp a op b = Hs.InfixApp a (Hs.QVarOp $ Hs.UnQual op) b

mkLet :: [(Hs.Name, Hs.Exp)] -> Hs.Stmt
mkLet pairs = Hs.LetStmt $ Hs.BDecls $ map mkDecl pairs
    where
      mkDecl (nam, e) = Hs.PatBind
                        noLoc
                        (Hs.PVar nam)
                        Nothing
                        (Hs.UnGuardedRhs e)
                        (Hs.BDecls [])

mkName :: String -> Hs.Name
mkName = Hs.Ident

mkImportDecl :: String -> [String] -> Hs.ImportDecl
mkImportDecl name hidden =
    Hs.ImportDecl { Hs.importLoc = noLoc
                  , Hs.importModule = Hs.ModuleName name
                  , Hs.importQualified = False
                  , Hs.importSrc = False
                  , Hs.importPkg = Nothing
                  , Hs.importAs = Nothing
                  , Hs.importSpecs = case hidden of
                                       [] -> Nothing
                                       is -> Just (True, map (Hs.IVar . mkName) is)
                  }

parseType :: String -> Hs.Type
parseType s =
    case Hs.parse s of
      Hs.ParseOk val -> val
      Hs.ParseFailed _ msg -> error $ "Error parsing type string '" ++ s ++ "': " ++ msg

attrsToExpr :: (Maybe String, Maybe String) -> Maybe Hs.Exp
attrsToExpr (Nothing, Nothing) = Nothing
attrsToExpr (Just fg, Nothing) = Just $ call "fgColor" [expr $ mkName fg]
attrsToExpr (Nothing, Just bg) = Just $ call "bgColor" [expr $ mkName bg]
attrsToExpr (Just fg, Just bg) = Just $ opApp
                                 (expr $ mkName fg)
                                 (mkName "on")
                                 (expr $ mkName bg)

nameStr :: Hs.Name -> String
nameStr (Hs.Ident s) = s
nameStr n = error $ "Unsupported name: " ++ (show n)

noLoc :: Hs.SrcLoc
noLoc = Hs.SrcLoc { Hs.srcFilename = "-"
                  , Hs.srcLine = 0
                  , Hs.srcColumn = 0
                  }

mkTyp :: String -> [Hs.Type] -> Hs.Type
mkTyp tyCon [] = Hs.TyCon $ Hs.UnQual $ mkName tyCon
mkTyp tyCon args = mkApp (Hs.TyCon (Hs.UnQual (mkName tyCon))) args
    where
      mkApp ty [] = ty
      mkApp ty (ty':tys) = mkApp (Hs.TyApp ty ty') tys
