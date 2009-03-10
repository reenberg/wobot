%if False
\begin{code}
{-# LANGUAGE FlexibleInstances #-}

-- Copyright (c) 2006-2008
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

--------------------------------------------------------------------------------
-- |
-- Module      :  Language.Hs.Pretty
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.Hs.Pretty where

import Data.Name
import Language.Hs.Syntax
import Text.PrettyPrint.Mainland
\end{code}
%endif

\subsection{Pretty Printing}

\subsubsection{Bindings}

\begin{code}
instance Pretty TyVar where
    ppr (TyVar n) = ppr n

instance Pretty TyCon where
    ppr (TyCon n)           = ppr n
    ppr (TupleTyCon arity)  = parens $ text $ replicate (arity - 1) ','

instance Pretty Var where
    ppr (Var v)        =  parensIf (isVarsym v) $ ppr v
    ppr (AntiVar v)    =  text "$var:" <> text v
    ppr (AntiVarId v)  =  text "$id:" <> text v

instance Pretty Con where
    ppr (Con v)           = parensIf (isConsym v) $ ppr v
    ppr (TupleCon arity)  = parens $ text $ replicate (arity - 1) ','
\end{code}

\subsubsection{Types}

\begin{code}
instance Pretty Type where
    pprPrec _ (TyConTy tycon) = ppr tycon
    pprPrec _ (TyVarTy tyvar) = ppr tyvar

    pprPrec p ty@(AppTy ty1 ty2) =
        case unfoldAppTy ty of
          [TyConTy (TyCon n), ty]
              | n == builtinNil ->
                  brackets $ ppr ty
          [TyConTy (TyCon n), ty1, ty2]
              | n == builtinArrow ->
                  infixOp p (RightAssoc, 0) (text "->") ty1 ty2
          (TyConTy (TupleTyCon arity)) : tys | arity == length tys ->
              parens $ commasep (map ppr tys)
          _ ->
              infixOp p (LeftAssoc, 1) empty ty1 ty2

    pprPrec p (ForAll _ _ [] ty)   = pprPrec p ty
    pprPrec p (ForAll _ _ ctx ty)  = ppr ctx <+> text "=>" <+> pprPrec p ty

    pprPrec _ (AntiType s) = text "$ty:" <> text s
\end{code}

\subsubsection{Modules}

\begin{code}
instance Pretty Module where
    ppr (Module mod exports body) =
        text "module" <+> ppr mod <+> pprExports exports <+> ppr body
      where
        pprExports :: [Export] -> Doc
        pprExports  []    = empty
        pprExports  exps  = parens $ commasep (map ppr exps)
\end{code}

\begin{code}
instance Pretty Body where
    ppr (Body imps topdecls) =
        embrace $ semiseplines (map ppr imps ++ map ppr topdecls)
\end{code}

\begin{code}
instance Pretty Export where
    ppr (VarExport v)                      = ppr v
    ppr (TyConExport tycon Nothing)        = ppr tycon <> text "(..)"
    ppr (TyConExport tycon (Just cnames))  = ppr tycon <> ppr cnames
    ppr (ModuleExport mod)                 = ppr mod <+> ppr mod
\end{code}

\begin{code}
instance Pretty ImportDecl where
    ppr (ImportDecl qualified mod as hiding imps _) =
        text "import" <+> if qualified then text "qualified" else empty <+> ppr mod
        <+> pas <+> if hiding then text "hiding" else empty
        <+> (parens $ commasep (map ppr imps))
      where
        pas :: Doc
        pas = case as of
                Nothing   -> empty
                Just mod  -> ppr mod
\end{code}

\begin{code}
instance Pretty Import where
    ppr (VarImport v _)                      = ppr v
    ppr (TyConImport tycon Nothing _)        = ppr tycon <> text "(..)"
    ppr (TyConImport tycon (Just cnames) _)  = ppr tycon <> ppr cnames
\end{code}

\begin{code}
instance Pretty CName where
    ppr (VarCName v)  = ppr v
    ppr (ConCName c)  = ppr c

    pprList cnames = parens $ commasep (map ppr cnames)
\end{code}

\subsubsection{Declarations}

\begin{code}
instance Pretty Assoc where
    ppr LeftAssoc  = text "left"
    ppr RightAssoc = text "right"
    ppr NonAssoc   = text "non"

instance Pretty Pred where
    ppr (ClassPred con tys) = spread $ ppr con : map ppr tys

    pprList []      = empty
    pprList [pred]  = ppr pred
    pprList preds   = parens $ commasep (map ppr preds)

instance Pretty ConDecl where
    ppr (ConDecl con _ [] _) =
        ppr con

    ppr (ConDecl con _ tys _) =
        ppr con <+> spread (map (pprPrec 10) tys)

    ppr (OpConDecl ty1 con _ ty2 _) =
        ppr ty1 <+> ppr con <+> ppr ty2

    ppr (RecConDecl con _ flds _) =
        ppr con
        <+> embrace (semiseplines (map pprField flds))
      where
        pprField :: (Var, Type) -> Doc
        pprField (v, ty) = nest 4 $ ppr v <+> text "::" <+> ppr ty

instance Pretty Fixity where
    ppr Infixl = text "infixl"
    ppr Infixr = text "infixr"
    ppr Infix  = text "infix"

instance Pretty Decl where
    ppr (TypeDecl tycon tyvars ty _) =
        text "type"  <+> ppr tycon <+> spread (map ppr tyvars)
                     <+> text "=" <+> ppr ty

    ppr (DataDecl dataornewtype ctx tycon tyvars condecls derive _) =
            (case dataornewtype of
               DataType -> text "data";
               NewType -> text "newtype"
            )
        <+> (case ctx of
               []      -> empty
               [pred]  -> ppr pred <+> text "=> "
               preds   -> parens (commasep (map ppr preds)) <+> text "=> "
            )
        <> ppr tycon
        <> (case tyvars of
              []  -> empty
              _   -> text " " <> spread (map ppr tyvars)
           )
        <> (case condecls of
               []         ->  empty
               [datacon]  ->  text " =" <+> ppr datacon
               datacons   ->  nest 2 (text " =" </> text "  "
                              <> folddoc (\hd tl -> hd </> text "|" <+> tl)
                                         (map ppr datacons))
            )
        <> case derive of
             []  -> empty
             _   -> text " deriving" <+> parens (commasep $ map ppr derive)

    ppr (ClassDecl ctx tycls tvs decls _) =
        text "class"
        <+> case ctx of
              []  -> empty
              _   -> ppr ctx <+> text "=>"
        <+> spread (ppr tycls : map ppr tvs)
        <+> nest 4 (text "where {" </> semiseplines (map ppr decls))
        </> text "}"

    ppr (InstDecl ctx tycls tvs decls _) =
        text "instance"
        <+> case ctx of
              []  -> empty
              _   -> ppr ctx <+> text "=>"
        <+> spread (ppr tycls : map ppr tvs)
        <+> nest 4 (text "where {" </> semiseplines (map ppr decls))
        </> text "}"

    ppr (DefaultDecl tys _) =
        nest 4 $ parens $ commasep (map ppr tys)

    ppr (ExpBindDecl e rhs _) =
        nest 4 $ ppr e <+> ppr rhs

    ppr (VarBindDecl v [] rhs _) =
        nest 4 $ ppr v <+> ppr rhs

    ppr (VarBindDecl v pats rhs _) =
        nest 4 $ ppr v <+> spread (map (pprPrec 10) pats) <+> ppr rhs

    ppr (PatBindDecl pat rhs _) =
        nest 4 $ ppr pat <+> ppr rhs

    ppr (SigDecl vars ty _) =
        nest 4 $ commasep (map ppr vars) <+> text "::" <+> ppr ty

    ppr (FixityDecl ops fixity prec _)
        | prec == 9  = nest 4 $ ppr fixity <+> commasep (map ppr ops)
        | otherwise  = nest 4 $ ppr fixity <+> commasep (map ppr ops) <+> (ppr prec)

    pprList decls = seplines line (map ppr decls)

prettyGuard :: (Maybe Exp, Exp) ->  Doc
prettyGuard (Nothing, exp)  = text "=" <+> ppr exp
prettyGuard (Just gd, exp)  = (text "|" <+> (ppr gd <+> text "=")) <+> ppr exp

prettyWhere :: [Decl] ->  Doc
prettyWhere []     =  empty
prettyWhere decls  =  text "where" <+> embrace (semiseplines (map ppr decls))

instance Pretty Rhs where
    ppr (Rhs guards []) =
        stack (map prettyGuard guards)

    ppr (Rhs guards decls) =
        stack (map prettyGuard guards) <+> prettyWhere decls

pprBindingSig :: Var -> Maybe Type -> Doc
pprBindingSig _ Nothing    = empty
pprBindingSig v (Just ty)  = nest 4 $ ppr v <+> text "::" <+> ppr ty

pprBindingFixity :: Pretty v => v -> OpFixity -> Doc
pprBindingFixity v fixity
    | fixity == defaultFixity  = empty
    | otherwise                = nest 4 $ go fixity
  where
    go (LeftAssoc, i)   = text "infixl" <+> ppr i <+> ppr v
    go (RightAssoc, i)  = text "infixr" <+> ppr i <+> ppr v
    go (NonAssoc, i)    = text "infixr" <+> ppr i <+> ppr v

instance Pretty Binding where
    ppr (SigBind v ty fixity) =
        pprBindingFixity v fixity </>
        ppr v <+> text "::" <+> ppr ty

    ppr (FixityBind n fixity) =
        pprBindingFixity n fixity

    ppr (VarBind v maybe_ty fixity alts) =
        pprBindingFixity v fixity </>
        pprBindingSig v maybe_ty </>
        stack (map printAlt alts)
      where
        printAlt  ::  ([Pat], Rhs)
                  ->  Doc
        printAlt ([], rhs) =
            nest 4 $ ppr v <+> ppr rhs
        printAlt (pats, rhs) =
            nest 4 $ ppr v <+> spread (map (pprPrec 10) pats) <+> ppr rhs

    ppr (PatBind v maybe_ty fixity rhs) =
        pprBindingFixity v fixity </>
        pprBindingSig v maybe_ty </>
        (nest 4 $ ppr v <+> ppr rhs)
\end{code}

\subsubsection{Expressions}

\begin{code}
instance Pretty Lit where
    ppr (IntegerLit n)    = text $ show n
    ppr (FloatLit n)      = text $ show n
    ppr (CharLit c)       = text $ show c
    ppr (StringLit s)     = text $ show s
    ppr (AntiInt v)       = text "$int:" <> ppr v
    ppr (AntiFloat v)     = text "$flo:" <> ppr v

prettyPatGuard :: (Maybe Exp, Exp) ->  Doc
prettyPatGuard (Nothing, exp)  = text "->" <+> ppr exp
prettyPatGuard (Just gd, exp)  = text "|" <+> ppr gd <+> text "->" <+> ppr exp

instance Pretty Qual where
    ppr (GenQual pat exp _) =
        ppr pat <+> text "<-" <+> ppr exp

    ppr (LetQual decls _) =
        text "let" <+> embrace (semiseplines (map ppr decls))

    ppr (GuardQual exp _) = ppr exp

    pprList quals = commasep (map ppr quals)

instance Pretty Alt where
    ppr (Alt pat guards decls) =
        ppr pat <+> stack (map prettyPatGuard guards) <+/> prettyWhere decls

instance Pretty Exp where
    pprPrec _ (WildPatExp _)        = text "_"
    pprPrec _ (AsPatExp v e _)      = ppr v <> text "@" <> ppr e
    pprPrec _ (IrrefutPatExp e _)   = text "~" <> ppr e
    pprPrec _ (LitExp lit _)        = ppr lit
    pprPrec _ (VarExp v _)          = ppr v
    pprPrec _ (ConExp con _)        = ppr con

    pprPrec _ (RecConExp con flds _) =
        ppr con <+>
        embrace (semiseplines (map pprField flds))
      where
        pprField :: (Var, Exp) -> Doc
        pprField (v, e) = ppr v <+> text "=" <+> ppr e

    pprPrec _ (RecUpdateExp e flds _) =
        ppr e <+>
        embrace (semiseplines (map pprField flds))
      where
        pprField :: (Var, Exp) -> Doc
        pprField (v, e) = ppr v <+> text "=" <+> ppr e

    pprPrec _ (ArithSeqExp fr Nothing Nothing _) =
        brackets $ ppr fr <> text ".."

    pprPrec _ (ArithSeqExp fr (Just th) Nothing _) =
        brackets $ ppr fr <> text "," <> ppr th <> text ".."

    pprPrec _ (ArithSeqExp fr Nothing (Just to) _) =
        brackets $ ppr fr <> text ".." <> ppr to

    pprPrec _ (ArithSeqExp fr (Just th) (Just to) _) =
        brackets $ ppr fr <> text "," <> ppr th <> text ".." <> ppr to

    pprPrec _ (ListCompExp e quals _) =
        brackets $ ppr e <+> text "|" <+> ppr quals

    pprPrec p (LamExp v e _) =
        parensIf (p >= 10) $ text "\\" <> ppr v <+> text "->" <+> ppr e

    pprPrec p (AppExp e1 e2 _) =
        infixOp p (LeftAssoc, 11) empty e1 e2

    pprPrec p (OpAppExp e1 (VarExp (Var op) _) fixity e2 _) =
        if isVarsym op || isConsym op
        then infixOp p fixity (ppr op) e1 e2
        else infixOp p fixity (backquotes $ ppr op) e1 e2

    pprPrec p (OpAppExp e1 (ConExp (Con op) _) fixity e2 _) =
        if isConsym op
        then infixOp p fixity (ppr op) e1 e2
        else infixOp p fixity (backquotes $ ppr op) e1 e2

    pprPrec _ (OpAppExp _ _ _ _ _) = error "internal error: bad OpAppExp"

    pprPrec p (NegAppExp e _) =
        parensIf (p > 6) $ text "-" <> pprPrec 6 e

    pprPrec p (LetExp decls e _) =
        parensIf (p >= 10) $
        text "let"
        <+> embrace (semiseplines (map ppr decls))
        <+> text "in"
        <+/> ppr e

    pprPrec p (IfExp test then' else' _) =
        parensIf (p >= 10) $
        text "if" <+> ppr test
        <+/> text "then" <+> ppr then'
        <+/> text "else" <+> ppr else'

    pprPrec p (CaseExp e alts _) =
        parensIf (p >= 10) $
        text "case" <+> ppr e <+> text "of"
        <+> embrace (semiseplines (map ppr alts))

    pprPrec p (DoExp stmts _) =
        parensIf (p >= 10) $
        text "do"
        <+> embrace (semiseplines (map ppr stmts))

    pprPrec _ (LSection e op _) =
        parens $ ppr e <+> ppr op

    pprPrec _ (RSection op e _) =
        parens $ ppr op <+> ppr e

    pprPrec p (SigExp e ty _) =
        nest 4 $ infixOp p (NonAssoc, 10) (text "::") e ty

    pprPrec _ (ParExp e _) =
        parens $ ppr e

    pprPrec _ (AntiExp s _) =
        text "$exp:" <> text s
\end{code}

\subsubsection{Patterns}

\begin{code}
instance Pretty Pat where
    pprPrec _ (WildPat _)         = text "_"
    pprPrec _ (IrrefutPat pat _)  = text "~" <+> ppr pat
    pprPrec _ (LitPat lit _)      = ppr lit
    pprPrec _ (NPlusKPat v i _)   = ppr v <+> text "+" <+> ppr i

    pprPrec _ (ConPat (TupleCon _) pats _) =
        parens $ commasep (map ppr pats)

    pprPrec p (OpConPat p1 (Con con) fixity p2 _) =
        if isConsym con
        then infixOp p fixity (ppr con) p1 p2
        else infixOp p fixity (backquotes $ ppr con) p1 p2

    pprPrec _ (OpConPat _ _ _ _ _) = error "internal error: bad OpConPat"

    pprPrec _ (RecConPat con flds _) =
        ppr con <+>
        embrace (semiseplines (map pprField flds))
      where
        pprField :: (Var,Pat) -> Doc
        pprField (v, p) = ppr v <+> text "=" <+> ppr p

    pprPrec p (ConPat con pats _) =
        let doc = parens $ spread $ ppr con : map (pprPrec 11) pats
        in
          if p >= 11
          then parens doc
          else doc

    pprPrec _ (AsPat v (WildPat _) _) =
        ppr v

    pprPrec _ (AsPat v pat _) =
        ppr v <> text "@" <> ppr pat

    pprPrec p (SigPat pat ty _) = nest 4 $ infixOp p (NonAssoc, 0) (text "::") pat ty
\end{code}

\subsubsection{Statements}

\begin{code}
instance Pretty Stmt where
    ppr (ExpStmt exp _) = ppr exp

    ppr (PatStmt pat exp _)
        = ppr pat <+> text "<-" <+> ppr exp

    ppr (LetStmt decls _)
        = text "let"
          <+> embrace (semiseplines (map ppr decls))
\end{code}
