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
-- Module      :  Language.F.Pretty
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.F.Pretty where

import Data.Name
import Language.F.Syntax
import Text.PrettyPrint.Mainland
\end{code}
%endif

\subsection{Pretty Printing}

\subsubsection{Bindings}

\begin{code}
instance Pretty TyVar where
    ppr (TyVar v)           = ppr v

instance Pretty WildTyVar where
    ppr WildTyVar           = text "_"
    ppr (TameTyVar v)       = ppr v

instance Pretty TyCon where
    ppr (TyCon n)           = ppr n
    ppr (TupleTyCon arity)  = parens $ text $ replicate (arity - 1) ','

instance Pretty TyFun where
    ppr (TyFun f)           = ppr f
\end{code}

\begin{code}
instance Pretty Var where
    ppr (Var v)    = parensIf (isVarsym v) $ ppr v

instance Pretty WildVar where
    ppr WildVar           = text "_"
    ppr (TameVar v)       = ppr v

instance Pretty Con where
    ppr (Con v)           = parensIf (isConsym v) $ ppr v
    ppr (TupleCon arity)  = parens $ text $ replicate (arity - 1) ','
\end{code}

\subsubsection{Sorts}

\begin{code}
instance Pretty Sort where
    ppr  TY  = text "TY"
    ppr  CO  = text "CO"
\end{code}

\subsubsection{Kinds}

\begin{code}
instance Pretty Kind where
    pprPrec _  (:*)             = text "*"
    pprPrec p  (k1   :=>  k2)   = infixOp p (RightAssoc, 0) (text "->") k1 k2

    pprPrec p  (ty1  :~   ty2)  =
        parensIf (p >= 1) $
        pprPrec p ty1 <+> text "~" <+> pprPrec p ty2
\end{code}

\subsubsection{Types}

\begin{code}
pprKinded :: Pretty a => a -> Kind -> Doc
pprKinded  a  (:*)  = ppr a
pprKinded  a  k     = ppr a <+> text "::" <+/> ppr k

instance Pretty Type where
    pprPrec _  (TyConTy tycon _)  = ppr tycon
    pprPrec _  (TyVarTy tyvar _)  = ppr tyvar

    pprPrec p ty@(AppTy ty1 ty2 _) =
        case unfoldAppTy ty of
          [TyConTy (TyCon n) _, ty]
              | n == builtinNil ->
                  brackets $ ppr ty
              | n == builtinArrow ->
                  text "(->)" <+> ppr ty
          [TyConTy (TyCon n) _, ty1, ty2]
              | n == builtinArrow ->
                  infixOp p (RightAssoc, 0) (text "->") ty1 ty2
          TyConTy (TupleTyCon arity) _ : tys | arity == length tys ->
              parens $ commasep (map ppr tys)
          _ ->
              infixOp p (LeftAssoc, 1) empty ty1 ty2

    pprPrec p (AppTyFunTy tyfun tys _) =
        parensIf (p >= 1) $
        pprPrec p tyfun <+> spread (map (pprPrec p) tys)

    pprPrec p  ty@(ForAll _ _ _ _) =
        let (binds, qty) = unfoldForAll ty
        in
          parensIf (p >= 10) $
          text "\\/" <> commasep (map (uncurry pprKinded) binds)
          <+> text "." <+> ppr qty

    pprPrec p (SymCo ty _) =
        parensIf (p >= 1) $
        text "sym" <+> ppr ty

    pprPrec p (TransCo ty1 ty2 _) =
        parensIf (p >= 1) $
        ppr ty1 <+> text "o" <+> ppr ty2

    pprPrec p (AppCo ty1 ty2 _) =
        parensIf (p >= 1) $
        ppr ty1 <+> text "@" <+> ppr ty2

    pprPrec p (LeftCo ty _) =
        parensIf (p >= 1) $
        text "left" <+> ppr ty

    pprPrec p (RightCo ty _) =
        parensIf (p >= 1) $
        text "left" <+> ppr ty
\end{code}

\subsubsection{Declarations}

\begin{code}
instance Pretty Decl where
    ppr (DataDecl tycon k [] _) =
        text "data" <+> ppr tycon <+> text "::" <+/> ppr k

    ppr (DataDecl tycon k datacons _) =
        (nest 4 $
        text "data" <+> ppr tycon <+> text "::" <+/> ppr k <+> text "where" <+> text "{"
        </> semiseplines (map ppr datacons))
        </> text "}"

    ppr (TypeDecl tyfun args res _) =
        text "type" <+> ppr tyfun
        <+> text "::" <+/> ppr (foldr (:=>) res args)

    ppr (AxiomDecl coaxiom k _) =
        text "axiom" <+> ppr coaxiom
        <+> text "::" <+/> ppr k

    ppr (LetDecl (NonRec b) _)  =
        (nest 4 $
        text "let {"
        </> ppr b)
        </> text "}"

    ppr (LetDecl (Rec bs) _)  =
        (nest 4 $
        text "letrec {"
        </> semiseplines (map ppr bs))
        </> text "}"

    ppr (SigDecl v ty _)  =
        ppr v <+> text "::" <+/> ppr ty <> text ";"

instance Pretty ConDecl where
    ppr (ConDecl datacon ty maybe_fields _)
        = ppr datacon <+> pprFields maybe_fields <+> text "::" <+/> ppr ty
      where
        pprFields :: [Var] -> Doc
        pprFields []      = empty
        pprFields fields  = embrace $ commasep $ map ppr fields

instance Pretty Binding where
    ppr (Binding v k _ e _) =
        ppr v <+> text "::" <+/> ppr k <+> text "=" <+/> ppr e
\end{code}

\begin{code}
instance Pretty OccInfo where
    ppr Dead         = text "dead"
    ppr LoopBreaker  = text "loop breaker"
    ppr Once         = text "once"
    ppr OnceInLam    = text "once in lambda"
    ppr ManyBranch   = text "many branch"
    ppr Many         = text "many"
\end{code}

\begin{code}
instance Pretty ArgInfo where
    ppr Scrutinized  = text "scrutinized"
    ppr NoArgInfo    = text "no info"
\end{code}

\begin{code}
instance Pretty BindInfo where
    ppr (BindInfo { bind_occinfo = occinfo, bind_arginfo = arginfo }) =
        ppr occinfo <+> ppr arginfo
\end{code}

\subsubsection{Expressions}

\begin{code}
instance Pretty Lit where
    ppr (IntegerLit n)  = text $ show n
    ppr (FloatLit n)    = text $ show n
    ppr (CharLit c)     = text $ show c
    ppr (StringLit s)   = text $ show s

instance Pretty Exp where
    pprPrec _ (LitExp lit _)  = ppr lit
    pprPrec _ (VarExp v _)    = ppr v
    pprPrec _ (ConExp con _)  = ppr con

    pprPrec p (LamExp v ty e _)
        =  parensIf (p >= 10) $
           text "\\" <> ppr v <+> text "::" <+/> ppr ty
           <+> text "." <+> ppr e

    pprPrec p (TyLamExp tv k e _)
        =  parensIf (p >= 10) $
           text "/\\" <> pprKinded tv k
           <+> text "." <+> ppr e

    pprPrec p (AppExp e1 e2 _)
        = infixOp p (LeftAssoc, 11) empty e1 e2

    pprPrec p (TyAppExp e ty _)
        = parensIf (p > 11) $
          pprPrec 11 e <+> brackets (ppr ty)

    pprPrec p (LetExp (NonRec b) e _) =
        parensIf (p >= 10) $
        (nest 4 $
        text "let {"
        </> ppr b)
        </> text "} in" <+/> ppr e

    pprPrec p (LetExp (Rec bs) e _) =
        parensIf (p >= 10) $
        (nest 4 $
        text "letrec {"
        </> seplines semi (map ppr bs))
        </> text "} in" <+/> ppr e

    pprPrec p (CaseExp e v alts _) =
        parensIf (p >= 10) $
        (nest 4 $
        text "case" <+> ppr e <+> text "of" <+> ppr v <+> text "{"
        </> seplines semi (map ppr alts))
        </> text "}"

    pprPrec p (CastExp e ty _) =
        parensIf (p >= 10) $
        ppr e <+> text "|>" <+> ppr ty
\end{code}

\begin{code}
instance Pretty Alt where
    ppr (Alt p e) = nest 4 $ ppr p <+> text "->" <+> ppr e
\end{code}

\subsubsection{Patterns}

\begin{code}
instance Pretty (WildTyVar, Kind) where
    ppr (wtv, k) = brackets $ ppr wtv <+> text "::" <+> ppr k

instance Pretty (WildVar, Type) where
    ppr (wv, ty) = parens $ ppr wv <+> text "::" <+> ppr ty

instance Pretty Pat where
    pprPrec  _  (LitPat lit _)  = ppr lit
    pprPrec  p  (VarPat pv _)   = pprPrec p pv

    pprPrec  p  (ConPat con ptvs pvs _) =
        parensIf (p >= 11) $
        (spread $ ppr con : map ppr ptvs ++ map ppr pvs)
\end{code}

