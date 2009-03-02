%if False
\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
-- Module      :  Check.Hs.Types
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Check.Hs.Types (
    TyVar(..),
    TyCon(..),
    MetaKv(..),
    KindRef,
    Kind(..),
    MetaTv(..),
    TyRef,
    Type(..),
    Pred(..),
    Context,
    Tau,
    Rho,
    Sigma,
    allTyVars,
    tyVarName,
    isTyVar,
    pprKinded,
    forallT,
    (-->),
    unfoldKindFun,
    foldAppTy,
    unfoldAppTy,
    isFunTy,
    unfoldFunTy,
    destructFunTy,
    destructForAll,
    destructForAllFunTy
  ) where

import Data.List (foldl', (\\))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

import Control.Monad.Unique
import Text.PrettyPrint.Mainland
import Data.Name
\end{code}
%endif

\section{The Type Checker's Type System}

\subsection{Bindings}

\begin{code}
data TyVar =  TyVar Name
           |  SkolemTyVar Name Uniq
    deriving (Eq, Ord)

data TyCon  =  TyCon Name
            |  TupleTyCon Int
    deriving (Eq, Ord)
\end{code}

\subsection{Kinds}

\begin{code}
infixr :=>

data MetaKv r = MetaK Uniq (KindRef r)

type KindRef r = r (Maybe (Kind r))

data Kind r  =  (:*)
             |  MetaKv (MetaKv r)
             |  (Kind r) :=> (Kind r)
  deriving (Eq, Ord)
\end{code}

\subsection{Types}

\begin{code}
data MetaTv r = Meta Uniq (TyRef r)

type TyRef r = r (Maybe (Tau r))

data Type r  =  TyConTy TyCon
             |  TyVarTy TyVar
             |  MetaTv (MetaTv r)
             |  AppTy (Type r) (Type r)
             |  ForAll [(TyVar, Kind r)] (Context r) (Type r)
  deriving (Eq, Ord)

data Pred r = ClassPred TyCon [Type r]
  deriving (Eq, Ord)

type Context r = [Pred r]

type Tau = Type

type Rho = Type

type Sigma = Type
\end{code}

\subsection{Utilities}

\begin{code}
instance HasVars (Type r) (MetaTv r) where
    free (TyConTy _)         = Set.empty
    free (TyVarTy _)         = Set.empty
    free (MetaTv mtv)        = Set.singleton mtv
    free (AppTy tau1 tau2)   = free tau1 `Set.union` free tau2
    free (ForAll _ ctx tau)  = free ctx `Set.union` free tau

    occurs (TyConTy _)         = Set.empty
    occurs (TyVarTy _)         = Set.empty
    occurs (MetaTv mtv)        = Set.singleton mtv
    occurs (AppTy tau1 tau2)   = occurs tau1 `Set.union` occurs tau2
    occurs (ForAll _ ctx tau)  = occurs ctx `Set.union` occurs tau
\end{code}

\begin{code}
instance HasVars (Type r) TyVar where
    free (TyConTy _)           =  Set.empty
    free (TyVarTy tv)          =  Set.singleton tv
    free (MetaTv _)            =  Set.empty
    free (AppTy tau1 tau2)     =  free tau1 `Set.union` free tau2
    free (ForAll tvs ctx tau)  =  foldl'  (flip Set.delete)
                                          (free ctx `Set.union` free tau)
                                          (map fst tvs)
\end{code}

\begin{code}
instance HasVars (Pred r) (MetaTv r) where
    free (ClassPred _ tys) = free tys

    occurs (ClassPred _ tys) = occurs tys
\end{code}

\begin{code}
instance HasVars (Pred r) TyVar where
    free (ClassPred _ tys) = free tys
\end{code}

\begin{code}
allTyVars :: [TyVar]
allTyVars =  [TyVar (mkName [x]) | x <- ['a'..'z']] ++
             [TyVar (mkName (x : show i)) |  i <- [1 :: Integer ..],
                                             x <- ['a'..'z']]

instance Freshname TyVar where
    freshname phi _ =
        head $ binders \\ toList phi
      where
        binders :: [TyVar]
        binders =  [TyVar (mkName [x])           |  x <- ['a'..'z']] ++
                   [TyVar (mkName (x : show i))  |  x <- ['a'..'z'],
                                                    i <- [1 :: Integer ..]]
\end{code}

\subsection{Type Construction and Deconstruction}

\begin{code}
forallT :: [(TyVar, Kind r)] -> Context r -> Type r -> Type r
forallT  []    _      tau  = tau
forallT  tvks  gamma  tau  = ForAll tvks gamma tau
\end{code}

\begin{code}
infixr -->

(-->) :: Type r -> Type r -> Type r
arg --> res = AppTy (AppTy (TyConTy (TyCon builtinArrow)) arg) res
\end{code}

\begin{code}
tyVarName :: TyVar -> Name
tyVarName  (TyVar name)          = name
tyVarName  (SkolemTyVar name _)  = name
\end{code}

\begin{code}
isTyVar :: Type r -> Bool
isTyVar  (TyVarTy _)  = True
isTyVar  _            = False
\end{code}

\begin{code}
unfoldKindFun :: Kind r -> [Kind r]
unfoldKindFun (k1 :=> k2)  =  k1 : unfoldKindFun k2
unfoldKindFun k            =  [k]
\end{code}

\begin{code}
foldAppTy :: Type r -> [Type r] -> Type r
foldAppTy z tys = foldl' AppTy z tys
\end{code}

\begin{code}
unfoldAppTy :: Type r -> [Type r]
unfoldAppTy  (AppTy ty1 ty2)  =  unfoldAppTy ty1 ++ [ty2]
unfoldAppTy  ty               =  [ty]
\end{code}

\begin{code}
isFunTy :: Type r -> Bool
isFunTy  (AppTy (AppTy (TyConTy (TyCon tycon)) _) _)
    | tycon == builtinArrow  = True
isFunTy  _                   = False
\end{code}

\begin{code}
unfoldFunTy :: Type r -> [Type r]
unfoldFunTy  (AppTy (AppTy (TyConTy (TyCon tycon)) ty1) ty2)
    | tycon == builtinArrow  = ty1 : unfoldFunTy ty2
unfoldFunTy  ty              = [ty]
\end{code}

\begin{code}
destructFunTy :: Type r -> ([Type r], Type r)
destructFunTy ty =
    case reverse (unfoldFunTy ty) of
      res_ty : arg_tys  -> (reverse arg_tys, res_ty)
      []                -> error "destructFunTy: the impossible happened"
\end{code}

\begin{code}
destructForAll :: Type r -> ([(TyVar, Kind r)], Context r, Type r)
destructForAll (ForAll binds ctx ty)  = (binds, ctx, ty)
destructForAll ty                     = ([], [], ty)
\end{code}

\begin{code}
destructForAllFunTy  ::  Type r
                     ->  ([(TyVar, Kind r)], Context r, [Type r], Type r)
destructForAllFunTy ty = let  (quals, ctx, ty')  = destructForAll ty
                              (arg_tys, ret_ty)  = destructFunTy ty'
                         in
                           (quals, ctx, arg_tys, ret_ty)
\end{code}

\begin{code}
instance NameBinding TyVar where
    bindingName  (TyVar n)          =  n
    bindingName  (SkolemTyVar _ _)  =  error
                                       "bindingName: encountered SkolemTyVar"

    bindingLoc  (TyVar n)          =  nameLoc n
    bindingLoc  (SkolemTyVar _ _)  =  error
                                      "bindingLoc: encountered SkolemTyVar"
\end{code}

\subsection{Performing Substitutions}

\begin{code}
instance CanSubst (Tau r) (MetaTv r) (Type r) where
    subst  _      _    tau@(TyConTy _)      =  tau
    subst  _      _    tau@(TyVarTy _)      =  tau
    subst  theta  _    tau@(MetaTv mv)      =  fromMaybe tau (Map.lookup mv theta)
    subst  theta  phi  (AppTy tau1 tau2)    =  AppTy  (subst theta phi tau1)
                                                      (subst theta phi tau2)
    subst  theta  phi  (ForAll bs ctx tau)  =  ForAll bs  (subst theta phi ctx)
                                                          (subst theta phi tau)
\end{code}

\begin{code}
instance CanSubst (Tau r) TyVar (Type r) where
    subst  _      _    tau@(TyConTy _)      =  tau
    subst  theta  _    tau@(TyVarTy tv)     =  fromMaybe tau (Map.lookup tv theta)
    subst  _      _    tau@(MetaTv _)       =  tau
    subst  theta  phi  (AppTy tau1 tau2)    =  AppTy  (subst theta phi tau1)
                                                    (subst theta phi tau2)

    subst  theta  phi  (ForAll tvs ctx tau) =
        ForAll (reverse tvs') (subst theta' phi' ctx) (subst theta' phi' tau)
      where
        theta' :: Map.Map TyVar (Tau r)
        phi'   :: Set.Set TyVar
        tvs'  :: [(TyVar, Kind r)]
        (theta', phi', tvs') = foldl' f (theta, phi, []) tvs

        f  ::  (Map.Map TyVar (Tau r), Set.Set TyVar, [(TyVar, Kind r)])
           ->  (TyVar, Kind r)
           ->  (Map.Map TyVar (Tau r), Set.Set TyVar, [(TyVar, Kind r)])
        f (theta, phi, bs) (alpha, kappa)
            | alpha `Set.member` phi =
                (theta', phi', (alpha', kappa) : bs)
         where
           alpha'  = freshname phi alpha
           theta'  = Map.insert alpha (TyVarTy alpha') theta
           phi'    = Set.insert alpha' phi

        f (theta, phi, bs) (alpha, kappa) =
            (theta', phi', (alpha, kappa) : bs)
         where
           theta'  = Map.delete alpha theta
           phi'    = Set.insert alpha phi
\end{code}

\begin{code}
instance CanSubst (Tau r) (MetaTv r) (Pred r) where
    subst theta phi (ClassPred tycon tys) =
        ClassPred tycon (map (subst theta phi) tys)
\end{code}

\begin{code}
instance CanSubst (Tau r) TyVar (Pred r) where
    subst theta phi (ClassPred tycon tys) =
        ClassPred tycon (map (subst theta phi) tys)
\end{code}

\subsection{{\tt Eq} and {\tt Ord} Instances}

\subsubsection{Kinds}

\begin{code}
instance Eq (MetaKv r) where
    (MetaK u1 _) == (MetaK u2 _) = u1 == u2

instance Ord (MetaKv r) where
    (MetaK u1 _) <= (MetaK u2 _) = u1 <= u2
\end{code}

\subsubsection{Types}

\begin{code}
instance Eq (MetaTv r) where
    (Meta u1 _) == (Meta u2 _) = u1 == u2

instance Ord (MetaTv r) where
    (Meta u1 _) <= (Meta u2 _) = u1 <= u2
\end{code}

\subsection{Pretty Printing}

\subsubsection{Bindings}

\begin{code}
instance Pretty TyVar where
    ppr (TyVar v)          = ppr v
    ppr (SkolemTyVar v _)  = ppr v
    --ppr (SkolemTyVar v n)  = ppr v <> text "%" <> text (show n)

instance Pretty TyCon where
    ppr (TyCon n)           = ppr n
    ppr (TupleTyCon arity)  = parens $ text $ replicate (arity - 1) ','
\end{code}

\subsubsection{Kinds}

\begin{code}
instance Pretty (MetaKv r) where
    ppr (MetaK u _) = text ("*" ++ show u)

instance Pretty (Kind r) where
    pprPrec _  (:*)             = text "*"
    pprPrec _  (MetaKv meta)    = ppr meta
    pprPrec p  (k1 :=> k2)      = infixOp p (RightAssoc, 0) (text "->") k1 k2
\end{code}

\subsubsection{Types}

\begin{code}
pprKinded :: Pretty a => a -> Kind r -> Doc
pprKinded  a  (:*)  = ppr a
pprKinded  a  k     = ppr a <+> text "::" <+/> ppr k

instance Pretty (MetaTv r) where
    ppr(Meta u _) = text ("%" ++ show u)

instance Pretty (Type r) where
    pprPrec _  (TyConTy tycon)  = ppr tycon
    pprPrec _  (TyVarTy tyvar)  = ppr tyvar
    pprPrec _  (MetaTv meta)    = ppr meta

    pprPrec p ty@(AppTy ty1 ty2) =
        case unfoldAppTy ty of
          [TyConTy (TyCon tycon), ty]
              | tycon == builtinNil ->
                  brackets $ ppr ty
          [TyConTy (TyCon tycon), ty1, ty2]
              | tycon == builtinArrow ->
                  infixOp p (RightAssoc, 0) (text "->") ty1 ty2
          TyConTy (TupleTyCon arity) : tys | arity == length tys ->
              parens $ commasep (map ppr tys)
          _ ->
              infixOp p (LeftAssoc, 1) empty ty1 ty2

    pprPrec p  (ForAll [] [] ty) =
        parensIf (p >= 10) $
        text "\\/ ." <+> ppr ty

    pprPrec p  (ForAll [] ctx ty) =
        parensIf (p >= 10) $
        text "\\/ ." <+> ppr ctx <+> ppr ty

    pprPrec p  (ForAll binds [] ty) =
        parensIf (p >= 10) $
        text "\\/" <> commasep (map (uncurry pprKinded) binds)
        <+> text "." <+> ppr ty

    pprPrec p  (ForAll binds ctx ty) =
        parensIf (p >= 10) $
        text "\\/" <> commasep (map (uncurry pprKinded) binds)
        <+> text "." <+> ppr ctx <+> text "=>" <+> ppr ty
\end{code}

\subsubsection{Predicates}

\begin{code}
instance Pretty (Pred r) where
    ppr (ClassPred con tys) = spread $ ppr con : map ppr tys

    pprList  []  = empty
    pprList  ps  = parens $ commasep (map ppr ps)
\end{code}

\subsection{Show Instances}

\begin{code}
instance Show TyCon where
    show = pprint

instance Show TyVar where
    show = pprint

instance Show (Type r) where
    show = pprint
\end{code}
