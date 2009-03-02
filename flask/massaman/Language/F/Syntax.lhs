%if False
\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

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
-- Module      :  Language.F.Syntax
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.F.Syntax (
    Var(..),
    WildVar(..),
    Con(..),
    TyVar(..),
    WildTyVar(..),
    TyCon(..),
    TyFun(..),
    Sort(..),
    Kind(..),
    Type(..),
    Decl(..),
    ConDecl(..),
    OccInfo(..),
    ArgInfo(..),
    BindInfo(..),
    defaultBindInfo,
    Binding(..),
    Lit(..),
    Exp(..),
    Alt(..),
    Pat(..),
    allTyVars,
    boundVar,
    unfoldKindFun,
    destructKindFun,
    isCo,
    destructCo,
    unfoldAppTy,
    isFunTy,
    unfoldFunTy,
    destructFunTy,
    unfoldForAll,
    unfoldAppExp,
    unfoldTyAppExp,
    groupBindings,
    bindings
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List ((\\),
                  foldl')
import Maybe (fromMaybe,
              fromJust)

import Data.Loc
import Data.Name
import Text.PrettyPrint.Mainland ()
\end{code}
%endif

\section{F Abstract Syntax}

\subsection{Binders}

\begin{code}
data TyVar = TyVar Name
    deriving (Eq, Ord)

data WildTyVar  =  WildTyVar
                |  TameTyVar TyVar
    deriving (Eq, Ord)

data TyCon  =  TyCon Name
            |  TupleTyCon Int
    deriving (Eq, Ord)

data TyFun = TyFun Name
    deriving (Eq, Ord)
\end{code}

\begin{code}
data Var = Var Name
    deriving (Eq, Ord)

data WildVar  =  WildVar
              |  TameVar Var
    deriving (Eq, Ord)

data Con  =  Con Name
          |  TupleCon Int
    deriving (Eq, Ord)
\end{code}

\begin{code}
instance Located TyVar where
    getLoc (TyVar n) = getLoc n

instance Located WildTyVar where
    getLoc WildTyVar         = internalLoc
    getLoc (TameTyVar alpha) = getLoc alpha

instance Located TyCon where
    getLoc (TyCon n)       = getLoc n
    getLoc (TupleTyCon _)  = internalLoc

instance Located TyFun where
    getLoc (TyFun n) = getLoc n

instance Located Var where
    getLoc (Var n) = getLoc n

instance Located WildVar where
    getLoc WildVar         = internalLoc
    getLoc (TameVar alpha) = getLoc alpha

instance Located Con where
    getLoc (Con n)       = getLoc n
    getLoc (TupleCon _)  = internalLoc
\end{code}

\begin{code}
instance NameBinding TyVar where
    bindingName  (TyVar n)  = n
    bindingLoc   (TyVar n)  = nameLoc n

instance NameBinding TyCon where
    bindingName  (TyCon n)       = n
    bindingName  (TupleTyCon _)  = error "bad binding"
    bindingLoc   (TyCon n)       = nameLoc n
    bindingLoc   (TupleTyCon _)  = error "bad binding location"

instance NameBinding Var where
    bindingName  (Var n)  = n
    bindingLoc   (Var n)  = nameLoc n

instance NameBinding Con where
    bindingName  (Con n)       = n
    bindingName  (TupleCon _)  = error "bad binding"
    bindingLoc   (Con n)       = nameLoc n
    bindingLoc   (TupleCon _)  = error "bad binding location"
\end{code}

\subsection{Sorts}

\begin{code}
data Sort  =  TY
           |  CO
    deriving (Eq, Ord)
\end{code}

\subsection{Kinds}

\begin{code}
infixr :=>

data Kind  =  (:*)
           |  Kind  :=>  Kind
           |  Type  :~   Type
  deriving (Eq, Ord)
\end{code}

\subsection{Types}

\begin{code}
data Type  =  TyConTy TyCon SrcLoc
           |  TyVarTy TyVar SrcLoc
           |  AppTy Type Type SrcLoc
           |  AppTyFunTy TyFun [Type] SrcLoc
           |  ForAll WildTyVar Kind Type SrcLoc
           |  SymCo Type SrcLoc
           |  TransCo Type Type SrcLoc
           |  AppCo Type Type SrcLoc
           |  LeftCo Type SrcLoc
           |  RightCo Type SrcLoc
  deriving (Eq, Ord)
\end{code}

\subsection{Declarations}

\begin{code}
data Decl  =  DataDecl TyCon Kind [ConDecl] SrcLoc
           |  TypeDecl TyFun [Kind] Kind SrcLoc
           |  AxiomDecl TyCon Kind SrcLoc
           |  LetDecl (Rec Binding) SrcLoc
           |  SigDecl Var Type SrcLoc
  deriving (Eq, Ord)

data ConDecl  =  ConDecl Con Type [Var] SrcLoc
  deriving (Eq, Ord)
\end{code}

\subsection{Bindings and Occurence Information}

Bindings are annotated with occurence info in the form of the |OccInfo| data
type, inspired by~\cite{jones99secrets}.

\begin{code}
data OccInfo  =  Dead
              |  LoopBreaker
              |  Once
              |  OnceInLam
              |  ManyBranch
              |  Many
  deriving (Eq, Ord, Enum)

data ArgInfo  =  Scrutinized
              |  NoArgInfo
  deriving (Eq, Ord, Enum)

data BindInfo = BindInfo
    {  bind_occinfo :: OccInfo
    ,  bind_arginfo :: [ArgInfo]
    }
  deriving (Eq, Ord)

defaultBindInfo :: BindInfo
defaultBindInfo = BindInfo
    {  bind_occinfo = Dead
    ,  bind_arginfo = []
    }
\end{code}

\begin{description}

\item[|Dead|.] The binder does not occur at all. A |Binding| is initialized with
this value. When performing the bottom-up occurence calculation, no variable
should ever map to this value.

\item[|LoopBreaker|.] The binder has been chosen as a loop breaker. When
performing the bottom-up occurence calculation, no variable should ever map to
this value.

\item[|Once|.] The binder occurs exactly once, neither under a lambda, nor as
the argument to a constructor.

\item[|OnceInLam|.] The binder occurs exactly once, but inside a lambda.

\item[|ManyBranch|.] The binder occurs at most once in each of several distinct
|case| branches.

\item[|Many|.] The binder may occur many times, including inside lambdas.
\end{description}

\begin{code}
data Binding = Binding Var Type BindInfo Exp SrcLoc
  deriving (Eq, Ord)
\end{code}

\subsection{Expressions}

\begin{code}
data Lit  =  IntegerLit Integer
          |  FloatLit Double
          |  CharLit Char
          |  StringLit String
  deriving (Eq, Ord)

data Exp  =  LitExp Lit SrcLoc
          |  ConExp Con SrcLoc
          |  VarExp Var SrcLoc
          |  LamExp Var Type Exp SrcLoc
          |  AppExp Exp Exp SrcLoc
          |  TyLamExp TyVar Kind Exp SrcLoc
          |  TyAppExp Exp Type SrcLoc
          |  LetExp (Rec Binding) Exp SrcLoc
          |  CaseExp Exp Var [Alt] SrcLoc
          |  CastExp Exp Type SrcLoc
  deriving (Eq, Ord)

data Alt = Alt Pat Exp
  deriving (Eq, Ord)
\end{code}

\subsection{Patterns}

\begin{code}
data Pat  =  LitPat Lit SrcLoc
          |  VarPat (WildVar, Type) SrcLoc
          |  ConPat Con [(WildTyVar, Kind)] [(WildVar, Type)] SrcLoc
  deriving (Eq, Ord)
\end{code}

\subsection{Smart Deconstructors}

\subsection{Utilities}

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

instance Freshname Var where
    freshname phi (Var n) =
        head $ binders \\ toList phi
      where
        binders :: [Var]
        binders =  [Var n' | n' <- nameAlts n]
\end{code}

\subsection{Type Construction and Deconstruction}

\begin{code}
boundVar :: Binding -> (Var, Type)
boundVar (Binding v ty _ _ _) = (v, ty)
\end{code}

\begin{code}
unfoldKindFun :: Kind -> [Kind]
unfoldKindFun (k1 :=> k2)      =  k1 : unfoldKindFun k2
unfoldKindFun k                =  [k]
\end{code}

\begin{code}
destructKindFun :: Kind -> ([Kind], Kind)
destructKindFun k =
    case reverse (unfoldKindFun k) of
      res_k : arg_ks  -> (reverse arg_ks, res_k)
      []              -> error "destructKindFun: the impossible happened"
\end{code}

\begin{code}
isCo :: Kind -> Bool
isCo (_ :~ _)  =  True
isCo _         =  False
\end{code}

\begin{code}
destructCo :: Kind -> (Type, Type)
destructCo (ty1 :~ ty2)  =  (ty1, ty2)
destructCo _             =  error "destructCo: not a coercion"
\end{code}

\begin{code}
unfoldAppTy :: Type -> [Type]
unfoldAppTy  (AppTy ty1 ty2 _)  =  unfoldAppTy ty1 ++ [ty2]
unfoldAppTy  ty                 =  [ty]
\end{code}

\begin{code}
isFunTy :: Type -> Bool
isFunTy  (AppTy (AppTy (TyConTy (TyCon tycon) _) _ _) _ _)
    | tycon == builtinArrow  = True
isFunTy  (ForAll _ _ ty _)   = isFunTy ty
isFunTy  _                   = False
\end{code}

\begin{code}
unfoldFunTy :: Type -> [Type]
unfoldFunTy  (AppTy (AppTy (TyConTy (TyCon tycon) _) ty1 _) ty2 _)
    | tycon == builtinArrow  = ty1 : unfoldFunTy ty2
unfoldFunTy  ty              = [ty]
\end{code}

\begin{code}
destructFunTy :: Type -> ([Type], Type)
destructFunTy ty =
    case reverse (unfoldFunTy ty) of
      res_ty : arg_tys  -> (reverse arg_tys, res_ty)
      []                -> error "destructFunTy: the impossible happened"
\end{code}

\begin{code}
unfoldForAll :: Type -> ([(WildTyVar, Kind)], Type)
unfoldForAll (ForAll tv k ty _) = ((tv, k) : binds, qty)
  where
    (binds, qty) = unfoldForAll ty

unfoldForAll ty = ([], ty)
\end{code}

\begin{code}
unfoldAppExp :: Exp -> [Exp]
unfoldAppExp  (AppExp e1 e2 _)  =  unfoldAppExp e1 ++ [e2]
unfoldAppExp  e                 =  [e]
\end{code}

\begin{code}
unfoldTyAppExp :: Exp -> (Exp, [Type])
unfoldTyAppExp  (TyAppExp e ty _)  =  (e', tys ++ [ty])
  where
    (e', tys) = unfoldTyAppExp e
unfoldTyAppExp  e                  =  (e, [])
\end{code}

\begin{code}
groupBindings :: [Rec Binding] -> [(Var, Binding)]
groupBindings = concatMap go
  where
    go :: Rec Binding -> [(Var, Binding)]
    go  (NonRec b)  = bindings [b]
    go  (Rec bs)    = bindings bs
\end{code}

\begin{code}
bindings :: [Binding] -> [(Var, Binding)]
bindings = reverse . go []
  where
    go :: [(Var, Binding)] -> [Binding] -> [(Var, Binding)]
    go result  []                             = result
    go result  (b@(Binding v _ _ _ _)  : bs)  = go  ((v, b) : result)  bs
\end{code}

\subsection{Calculating Free and Bound Variables}

\begin{code}
instance HasVars Var Var where
    free v    = Set.singleton v
    occurs v  = Set.singleton v
\end{code}

\begin{code}
instance HasVars TyVar TyVar where
    free v    = Set.singleton v
    occurs v  = Set.singleton v
\end{code}

\begin{code}
instance HasVars Type TyVar where
    free (TyConTy _ _)                   = Set.empty
    free (TyVarTy tv _)                  = Set.singleton tv
    free (AppTy ty1 ty2 _)               = free ty1 `Set.union` free ty2
    free (AppTyFunTy _ tys _)            = free tys
    free (ForAll WildTyVar _ ty _)       = free ty
    free (ForAll (TameTyVar tv) _ ty _)  = Set.delete tv (free ty)
    free (SymCo ty _)                    = free ty
    free (TransCo ty1 ty2 _)             = free ty1 `Set.union` free ty2
    free (AppCo ty1 ty2 _)               = free ty1 `Set.union` free ty2
    free (LeftCo ty _)                   = free ty
    free (RightCo ty _)                  = free ty

    occurs (TyConTy _ _)                   = Set.empty
    occurs (TyVarTy tv _)                  = Set.singleton tv
    occurs (AppTy ty1 ty2 _)               = occurs ty1 `Set.union` occurs ty2
    occurs (AppTyFunTy _ tys _)            = occurs tys
    occurs (ForAll WildTyVar _ ty _)       = occurs ty
    occurs (ForAll (TameTyVar tv) _ ty _)  = Set.insert tv (occurs ty)
    occurs (SymCo ty _)                    = occurs ty
    occurs (TransCo ty1 ty2 _)             = occurs ty1 `Set.union` occurs ty2
    occurs (AppCo ty1 ty2 _)               = occurs ty1 `Set.union` occurs ty2
    occurs (LeftCo ty _)                   = occurs ty
    occurs (RightCo ty _)                  = occurs ty
\end{code}

\begin{code}
instance HasVars Binding Var where
    free     (Binding _ _ _ e _)  = free e
    occurs   (Binding v _ _ e _)  = Set.insert v (occurs e)
    binders  (Binding v _ _ _ _)  = [v]

instance HasVars (Rec Binding) Var where
    free (NonRec b)  = free b
    free (Rec bs)    = free bs Set.\\ Set.fromList (binders bs)

    occurs (NonRec b)  = occurs b
    occurs (Rec bs)    = occurs bs

    binders (NonRec b)  = binders b
    binders (Rec bs)    = binders bs
\end{code}

\begin{code}
instance HasVars Exp Var where
    free (LitExp _ _)          =  Set.empty
    free (ConExp _ _)          =  Set.empty
    free (VarExp v _)          =  Set.singleton v
    free (LamExp v _ b _)      =  Set.delete v (free b)
    free (AppExp e1 e2 _)      =  free e1 `Set.union` free e2
    free (TyLamExp _ _ e _)    =  free e
    free (TyAppExp e _ _)      =  free e
    free (LetExp bg e _)       =  free bg `Set.union` (free e Set.\\ Set.fromList (binders bg))
    free (CaseExp e v alts _)  =  free e `Set.union` (Set.delete v (free alts))
    free (CastExp e _ _)       =  free e

    occurs (LitExp _ _)          =  Set.empty
    occurs (ConExp _ _)          =  Set.empty
    occurs (VarExp v _)          =  Set.singleton v
    occurs (LamExp v _ b _)      =  Set.insert v (occurs b)
    occurs (AppExp e1 e2 _)      =  occurs e1 `Set.union` occurs e2
    occurs (TyLamExp _ _ e _)    =  occurs e
    occurs (TyAppExp e _ _)      =  occurs e
    occurs (LetExp bg e _)       =  occurs bg `Set.union` occurs e
    occurs (CaseExp e v alts _)  =  Set.insert v (occurs e `Set.union` occurs alts)
    occurs (CastExp e _ _)       =  occurs e
\end{code}

\begin{code}
instance HasVars Alt Var where
    free (Alt p e)    = free e Set.\\ Set.fromList (binders p)
    occurs (Alt p e)  = occurs p `Set.union` occurs e
\end{code}

\begin{code}
instance HasVars WildTyVar TyVar where
    occurs WildTyVar        = Set.empty
    occurs (TameTyVar tv)   = Set.singleton tv

    binders WildTyVar       = []
    binders (TameTyVar tv)  = [tv]

instance HasVars (WildVar, Type) Var where
    binders (TameVar v, _)  = [v]
    binders (WildVar, _)    = []
\end{code}

\begin{code}
instance HasVars Pat Var where
    occurs (LitPat _ _)           =  Set.empty
    occurs (VarPat pv _)          =  occurs pv
    occurs (ConPat _ ptvs pvs _)  =  occurs ptvs `Set.union` occurs pvs

    binders (LitPat _ _)        = []
    binders (VarPat pv _)       = binders pv
    binders (ConPat _ _ pvs _)  = binders pvs
\end{code}

\begin{code}
instance HasVars Kind TyVar where
    occurs (:*)                  = Set.empty
    occurs (k1 :=> k2)           = occurs k1 `Set.union` occurs k2
    occurs (gamma_1 :~ gamma_2)  = occurs gamma_1 `Set.union` occurs gamma_2
\end{code}

\begin{code}
instance HasVars Exp TyVar where
    occurs _ = Set.empty
\end{code}

\begin{code}
instance HasVars WildTyVar Var where
    occurs _ = Set.empty
\end{code}

\begin{code}
instance HasVars Kind Var where
    occurs _ = Set.empty
\end{code}

\begin{code}
instance HasVars WildVar Var where
    occurs WildVar      = Set.empty
    occurs (TameVar v)  = Set.singleton v
\end{code}

\subsection{Performing Substitutions}

\subsubsection{Substituting for Type Variables}

\begin{code}
instance CanSubst Type TyVar Kind where
    subst  _      _    kappa@(:*)             =  kappa
    subst  _      _    kappa@(_ :=> _)        =  kappa
    subst  theta  phi  (gamma_1 :~ gamma_2)   =  subst theta phi gamma_1 :~
                                                 subst theta phi gamma_2
\end{code}

\begin{code}
instance CanSubst Type TyVar Type where
    subst  _      _    tau@(TyConTy _ _)         = tau
    subst  theta  _    tau@(TyVarTy alpha _)     = fromMaybe tau (Map.lookup alpha theta)
    subst  theta  phi  (AppTy tau1 tau2 sloc)    = AppTy  (subst theta phi tau1)
                                                          (subst theta phi tau2)
                                                          sloc
    subst  theta  phi  (AppTyFunTy f taus sloc)  = AppTyFunTy f (subst theta phi taus) sloc

    subst  theta  phi  (SymCo tau sloc)          = SymCo (subst theta phi tau) sloc
    subst  theta  phi  (TransCo tau1 tau2 sloc)  = TransCo  (subst theta phi tau1)
                                                            (subst theta phi tau2)
                                                            sloc
    subst  theta  phi  (AppCo tau1 tau2 sloc)    = AppCo  (subst theta phi tau1)
                                                          (subst theta phi tau2)
                                                          sloc
    subst  theta  phi  (LeftCo tau sloc)         = LeftCo (subst theta phi tau) sloc
    subst  theta  phi  (RightCo tau sloc)        = RightCo (subst theta phi tau) sloc

    subst  theta  phi  (ForAll WildTyVar kappa tau sloc) =
        ForAll WildTyVar (subst theta phi kappa) (subst theta phi tau) sloc

    subst  theta  phi  (ForAll (TameTyVar alpha) kappa tau sloc)
        | alpha `Set.member` phi = ForAll  (TameTyVar alpha')
                                           (subst theta' phi' kappa)
                                           (subst theta' phi' tau)
                                           sloc
      where
        alpha'  = freshname phi alpha
        theta'  = Map.insert alpha (TyVarTy alpha' sloc) theta
        phi'    = Set.insert alpha' phi

    subst  theta  phi  (ForAll wtv@(TameTyVar alpha) kappa tau sloc) =
        ForAll wtv (subst theta' phi' kappa) (subst theta' phi' tau) sloc
      where
        theta'  = Map.delete alpha theta
        phi'    = Set.insert alpha phi
\end{code}

\begin{code}
instance CanSubst Type TyVar Binding where
    subst theta phi (Binding v tau occ e sloc) =
        Binding v  (subst theta phi tau)
                   occ
                   (subst theta phi e)
                   sloc

instance CanSubst Type TyVar (Rec Binding) where
    subst theta phi (NonRec b)  =  NonRec (subst theta phi b)
    subst theta phi (Rec bs)    =  Rec (subst theta phi bs)
\end{code}

\begin{code}
instance CanSubst Type TyVar Exp where
    subst  _      _    e@(LitExp _ _)           =  e
    subst  _      _    e@(ConExp _ _)           =  e
    subst  _      _    e@(VarExp _ _)           =  e
    subst  theta  phi  (LamExp v tau b sloc)    =  LamExp v  (subst theta phi tau)
                                                             (subst theta phi b)
                                                             sloc
    subst  theta  phi  (AppExp e1 e2 sloc)      =  AppExp  (subst theta phi e1)
                                                           (subst theta phi e2)
                                                           sloc
    subst  theta  phi  (TyAppExp e tau sloc)    =  TyAppExp  (subst theta phi e)
                                                             (subst theta phi tau)
                                                             sloc
    subst  theta  phi  (LetExp bg e sloc)       =  LetExp  (subst theta phi bg)
                                                           (subst theta phi e)
                                                           sloc
    subst  theta  phi  (CaseExp e v alts sloc)  =  CaseExp  (subst theta phi e) v
                                                            (subst theta phi alts)
                                                            sloc
    subst  theta  phi  (CastExp e tau sloc)     =  CastExp  (subst theta phi e)
                                                            (subst theta phi tau)
                                                            sloc

    subst  theta  phi  (TyLamExp alpha kappa body sloc)
        | alpha `Set.member` phi =  TyLamExp alpha'  (subst theta' phi' kappa)
                                                     (subst theta' phi' body)
                                                     sloc
      where
        alpha'  = freshname phi alpha
        theta'  = Map.insert alpha (TyVarTy alpha' sloc) theta
        phi'    = Set.insert alpha' phi

    subst  theta  phi  (TyLamExp alpha kappa body sloc) =
        TyLamExp  alpha
                  (subst theta' phi' kappa)
                  (subst theta' phi' body)
                  sloc
      where
        theta'  = Map.delete alpha theta
        phi'    = Set.insert alpha phi
\end{code}

\begin{code}
instance CanSubst Type TyVar Alt where
    subst theta phi (Alt p e) = Alt p' (subst theta' phi' e)
      where
        (theta', phi', p') = subste theta phi p
\end{code}

\begin{code}
instance CanSubst Type TyVar Pat where
    subste  theta  phi  p@(LitPat _ _) =
        (theta, phi, p)

    subste  theta  phi  (VarPat (wv, tau) sloc) =
        (theta, phi, VarPat (wv, subst theta phi tau) sloc)

    subste  theta  phi  (ConPat con ptvs pvs sloc) =
        (theta', phi', ConPat con (reverse ptvs') (wvs `zip` taus') sloc)
      where
        wvs   :: [WildVar]
        taus  :: [Type]
        (wvs, taus) = unzip pvs

        taus' :: [Type]
        taus' = subst theta phi taus

        theta' :: Map.Map TyVar Type
        phi'   :: Set.Set TyVar
        ptvs'  :: [(WildTyVar, Kind)]
        (theta', phi', ptvs') = foldl' f (theta, phi, []) ptvs

        f  ::  (Map.Map TyVar Type, Set.Set TyVar, [(WildTyVar, Kind)])
           ->  (WildTyVar, Kind)
           ->  (Map.Map TyVar Type, Set.Set TyVar, [(WildTyVar, Kind)])
        f (theta, phi, bs) (WildTyVar, kappa) =
            (theta, phi, (WildTyVar, subst theta phi kappa) : bs)

        f (theta, phi, bs) (TameTyVar alpha, kappa)
            | alpha `Set.member` phi =
                (theta', phi', (TameTyVar alpha', subst theta phi kappa) : bs)
         where
           alpha'  = freshname phi alpha
           theta'  = Map.insert alpha (TyVarTy alpha' sloc) theta
           phi'    = Set.insert alpha' phi

        f (theta, phi, bs) (wtv@(TameTyVar alpha), kappa) =
            (theta', phi', (wtv, subst theta phi kappa) : bs)
         where
           theta'  = Map.delete alpha theta
           phi'    = Set.insert alpha phi
\end{code}

\subsubsection{Substituting for Variables}

\begin{code}
instance CanSubst Exp Var (Rec Binding) where
    subste theta phi (NonRec (Binding v tau occ e sloc))
        | v `Set.member` phi =
            (theta', phi', NonRec (Binding v' tau occ (subst theta' phi' e) sloc))
     where
       v'      = freshname phi v
       theta'  = Map.insert v (VarExp v' (SrcLoc internalLoc)) theta
       phi'    = Set.insert v' phi

    subste theta phi (NonRec (Binding v tau occ e sloc)) =
        (theta', phi', NonRec (Binding v tau occ (subst theta' phi' e) sloc))
     where
       theta'  = Map.delete v theta
       phi'    = Set.insert v phi

    subste theta phi (Rec bs) =
        (theta', phi', Rec $ map subst' bs)
     where
       (theta', phi', theta_v) = foldl' f (theta, phi, Map.empty) bs

       binder :: Binding -> Var
       binder (Binding v _ _ _ _) = v

       subst' :: Binding -> Binding
       subst'  (Binding v ty occ e sloc) =
           Binding  (fromJust $ Map.lookup v theta_v)
                    ty
                    occ
                    (subst theta' phi' e)
                    sloc

       f  ::  (Map.Map Var Exp, Set.Set Var, Map.Map Var Var)
          ->  Binding
          ->  (Map.Map Var Exp, Set.Set Var, Map.Map Var Var)
       f (theta, phi, theta_v) b
           | v `Set.member` phi =
               (theta', phi', theta_v')
         where
           v         = binder b
           v'        = freshname phi v
           theta'    = Map.insert v (VarExp v' (SrcLoc internalLoc)) theta
           phi'      = Set.insert v' phi
           theta_v'  = Map.insert v v' theta_v

       f (theta, phi, theta_v) b =
           (theta', phi', theta_v')
         where
           v         = binder b
           theta'    = Map.delete v theta
           phi'      = Set.insert v phi
           theta_v'  = Map.insert v v theta_v
\end{code}

\begin{code}
instance CanSubst Exp Var Exp where
    subst  _      _    e@(LitExp _ _)               =  e
    subst  _      _    e@(ConExp _ _)               =  e
    subst  theta  _    e@(VarExp v _)               =  fromMaybe e (Map.lookup v theta)
    subst  theta  phi  (AppExp e1 e2 sloc)          =  AppExp  (subst theta phi e1)
                                                            (subst theta phi e2)
                                                            sloc
    subst  theta  phi  (TyLamExp tv kappa e sloc)   =  TyLamExp  tv kappa
                                                                 (subst theta phi e)
                                                                 sloc
    subst  theta  phi  (TyAppExp e tau sloc)        =  TyAppExp  (subst theta phi e) tau
                                                                 sloc
    subst  theta  phi  (CastExp e tau sloc)         =  CastExp  (subst theta phi e) tau
                                                                sloc

    subst  theta  phi  (LamExp v tau e sloc)
        | v `Set.member` phi =
            LamExp v' tau (subst theta' phi' e) sloc
      where
        v'      = freshname phi v
        theta'  = Map.insert v (VarExp v' sloc) theta
        phi'    = Set.insert v' phi

    subst theta phi (LamExp v tau e sloc)=
        LamExp v tau (subst theta' phi' e) sloc
      where
        theta'  = Map.delete v theta
        phi'    = Set.insert v phi

    subst theta phi (LetExp bg body sloc) =
        LetExp bg' (subst theta' phi' body) sloc
      where
          (theta', phi', bg') = subste theta phi bg

    subst theta phi (CaseExp scrut v alts sloc)
        | v `Set.member` phi =
            CaseExp (subst theta phi scrut) v' (subst theta' phi' alts) sloc
      where
        v'      = freshname phi v
        theta'  = Map.insert v (VarExp v' sloc) theta
        phi'    = Set.insert v' phi

    subst theta phi (CaseExp scrut v alts sloc) =
        CaseExp (subst theta phi scrut) v (subst theta' phi' alts) sloc
      where
        theta'  = Map.delete v theta
        phi'    = Set.insert v phi
\end{code}

\begin{code}
instance CanSubst Exp Var Alt where
    subst theta phi (Alt p e) = Alt p' (subst theta' phi' e)
      where
        (theta', phi', p') = subste theta phi p
\end{code}

\begin{code}
instance CanSubst Exp Var Pat where
    subste  theta  phi  p@(LitPat _ _)  =
        (theta, phi, p)

    subste  theta  phi  p@(VarPat (WildVar, _) _) =
        (theta, phi, p)

    subste  theta  phi  (VarPat (TameVar v, tau) sloc)
        | v `Set.member` phi =
            (theta', phi', VarPat (TameVar v', tau) sloc)
      where
        v'      = freshname phi v
        theta'  = Map.insert v (VarExp v' sloc) theta
        phi'    = Set.insert v' phi

    subste  theta  phi  p@(VarPat (TameVar v, _) _) =
            (theta', phi', p)
      where
        theta'  = Map.delete v theta
        phi'    = Set.insert v phi

    subste  theta  phi  (ConPat con ptvs pvs sloc) =
        (theta', phi', ConPat con ptvs (reverse pvs') sloc)
      where
        theta'  :: Map.Map Var Exp
        phi'    :: Set.Set Var
        pvs'    :: [(WildVar, Type)]
        (theta', phi', pvs') = foldl' f (theta, phi, []) pvs

        f  ::  (Map.Map Var Exp, Set.Set Var, [(WildVar, Type)])
           ->  (WildVar, Type)
           ->  (Map.Map Var Exp, Set.Set Var, [(WildVar, Type)])
        f (theta, phi, bs) (WildVar, tau) =
            (theta, phi, (WildVar, tau) : bs)

        f (theta, phi, bs) (TameVar v, tau)
            | v `Set.member` phi =
                (theta', phi', (TameVar v', tau) : bs)
         where
           v'      = freshname phi v
           theta'  = Map.insert v (VarExp v' sloc) theta
           phi'    = Set.insert v' phi

        f (theta, phi, bs) b@(TameVar v, _) =
            (theta', phi', b : bs)
         where
           theta'  = Map.delete v theta
           phi'    = Set.insert v phi
\end{code}

\begin{code}
instance Located Type where
    getLoc  (TyConTy _ loc)       = toLoc loc
    getLoc  (TyVarTy _ loc)       = toLoc loc
    getLoc  (AppTy _ _ loc)       = toLoc loc
    getLoc  (AppTyFunTy _ _ loc)  = toLoc loc
    getLoc  (ForAll _ _ _ loc)    = toLoc loc
    getLoc  (SymCo _ loc)         = toLoc loc
    getLoc  (TransCo _ _ loc)     = toLoc loc
    getLoc  (AppCo _ _ loc)       = toLoc loc
    getLoc  (LeftCo _ loc)        = toLoc loc
    getLoc  (RightCo _ loc)       = toLoc loc

instance Located Decl where
    getLoc  (DataDecl _ _ _ loc)  = toLoc loc
    getLoc  (TypeDecl _ _ _ loc)  = toLoc loc
    getLoc  (AxiomDecl _ _ loc)   = toLoc loc
    getLoc  (LetDecl _ loc)       = toLoc loc
    getLoc  (SigDecl _ _ loc)     = toLoc loc

instance Located ConDecl where
    getLoc (ConDecl _ _ _ loc) = toLoc loc

instance Located Binding where
    getLoc (Binding _ _ _ _ loc) = toLoc loc

instance Located (Rec Binding) where
    getLoc (NonRec b)  = getLoc b
    getLoc (Rec bs)    = getLoc (head bs) <--> getLoc (last bs)

instance Located Exp where
    getLoc  (LitExp _ loc)        = toLoc loc
    getLoc  (ConExp _ loc)        = toLoc loc
    getLoc  (VarExp _ loc)        = toLoc loc
    getLoc  (LamExp _ _ _ loc)    = toLoc loc
    getLoc  (AppExp _ _ loc)      = toLoc loc
    getLoc  (TyLamExp _ _ _ loc)  = toLoc loc
    getLoc  (TyAppExp _ _ loc)    = toLoc loc
    getLoc  (LetExp _ _ loc)      = toLoc loc
    getLoc  (CaseExp _ _ _ loc)   = toLoc loc
    getLoc  (CastExp _ _ loc)     = toLoc loc

instance Located Pat where
    getLoc  (LitPat _ loc)      = toLoc loc
    getLoc  (VarPat _ loc)      = toLoc loc
    getLoc  (ConPat _ _ _ loc)  = toLoc loc
\end{code}
