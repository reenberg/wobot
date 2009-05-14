%if False
\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- Copyright (c) 2007-2008
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
-- Module      :  Language.F.Smart
-- Copyright   :  (c) Harvard University 2007-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.F.Smart (
    tyVar,
    tyVarAt,
    tyCon,
    tyConAt,
    tyFun,
    tyFunAt,
    var,
    varAt,
    con,
    conAt,
    tyVarT,
    tyConT,
    appT,
    appsT,
    funT,
    funsT,
    (-->),
    appTyFunT,
    forallT,
    forallsT,
    symcoT,
    transcoT,
    appcoT,
    leftcoT,
    rightcoT,
    dataD,
    typeD,
    axiomD,
    letD,
    sigD,
    conD,
    bindD,
    intE,
    floatE,
    charE,
    stringE,
    conE,
    varE,
    lamE,
    lamsE,
    appE,
    appsE,
    tylamE,
    tylamsE,
    tyappE,
    tyappsE,
    letE,
    letsE,
    caseE,
    castE,
    litP,
    varP,
    wildP,
    conP
  ) where

import Data.List (foldl')

import Data.Loc
import Data.Name
import Language.F.Syntax
\end{code}
%endif

\subsection{Smart Constructors}

\begin{code}
tyVar :: String -> TyVar
tyVar = TyVar . mkName

tyVarAt :: String -> Loc -> TyVar
tyVarAt v l = TyVar $ mkNameAt v l

tyCon :: String -> TyCon
tyCon = TyCon . mkName

tyConAt :: String -> Loc -> TyCon
tyConAt v l = TyCon $ mkNameAt v l

tyFun :: String -> TyFun
tyFun = TyFun . mkName

tyFunAt :: String -> Loc -> TyFun
tyFunAt v l = TyFun $ mkNameAt v l

var :: String -> Var
var = Var . mkName

varAt :: String -> Loc -> Var
varAt v l = Var $ mkNameAt v l

con :: String -> Con
con = Con . mkName

conAt :: String -> Loc -> Con
conAt con l = Con $ mkNameAt con l
\end{code}

\begin{code}
tyVarT :: TyVar -> Type
tyVarT tyvar = TyVarTy tyvar l
  where
    l = fromLoc (getLoc tyvar)

tyConT :: TyCon -> Type
tyConT tycon = TyConTy tycon l
  where
    l = fromLoc (getLoc tycon)

appT :: Type -> Type -> Type
appT tau1 tau2 = AppTy tau1 tau2 l
  where
    l = fromLoc $ getLoc tau1 <--> getLoc tau2

appsT :: Type -> [Type] -> Type
appsT tau taus = foldl' appT tau taus

funT :: Type -> Type -> Type
funT tau_arg tau_result = appT (appT arrowT tau_arg) tau_result
  where
    arrowT :: Type
    arrowT = tyConT (TyCon builtinArrow)

funsT :: [Type] -> Type -> Type
funsT taus_arg taus_result = foldr funT taus_result taus_arg

infixr -->
(-->) = funT

appTyFunT :: TyFun -> [Type] -> Type
appTyFunT tau taus = AppTyFunTy tau taus l
  where
    l = fromLoc $ getLoc tau <--> getLoc taus

forallT :: WildTyVar -> Kind -> Type -> Type
forallT alpha k tau = ForAll alpha k tau l
  where
    l = fromLoc $ getLoc alpha <--> getLoc tau

forallsT :: [(TyVar, Kind)] -> Type -> Type
forallsT tvks tau = foldr f tau tvks
  where
    f (tv, k) tau = forallT (TameTyVar tv) k tau

symcoT :: Type -> Type
symcoT tau = SymCo tau l
  where
    l = fromLoc (getLoc tau)

transcoT :: Type -> Type -> Type
transcoT tau1 tau2 = TransCo tau1 tau2 l
  where
    l = fromLoc $ getLoc tau1 <--> getLoc tau2

appcoT :: Type -> Type -> Type
appcoT tau1 tau2 = AppCo tau1 tau2 l
  where
    l = fromLoc $ getLoc tau1 <--> getLoc tau2

leftcoT :: Type -> Type
leftcoT tau = LeftCo tau l
  where
    l = fromLoc (getLoc tau)

rightcoT :: Type -> Type
rightcoT tau = RightCo tau l
  where
    l = fromLoc (getLoc tau)
\end{code}

\begin{code}
dataD :: TyCon -> Kind -> [ConDecl] -> Decl
dataD tycon k condecls = DataDecl tycon k condecls l
  where
    l = case condecls of
          [] ->  fromLoc $ getLoc tycon
          _ ->   fromLoc $ getLoc tycon <--> getLoc condecls

typeD :: TyFun -> [Kind] -> Kind -> Decl
typeD tyfun kappas kappa = TypeDecl tyfun kappas kappa l
  where
    l = fromLoc $ getLoc tyfun

axiomD :: TyCon -> Kind -> Decl
axiomD tycon kappa = AxiomDecl tycon kappa l
  where
    l = fromLoc $ getLoc tycon

letD :: Rec Binding -> Decl
letD bs = LetDecl bs l
  where
    l = fromLoc (getLoc bs)

sigD :: Var -> Type -> Decl
sigD v tau = SigDecl v tau l
  where
    l = fromLoc $ getLoc v <--> getLoc tau

conD :: Con -> Type -> [Var] -> ConDecl
conD con tau vs = ConDecl con tau vs l
  where
    l = case vs of
          [] ->  fromLoc $ getLoc con <--> getLoc tau
          _ ->   fromLoc $ getLoc con <--> getLoc vs

bindD :: Var -> Type -> Exp -> Binding
bindD v tau e = Binding v tau defaultBindInfo e l
  where
    l = fromLoc $ getLoc v <--> getLoc e
\end{code}

\begin{code}
intE :: Integer -> Exp
intE i = LitExp (IntegerLit i) internalLoc

floatE :: Double -> Exp
floatE f = LitExp (FloatLit f) internalLoc

charE :: Char -> Exp
charE c = LitExp (CharLit c) internalLoc

stringE :: String -> Exp
stringE s = LitExp (StringLit s) internalLoc

conE :: Con -> Exp
conE con = ConExp con (fromLoc (getLoc con))

varE :: Var -> Exp
varE v = VarExp v (fromLoc (getLoc v))

lamE :: Var -> Type -> Exp -> Exp
lamE v ty e = LamExp v ty e l
  where
    l = fromLoc $ getLoc v <--> getLoc e

lamsE :: [(Var, Type)] -> Exp -> Exp
lamsE vtys e0 = foldr (\(v, ty) e -> lamE v ty e) e0 vtys

appE :: Exp -> Exp -> Exp
appE e1 e2 = AppExp e1 e2 l
  where
    l = fromLoc $ getLoc e1 <--> getLoc e2

appsE :: Exp -> [Exp] -> Exp
appsE f es = foldl' appE f es

tylamE :: TyVar -> Kind -> Exp -> Exp
tylamE tv k e = TyLamExp tv k e l
  where
    l = fromLoc $ getLoc tv <--> getLoc e

tylamsE :: [(TyVar, Kind)] -> Exp -> Exp
tylamsE tvks e0 = foldr (\(tv, k) e -> tylamE tv k e) e0 tvks

tyappE :: Exp -> Type -> Exp
tyappE e tau = TyAppExp e tau l
  where
    l = fromLoc $ getLoc e <--> getLoc tau

tyappsE :: Exp -> [Type] -> Exp
tyappsE e taus = foldl' tyappE e taus

letE :: Rec Binding -> Exp -> Exp
letE bg e = LetExp bg e l
  where
    l = fromLoc $ getLoc bg <--> getLoc e

letsE :: [(Var, Type, Exp)] -> Exp -> Exp
letsE bs e = foldr f e bs
  where
    f :: (Var, Type, Exp) -> Exp -> Exp
    f (v, ty, e) ebody = letE (NonRec b) ebody
      where
        l = fromLoc $ getLoc v <--> getLoc e
        b = Binding v ty defaultBindInfo e l

caseE :: Exp -> Var -> [Alt] -> Exp
caseE e v alts = CaseExp e v alts l
  where
    l = fromLoc $ getLoc e <--> getLoc v

castE :: Exp -> Type -> Exp
castE e tau = CastExp e tau l
  where
    l = fromLoc $ getLoc e <--> getLoc tau
\end{code}

\begin{code}
litP :: Lit -> Pat
litP lit = LitPat lit internalLoc

varP :: Var -> Type -> Pat
varP v tau = VarPat (TameVar v, tau) l
  where
    l = fromLoc $ getLoc v <--> getLoc tau

wildP :: Type -> Pat
wildP tau = VarPat (WildVar, tau) l
  where
    l = fromLoc (getLoc tau)

conP :: Con -> [(WildTyVar, Kind)] -> [(WildVar, Type)] -> Pat
conP con wtvs wvs = ConPat con wtvs wvs l
  where
    l = fromLoc (getLoc con)
\end{code}
