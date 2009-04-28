%if False
\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
-- Module      :  Language.Hs.Smart
-- Copyright   :  (c) Harvard University 2007-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.Hs.Smart (
    tyVar,
    tyVarAt,
    tyCon,
    tyConAt,
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
    forallT,
    varD,
    patD,
    sigD,
    rhsD,
    intE,
    floatE,
    charE,
    stringE,
    conE,
    varE,
    lamE,
    appE,
    appsE,
    opappE,
    negappE,
    ifE,
    letE,
    sigE,
    undefinedE,
    errorE,
    wildP,
    varP
  ) where

import Data.List (foldl')

import Data.Loc
import Data.Name
import Language.Hs.Syntax
import Text.PrettyPrint.Mainland
\end{code}
%endif

\subsection{Smart constructors}

\begin{code}
tyVar :: String -> TyVar
tyVar = TyVar . mkName

tyVarAt :: String -> Loc -> TyVar
tyVarAt v l = TyVar $ mkNameAt v l

tyCon :: String -> TyCon
tyCon = TyCon . mkName

tyConAt :: String -> Loc -> TyCon
tyConAt v l = TyCon $ mkNameAt v l

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
varD :: Var -> [Pat] -> Rhs -> Decl
varD v ps rhs = VarBindDecl v ps rhs l
  where
    l = fromLoc $ getLoc v <--> getLoc rhs

patD :: Pat -> Rhs -> Decl
patD p rhs = PatBindDecl p rhs l
  where
    l = fromLoc $ getLoc p <--> getLoc rhs

sigD :: [Var] -> Type -> Decl
sigD vs tau = SigDecl vs tau l
  where
    l = fromLoc (getLoc vs)

rhsD :: [Exp] -> Rhs
rhsD es = Rhs [(Nothing, e) | e <- es] []
\end{code}

\begin{code}
tyVarT :: TyVar -> Type
tyVarT tyvar = TyVarTy tyvar

tyConT :: TyCon -> Type
tyConT tycon = TyConTy tycon

appT :: Type -> Type -> Type
appT tau1 tau2 = AppTy tau1 tau2

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

forallT :: ExplicitForAll -> [TyVar] -> Context -> Type -> Type
forallT ex tvs ctx tau = ForAll ex tvs ctx tau
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

lamE :: Pat -> Exp -> Exp
lamE p e = LamExp p e l
  where
    l = fromLoc $ getLoc p <--> getLoc e

appE :: Exp -> Exp -> Exp
appE e1 e2 = AppExp e1 e2 l
  where
    l = fromLoc $ getLoc e1 <--> getLoc e2

opappE :: Exp -> Exp -> OpFixity -> Exp -> Exp
opappE e1 op fixity e2 = OpAppExp e1 op fixity e2 l
  where
    l = fromLoc $ getLoc e1 <--> getLoc e2

negappE :: Exp -> Exp
negappE e = NegAppExp e l
  where
    l = fromLoc (getLoc e)

appsE :: Exp -> [Exp] -> Exp
appsE f es = foldl' appE f es

letE :: [Decl] -> Exp -> Exp
letE decls e = LetExp decls e l
  where
    l = fromLoc $ getLoc decls <--> getLoc e

sigE :: Exp -> Type -> Exp
sigE e tau = SigExp e tau l
  where
    l = fromLoc (getLoc e)

ifE :: Exp -> Exp -> Exp -> Exp
ifE teste thene elsee = IfExp teste thene elsee loc
  where
    loc = fromLoc $ getLoc teste <--> getLoc elsee

undefinedE :: Exp
undefinedE = varE undefinedV
  where
    undefinedV = Var prelUndefined

errorE :: String -> Exp
errorE s = appE (varE errorV) (stringE s)
  where
    errorV = Var prelError
\end{code}

\begin{code}
wildP :: Pat
wildP = WildPat internalLoc

varP :: Var -> Pat
varP v = AsPat v (WildPat l) l
  where
    l = fromLoc (getLoc v)
\end{code}
