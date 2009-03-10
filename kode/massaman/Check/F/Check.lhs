%if False
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

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
-- Module      :  Check.F.Check
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Check.F.Check (
    checkDecls,
    checkTypeDecl,
    checkTyConDecl,
    checkSigDecl,
    checkBindings,
    tcExp,
    equiv,
    (===),
    (/===),
    unpackConTy,
    conArity
  ) where

import Control.Monad.Error
import Control.Monad.Trace
import Data.Ord (comparing)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Check.F.Builtin
import Check.F.Exceptions
import Check.F.Monad
import Control.Monad.ContextException
import Control.Monad.Exception
import Data.Loc
import Data.Name
import Language.F
import Text.PrettyPrint.Mainland

import Util
\end{code}
%endif

\section{F Type Checking}

\subsection{Type Checking}

\begin{code}
checkDuplicates  ::  (NameBinding b, Ord b, MonadException m)
                 =>  Doc
                 ->  [b]
                 ->  m ()
checkDuplicates desc bs =
    case filter  (\x -> length x /= 1)
                 (equivalence (comparing fst) binds) of
      []    ->  return ()
      dups  ->  throwException $ Duplicates desc dups
  where
    binds =[(bindingName b, bindingLoc b) | b <- bs]
\end{code}

\begin{code}
checkDecls :: MonadTc m => [Decl] -> m ()
checkDecls decls = do
    mapM_ checkTyConDecl decls
    mapM_ checkTypeDecl decls
    mapM_ checkSigDecl decls
    s <- checkBindingDecls decls
    putTcEnv s
  where
    checkBindingDecls :: MonadTc m => [Decl] -> m TcEnv
    checkBindingDecls  [] =
        getTcEnv

    checkBindingDecls  (LetDecl (NonRec b) _ : decls) = do
        checkBinding b
        extendVars [boundVar b] $
            checkBindingDecls decls

    checkBindingDecls  (LetDecl (Rec bs) _ : decls) =
        checkBindings bs $ checkBindingDecls decls

    checkBindingDecls  (_ : decls) =
        checkBindingDecls decls
\end{code}

\begin{code}
checkTyConDecl :: MonadTc m => Decl -> m ()
checkTyConDecl  decl@(DataDecl tycon kappa condecls _)  =
    withLocContext (getLoc decl) (text "declaration:" <+> ppr decl) $ do
    checkKindSort kappa TY
    let (_, kapp_ret) = destructKindFun kappa
    checkKind kapp_ret (:*)
    insertTyCon tycon [con | ConDecl con _ _ _ <- condecls] kappa

checkTyConDecl  _                                =
    return ()
\end{code}

\begin{code}
checkSigDecl :: MonadTc m => Decl -> m ()
checkSigDecl  (SigDecl v ty _)  = insertVar v ty
checkSigDecl  _                 = return ()
\end{code}

\begin{code}
checkTypeDecl :: MonadTc m => Decl -> m ()
checkTypeDecl decl@(DataDecl tycon _ condecls _) =
    withLocContext (getLoc decl) (text "declaration:" <+> ppr decl) $ do
    traceTc $ text "checking declaration:"  <+> ppr decl
    cons <- mapM (checkConDecl tycon) condecls
    mapM_ (\(con, ty, labels) -> insertCon con ty labels) cons
  where
    checkConDecl  ::  MonadTc m
                  =>  TyCon
                  ->  ConDecl
                  ->  m (Con, Type, [Var])
    checkConDecl tycon condecl@(ConDecl con con_ty labels _) =
        withLocContext (getLoc condecl) (text "constructor:" <+> ppr condecl) $ do
        traceTc $ text "checking constructor declaration:"  <+> ppr condecl
        checkTyKind con_ty (:*)
        (_, _, _, tycon') <- unpackConTy con con_ty
        when (tycon' /= tycon) $
            throwException $ IllegalConstructorDeclaration con
        return (con, con_ty, labels)
\end{code}

\begin{code}
checkTypeDecl decl@(TypeDecl tyfun args res _) =
    withLocContext (getLoc decl) (text "declaration:" <+> ppr decl) $ do
    traceTc $ text "checking declaration:" <+> ppr decl
    checkKindSort (foldr (:=>) res args) TY
    insertTyFun tyfun (args, res)
\end{code}

\begin{code}
checkTypeDecl decl@(AxiomDecl axiom k _) =
    withLocContext (getLoc decl) (text "declaration:" <+> ppr decl) $ do
    traceTc $ text "checking declaration:" <+> ppr decl
    checkKindSort k CO
    insertTyCon axiom [] k
\end{code}

\begin{code}
checkTypeDecl  (LetDecl _ _)     = return ()
checkTypeDecl  (SigDecl _ _ _)   = return ()
\end{code}

\begin{code}
checkBindings  ::  MonadTc m
               =>  [Binding]
               ->  m a
               ->  m a
checkBindings bindings m = do
    traceTc $ text "checking bindings" <+> stack (map ppr bindings)
    extendVars (map boundVar bindings) $ do
        mapM_ checkBinding bindings
        m

checkBinding :: MonadTc m => Binding -> m ()
checkBinding (Binding _ ty _ e _) = do
    e_ty <- tcExp e
    ty `equiv` e_ty
\end{code}

\begin{code}
tcLit :: MonadTc m => Lit -> m Type
tcLit (IntegerLit _)    = return integerTy
tcLit (FloatLit _)      = return floatTy
tcLit (CharLit _)       = return charTy
tcLit (StringLit _)     = return stringTy
\end{code}

\begin{code}
tcExp :: MonadTc m => Exp -> m Type

tcExp e@(LitExp lit _) =
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $
    traceNest $ do
    traceTc $ text "checking expression:" <+> ppr e
    tau <- tcLit lit
    traceTc $ text "expression type:" <+> ppr tau
    return tau

tcExp e@(VarExp v _) =
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $
    traceNest $ do
    traceTc $ text "checking expression:" <+> ppr e
    tau <- lookupVar v
    traceTc $ text "expression type:" <+> ppr tau
    return tau

tcExp e@(ConExp con _) =
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $
    traceNest $ do
    traceTc $ text "checking expression:" <+> ppr e
    tau <- lookupCon con
    traceTc $ text "expression type:" <+> ppr tau
    return tau

tcExp e@(LamExp x sigma_x sube _) =
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $
    traceNest $ do
    traceTc $ text "checking expression:" <+> ppr e
    checkTyKind sigma_x (:*)
    sigma <- extendVars [(x, sigma_x)] $ tcExp sube
    let tau = (sigma_x --> sigma)
    traceTc $ text "expression type:" <+> ppr tau
    return tau

tcExp e@(AppExp e1 e2 _) =
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $
    traceNest $ do
    traceTc $ text "checking expression:" <+> ppr e
    ty1 <- tcExp e1
    ty2 <- tcExp e2
    (arg_ty, tau) <- funTy ty1 ty2
    ty2 `equiv` arg_ty
    traceTc $ text "expression type:" <+> ppr tau
    return tau

tcExp e@(TyLamExp a kappa sube sloc@_) =
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $
    traceNest $ do
    traceTc $ text "checking type abstraction expression:" <+> ppr e
    scKind kappa
    sigma <- extendTyVars [(a, kappa)] $ tcExp sube
    let tau = ForAll (TameTyVar a) kappa sigma sloc
    traceTc $ text "type abstraction expression type:" <+> ppr tau
    return tau

tcExp e@(TyAppExp sube phi _) =
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $
    traceNest $ do
    traceTc $ text "checking type application expression:" <+> ppr e
    (a_wild, kappa, sigma) <-  tcExp sube >>= unpackForAll
    kappa_phi <-  scKind kappa >>= \s ->
                  case s of
                    TY  ->  kcTy phi
                    CO  ->  do  (sigma_phi, tau_phi) <- kcCo phi
                                return $ sigma_phi :~ tau_phi
    when (kappa_phi /= kappa) $
        throwException $ KindMatchError kappa_phi kappa
    tau <- case a_wild of
             WildTyVar    -> return sigma
             TameTyVar a  -> do  let theta  =   Map.singleton a phi
                                 phi        <-  return (Set.insert a) `ap` inscopeTyVars
                                 return $ subst theta phi sigma
    traceTc $ text "type application expression type:" <+> ppr tau
    return tau

tcExp e@(LetExp (NonRec b) sube _) =
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $
    traceNest $ do
    traceTc $ text "checking expression:" <+> ppr e
    checkBinding b
    tau <- extendVars [boundVar b] $ tcExp sube
    traceTc $ text "expression type:" <+> ppr tau
    return tau

tcExp e@(LetExp (Rec bs) sube _) =
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $
    traceNest $ do
    traceTc $ text "checking expression:" <+> ppr e
    tau <- checkBindings bs $ tcExp sube
    traceTc $ text "expression type:" <+> ppr tau
    return tau
\end{code}

\begin{code}
tcExp e@(CaseExp sube v alts _) =
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $
    traceNest $ do
    traceTc $ text "checking expression:" <+> ppr sube
    sigma <- tcExp sube
    extendVars [(v, sigma)] $ do
        (sigmas, taus) <- mapAndUnzipM (\(Alt p e') -> tcAlt p e' sigma) alts
        let tau = head taus
        mapM_ (`equiv` sigma) sigmas
        mapM_ (`equiv` tau) taus
        traceTc $ text "expression type:" <+> ppr tau
        return tau
\end{code}

\begin{code}
tcExp e@(CastExp sube gamma _) =
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $
    traceNest $ do
    traceTc $ text "checking expression:" <+> ppr e
    sigma <- tcExp sube
    (gamma_sigma, tau) <- kcCo gamma
    gamma_sigma `equiv` sigma
    return tau
\end{code}

\begin{code}
tcAlt  ::  MonadTc m
       =>  Pat
       ->  Exp
       ->  Type
       ->  m (Type, Type)
tcAlt p@(LitPat lit _) e tau_p =
    withLocContext (getLoc p) (text "branch:" <+> ppr p <+> text "->" <+> ppr e) $
    traceNest $ do
    traceTc $ text "checking branch:" <+> ppr p <+> text "->" <+> ppr e
    tau_p' <- tcLit lit
    tau_p' `equiv` tau_p
    tau_e <- tcExp e
    traceTc $ text "branch type:" <+> ppr tau_p <+> text "->" <+> ppr tau_e
    return (tau_p, tau_e)

tcAlt p@(VarPat (wv, tau_p') _) e tau_p =
    withLocContext (getLoc p) (text "branch:" <+> ppr p <+> text "->" <+> ppr e) $
    traceNest $ do
    traceTc $ text "checking branch:" <+> ppr p <+> text "->" <+> ppr e
    tau_p' `equiv` tau_p
    tau_e <- extendWildVars [(wv, tau_p)] $ tcExp e
    traceTc $ text "branch type:" <+> ppr tau_p <+> text "->" <+> ppr tau_e
    return (tau_p, tau_e)
\end{code}

\begin{code}
tcAlt p@(ConPat con ptyvs pvs _) e tau_p =
    withLocContext (getLoc p) (text "branch:" <+> ppr p <+> text "->" <+> ppr e) $
    traceNest $ do
    traceTc $ text "checking branch:" <+> ppr p <+> text "->" <+> ppr e
    checkDuplicates (text "multiple declarations of pattern variable")
        [tv | (TameTyVar tv, _) <- ptyvs]
    checkDuplicates (text "multiple declarations of pattern variable")
        [v | (TameVar v, _) <- pvs]
    let (tycon_p : tyconargs_p) = unfoldAppTy tau_p
    con_ty <- lookupCon con
    (abinds, bbinds, sigmas, tycon) <- unpackConTy con con_ty
    when (length ptyvs /= length bbinds) $
        throwException $ UnsaturatedConstructor con
    when (length pvs /= length sigmas) $
        throwException $ UnsaturatedConstructor con
    mapM_ (uncurry checkKind) (map snd ptyvs `zip` map snd bbinds)
    TyConTy tycon internalLoc `equiv` tycon_p
    let theta    =   Map.fromList $ map fst abinds `zip` tyconargs_p
    phi          <-  return (Set.union (free theta)) `ap` inscopeTyVars
    let sigmas'  =   subst theta phi sigmas
    mapM_ (uncurry equiv) (map snd pvs `zip` sigmas')
    tau_e <-  extendWildTyVars ptyvs $
              extendWildVars pvs $
              tcExp e
    traceTc $ text "branch type:" <+> ppr tau_p <+> text "->" <+> ppr tau_e
    return (tau_p, tau_e)
\end{code}

\begin{code}
funTy :: MonadTc m => Type -> Type -> m (Type, Type)
funTy (AppTy (AppTy (TyConTy (TyCon tycon) _) ty1 _) ty2 _) _
    | tycon == builtinArrow = return (ty1, ty2)
funTy  ty arg_ty = throwException $ TypeMatchError ty wanted_ty
  where
    wanted_ty = arg_ty --> TyVarTy (TyVar (mkName "a")) internalLoc
\end{code}

\subsubsection{Sort Checking}

\begin{code}
scKind :: MonadTc m => Kind -> m Sort
scKind (:*) = return TY

scKind (k1 :=> k2) = do
    checkKindSort k1 TY
    checkKindSort k2 TY
    return TY

scKind (ty1 :~ ty2) = do
    k1 <- kcTy ty1
    k2 <- kcTy ty2
    when (k1 /= k2) $
        throwException $ KindMatchError k1 k2
    return CO
\end{code}

\subsubsection{Kind Checking Types}

\begin{code}
kcTy :: MonadTc m => Type -> m Kind
kcTy ty@(TyConTy tycon _) =
    withLocContext (getLoc ty) (text "type:" <+> ppr ty) $ do
    k <- lookupTyCon tycon
    checkKindSort k TY
    return k

kcTy ty@(TyVarTy tyvar _) =
    withLocContext (getLoc ty) (text "type:" <+> ppr ty) $ do
    k <- lookupTyVar tyvar
    checkKindSort k TY
    return k

kcTy ty@(AppTy ty1 ty2 _) =
    withLocContext (getLoc ty) (text "type:" <+> ppr ty) $ do
    k1 <- kcTy ty1
    k2 <- kcTy ty2
    (arg_k, ret_k) <- funK k1 k2
    checkKind k2 arg_k
    return ret_k
  where
    funK :: MonadTc m => Kind -> Kind -> m (Kind, Kind)
    funK  (arg_k :=> ret_k)  _      =
        return (arg_k, ret_k)

    funK  k                  arg_k  = do
      let wanted_k = arg_k :=> (:*)
      throwException $ KindMatchError k wanted_k

kcTy ty@(ForAll tv k qty _) =
    withLocContext (getLoc ty) (text "type:" <+> ppr ty) $ do
    k <-  extendWildTyVars [(tv, k)] $
          checkTyKind qty (:*)
    scKind k
    return (:*)

kcTy ty = throwException $ NotSortTY ty
\end{code}

\subsubsection{Kind Checking Coercions}

\begin{code}
kcCo :: MonadTc m => Type -> m (Type, Type)
kcCo ty@(TyConTy tycon _) =
    withLocContext (getLoc ty) (text "type:" <+> ppr ty) $ do
    k <- lookupTyCon tycon
    s <- scKind k
    case s of
      TY  -> return $ (ty, ty)
      CO  -> return $ destructCo k

kcCo ty@(TyVarTy tyvar _) =
    withLocContext (getLoc ty) (text "type:" <+> ppr ty) $ do
    k <- lookupTyVar tyvar
    checkKindSort k CO
    return $ destructCo k

kcCo ty@(AppTy gamma_1 gamma_2 sloc) =
    withLocContext (getLoc ty) (text "type:" <+> ppr ty) $ do
    (sigma_1, tau_1) <- kcCo gamma_1
    (sigma_2, tau_2) <- kcCo gamma_2
    kcTy (AppTy sigma_1 sigma_2 sloc)
    return $ (AppTy sigma_1 sigma_2 sloc, AppTy tau_1 tau_2 sloc)

kcCo ty@(AppTyFunTy sn gammas sloc) =
    withLocContext (getLoc ty) (text "type:" <+> ppr ty) $ do
    (sigmas, taus) <- mapAndUnzipM kcCo gammas
    kcTy (AppTyFunTy sn sigmas sloc)
    return $ (AppTyFunTy sn sigmas sloc, AppTyFunTy sn taus sloc)

kcCo ty@(ForAll wtv kappa phi sloc) =
    withLocContext (getLoc ty) (text "type:" <+> ppr ty) $ do
    s <- scKind kappa
    if s == TY
      then kcForAllTY wtv kappa phi
      else kcForAllCO wtv kappa phi
  where
    kcForAllTY :: MonadTc m => WildTyVar -> Kind -> Type -> m (Type, Type)
    kcForAllTY wtv kappa gamma = do
        checkKindSort kappa TY
        (sigma, tau) <-  extendWildTyVars [(wtv, kappa)] $
                         kcCo gamma
        return (ForAll wtv kappa sigma sloc, ForAll wtv kappa tau sloc)

    kcForAllCO :: MonadTc m => WildTyVar -> Kind -> Type -> m (Type, Type)
    kcForAllCO wtv kappa phi = do
        ((gamma_1, gamma_2), gamma) <- unpackConstraint (ForAll wtv kappa phi sloc)
        (tau_1, upsilon_1) <- kcCo gamma_1
        (tau_2, upsilon_2) <- kcCo gamma_2
        checkKindSort (tau_1 :~ tau_2) CO
        (sigma_1, sigma_2) <- kcCo gamma
        return $ (  ForAll WildTyVar (tau_1      :~  tau_2)      sigma_1 sloc,
                    ForAll WildTyVar (upsilon_1  :~  upsilon_2)  sigma_2 sloc)

kcCo ty@(SymCo gamma _) =
    withLocContext (getLoc ty) (text "type:" <+> ppr ty) $ do
    (sigma, tau) <- kcCo gamma
    return (tau, sigma)

kcCo ty@(TransCo gamma_1 gamma_2 _) =
    withLocContext (getLoc ty) (text "type:" <+> ppr ty) $ do
    (sigma_1,   sigma_2) <- kcCo gamma_1
    (sigma_2',  sigma_3) <- kcCo gamma_2
    sigma_2 `equiv` sigma_2'
    return (sigma_1, sigma_3)

kcCo ty@(AppCo gamma_1 phi  sloc) =
    withLocContext (getLoc ty) (text "type:" <+> ppr ty) $ do
    isTY <- (kcTy phi >> return True) `catchError` (\_ -> return False)
    if isTY
      then kcAppCoTY gamma_1 phi
      else kcAppCoCO gamma_1 phi
  where
    kcAppCoTY :: MonadTc m => Type -> Type -> m (Type, Type)
    kcAppCoTY gamma upsilon = do
        k <- kcTy upsilon
        checkKindSort k TY
        (tau_a, tau_b) <- kcCo gamma
        (a, kappa_a, sigma)  <- unpackForAll tau_a
        (b, kappa_b, tau)    <- unpackForAll tau_b
        when (kappa_a /= k) $
            throwException $ TypeMatchError tau_a (ForAll a k sigma sloc)
        when (kappa_b /= k) $
            throwException $ TypeMatchError tau_a (ForAll b k tau sloc)
        phi <- inscopeTyVars
        sigma'  <- case a of
                     WildTyVar        -> return sigma
                     TameTyVar alpha  -> do  let theta  = Map.singleton alpha upsilon
                                             let phi'   = Set.insert alpha phi
                                             return $ subst theta phi' sigma
        tau'    <- case b of
                     WildTyVar        -> return sigma
                     TameTyVar alpha  -> do  let theta  = Map.singleton alpha upsilon
                                             let phi'   = Set.insert alpha phi
                                             return $ subst theta phi' tau
        return (sigma', tau')

    kcAppCoCO :: MonadTc m => Type -> Type -> m (Type, Type)
    kcAppCoCO gamma_1 gamma_2 = do
        (tau_a, tau_b) <- kcCo gamma_1
        (_, sigma_1) <- unpackConstraint tau_a
        (co_b, sigma_2) <- unpackConstraint tau_b
        let (upsilon_1, upsilon_2) = co_b
        (rho_a, rho_b) <- kcCo gamma_2
        rho_a `equiv` upsilon_1
        rho_b `equiv` upsilon_2
        return (sigma_1, sigma_2)

kcCo ty@(LeftCo gamma _) =
    withLocContext (getLoc ty) (text "type:" <+> ppr ty) $ do
    (sigma, tau)   <- kcCo gamma
    (sigma_1,  _)  <- unpackAppTy sigma
    (tau_1,    _)  <- unpackAppTy tau
    return (sigma_1, tau_1)

kcCo ty@(RightCo gamma _) =
    withLocContext (getLoc ty) (text "type:" <+> ppr ty) $ do
    (sigma, tau)   <- kcCo gamma
    (_,  sigma_2)  <- unpackAppTy sigma
    (_,  tau_2)    <- unpackAppTy tau
    return (sigma_2, tau_2)
\end{code}

\begin{code}
checkSort :: MonadTc m => Sort -> Sort -> m ()
checkSort s1 s2 =
    when (s1 /= s2) $
        throwException $ SortMatchError s1 s2
\end{code}

\begin{code}
checkKindSort :: MonadTc m => Kind -> Sort -> m ()
checkKindSort k s = do
    k_s <- scKind k
    checkSort k_s s
\end{code}

\begin{code}
checkKind :: MonadTc m => Kind -> Kind -> m Kind
checkKind k1 k2 = do
    when (k1 /= k2) $
        throwException $ KindMatchError k1 k2
    return k1
\end{code}

\begin{code}
checkTyKind :: MonadTc m => Type -> Kind -> m Kind
checkTyKind ty k = do
    ty_k <- kcTy ty
    checkKind ty_k k
\end{code}

\begin{code}
unpackAppTy :: MonadTc m => Type -> m (Type, Type)
unpackAppTy (AppTy ty1 ty2 _)  = return (ty1, ty2)
unpackAppTy ty                 = throwException $ NotAppTy ty
\end{code}

\begin{code}
unpackForAll :: MonadTc m => Type -> m (WildTyVar, Kind, Type)
unpackForAll (ForAll wtv k ty _)  = return (wtv, k, ty)
unpackForAll ty                   = throwException $ NotForAll ty
\end{code}

\begin{code}
unpackConstraint :: MonadTc m => Type -> m ((Type, Type), Type)
unpackConstraint ty@(ForAll wtv co sigma sloc) = do
    case wtv of
      WildTyVar     ->
          return ()
      TameTyVar tv  ->
          when (tv `Set.member` free sigma) $
              throwException $ TypeMatchError ty (ForAll WildTyVar co sigma sloc)
    checkKindSort co CO
    return (destructCo co, sigma)

unpackConstraint ty =
    throwException $ NotForAll ty
\end{code}

Constructors are of the form:

\begin{equation*}
K : \forall \bar{a : \kappa} . \forall \bar{b : \iota}.\bar{sigma} -> T \bar{a}
\end{equation*}

Note that $\bar{a}$ must appear first among the $\forall$-bounds variables in
the constructor's type, and again in the same order as the type constructor's
argument. $\bar{b}$ represents the existentially bound variables. Tearing apart
constructor types is a pain, and we do it in more than one place, so we make
|unpackConTy| do all the work. The steps are as follows:

\begin{itemize}
\item Check that the constructor's type has kind $*$.

\item Unfold the $\forall$ bindings.

\item Rip apart the qualified type into as list of the types of its arguments,
$\bar{sigma}$, and the result type, $\tau$.

\item Rip apart the result type, $\tau$, which {\it must} be a constructor type,
into its type constructor and the arguments to the type constructor. These
arguments must by type variables, so we extract them as such.

\item Look up the type constructor's kind, and rip it apart into the argument
kinds and the resulting kind, checking that the result kind is $*$.

\item

\end{itemize}

\begin{code}
unpackConTy  ::  MonadTc m
             =>  Con
             ->  Type
             ->  m ([(TyVar, Kind)], [(WildTyVar, Kind)], [Type], TyCon)
unpackConTy con con_ty = do
    let (binds, tau_0)    =   unfoldForAll con_ty
    let (sigmas, tau)     =   destructFunTy tau_0
    (tycon, tau_a's)      <-  unpackTyCon tau
    a's                   <-  mapM fromTyVarTy tau_a's
    kappa_tycon           <-  lookupTyCon tycon
    let (kappa_args, _)   =   destructKindFun kappa_tycon
    let n                 =   length kappa_args
    let (abinds, bbinds)  =   splitAt n binds
    as                    <-  mapM (\(wtv, _) -> fromTameTyVar wtv) abinds
    let kappa_as          =   map snd abinds
    mapM (uncurry checkKind) (kappa_as `zip` kappa_args)
    when (as /= a's) $
        throwException $ IllegalConstructorDeclaration con
    return (as `zip` kappa_as, bbinds, sigmas, tycon)
  where
    fromTameTyVar :: MonadTc m => WildTyVar -> m TyVar
    fromTameTyVar WildTyVar       =  throwException $
                                     IllegalConstructorDeclaration con
    fromTameTyVar (TameTyVar tv)  =  return tv

    fromTyVarTy :: MonadTc m => Type -> m TyVar
    fromTyVarTy (TyVarTy tv _)  =  return tv
    fromTyVarTy _               =  throwException $
                                   IllegalConstructorDeclaration con

    unpackTyCon :: MonadTc m => Type -> m (TyCon, [Type])
    unpackTyCon ty =
        case unfoldAppTy ty of
          TyConTy tycon _ : args ->
              return (tycon, args)
          _ -> throwException $ IllegalConstructorDeclaration con
\end{code}

\begin{code}
conArity :: MonadTc m => Con -> m Int
conArity con = do
    con_ty <- lookupCon con
    (_, _, sigmas, _) <- unpackConTy con con_ty
    return $ length sigmas
\end{code}

\subsubsection{Deciding Type Equivalence}

\begin{code}
equiv :: MonadTc m => Type -> Type -> m ()
equiv ty1 ty2 = do
    phi <- inscopeTyVars
    equiv' phi ty1 ty2
  where
    equiv' :: MonadTc m => Set.Set TyVar -> Type -> Type -> m ()
    equiv' _ (TyConTy tycon1 _) (TyConTy tycon2 _)
        | tycon1 == tycon2 = return ()

    equiv' _ ty1@(TyVarTy tv1 _) ty2@(TyVarTy tv2 _)
        | tv1 == tv2  = return ()
        | otherwise   = throwException $ TypeMatchError ty1 ty2

    equiv' phi (AppTy ty1a ty2a _) (AppTy ty1b ty2b _) = do
        equiv' phi ty1a ty1b
        equiv' phi ty2a ty2b

    equiv' phi (AppTyFunTy f1 tys1 _) (AppTyFunTy f2 tys2 _)
        | f1 == f2 && length tys1 == length tys2 =
            mapM_ (\(ty1, ty2) -> equiv' phi ty1 ty2) (tys1 `zip` tys2)

    equiv' phi (ForAll WildTyVar k1 ty1 _) (ForAll WildTyVar k2 ty2 _) = do
        checkKind k1 k2
        equiv' phi ty1 ty2

    equiv' phi (ForAll (TameTyVar tv1) k1 ty1 _) (ForAll (TameTyVar tv2) k2 ty2 _) = do
        checkKind k1 k2
        let phi' = Set.insert tv1 phi
        equiv' phi' ty1 (subst (Map.singleton tv2 (TyVarTy tv1 internalLoc)) phi' ty2)

    equiv' phi (SymCo ty1 _) (SymCo ty2 _) =
        equiv' phi ty1 ty2

    equiv' phi (TransCo ty1a ty2a _) (TransCo ty1b ty2b _) = do
        equiv' phi ty1a ty1b
        equiv' phi ty2a ty2b

    equiv' phi (AppCo ty1a ty2a _) (AppCo ty1b ty2b _) = do
        equiv' phi ty1a ty1b
        equiv' phi ty2a ty2b

    equiv' phi (LeftCo ty1 _) (LeftCo ty2 _) =
        equiv' phi ty1 ty2

    equiv' phi (RightCo ty1 _) (RightCo ty2 _) =
        equiv' phi ty1 ty2

    equiv' _ ty1 ty2 = throwException $ TypeMatchError ty1 ty2
\end{code}

\begin{code}
(===) :: Type -> Type -> Bool
ty1 === ty2 =
    equiv' phi ty1 ty2
  where
    phi = free ty1 `Set.union` free ty2

    equiv' :: Set.Set TyVar -> Type -> Type -> Bool
    equiv' _ (TyConTy tycon1 _) (TyConTy tycon2 _) =
        tycon1 == tycon2

    equiv' _ (TyVarTy tv1 _) (TyVarTy tv2 _) =
        tv1 == tv2

    equiv' phi (AppTy ty1a ty2a _) (AppTy ty1b ty2b _) =
        equiv' phi ty1a ty1b && equiv' phi ty2a ty2b

    equiv' phi (AppTyFunTy f1 tys1 _) (AppTyFunTy f2 tys2 _) =
        f1 == f2 &&
        length tys1 == length tys2 &&
        all (\(ty1, ty2) -> equiv' phi ty1 ty2) (tys1 `zip` tys2)

    equiv' phi (ForAll WildTyVar k1 ty1 _) (ForAll WildTyVar k2 ty2 _) =
        k1 == k2 && equiv' phi ty1 ty2

    equiv' phi (ForAll (TameTyVar tv1) k1 ty1 _) (ForAll (TameTyVar tv2) k2 ty2 _) =
        k1 == k2 && equiv' phi' ty1 (subst (Map.singleton tv2 (TyVarTy tv1 internalLoc)) phi' ty2)
      where
        phi' = Set.insert tv1 phi

    equiv' phi (SymCo ty1 _) (SymCo ty2 _) =
        equiv' phi ty1 ty2

    equiv' phi (TransCo ty1a ty2a _) (TransCo ty1b ty2b _) =
        equiv' phi ty1a ty1b && equiv' phi ty2a ty2b

    equiv' phi (AppCo ty1a ty2a _) (AppCo ty1b ty2b _) =
        equiv' phi ty1a ty1b && equiv' phi ty2a ty2b

    equiv' phi (LeftCo ty1 _) (LeftCo ty2 _) =
        equiv' phi ty1 ty2

    equiv' phi (RightCo ty1 _) (RightCo ty2 _) =
        equiv' phi ty1 ty2

    equiv' _ _ _ = False

(/===) :: Type -> Type -> Bool
ty1 /=== ty2 = not (ty1 === ty2)
\end{code}

%if False
\end{document}
%endif
