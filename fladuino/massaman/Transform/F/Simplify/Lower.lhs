%format l = "\ell"

%if False
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}

-- Copyright (c) 2008
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
-- Module      :  Transform.F.Simplify.Lower
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Transform.F.Simplify.Lower where

import Control.Monad (ap)
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Check.F
import Control.Monad.Exception
import Data.Loc
import Data.Name
import Language.F
import Text.PrettyPrint.Mainland
import Transform.F.Simplify.Metrics
\end{code}
%endif

\section{Lowering F}

There are two transformations performed by the lowering phase:

\begin{itemize}

\item Establishing the trival-constructor-argument invariant. This means that
every |let|-bound constructor has only arguments that can be duplicated with no
cost; \see{jones99secrets}.

\item Establish the invariant that all constructors are fully applied. This is
accomplished by eta-expanding any constructors that are not fully applied.
\end{itemize}

The lowering phase requires type information because it needs to generate new
bindings (which must be annoted with their types) and it must know the arity of
constructors, so this module also serves as an example of how to use the F type
checking monad to maintain type information while performing type-based
source-to-source transformations.

\begin{code}
lowerDecls :: MonadTc m => [Decl] -> m [Decl]
lowerDecls decls = do
    mapM_ checkTyConDecl decls
    mapM_ checkTypeDecl decls
    mapM_ checkSigDecl decls
    lowerDecl decls
\end{code}

\begin{code}
lowerDecl :: MonadTc m => [Decl] -> m [Decl]
lowerDecl  [] = return []

lowerDecl  (d@(DataDecl _ _ _ _) : ds)  = do
    ds' <- lowerDecl ds
    return $ d : ds'

lowerDecl  (d@(TypeDecl _ _ _ _) : ds)  = do
    ds' <- lowerDecl ds
    return $ d : ds'

lowerDecl  (d@(AxiomDecl _ _ _) : ds)  = do
    ds' <- lowerDecl ds
    return $ d : ds'

lowerDecl  (d@(SigDecl _ _ _) : ds)  = do
    ds' <- lowerDecl ds
    return $ d : ds'

lowerDecl  ((LetDecl bs _) : ds) = do
    (bs', ds') <- lowerBindings bs (lowerDecl ds)
    return $ letD bs' : ds'
\end{code}

\begin{code}
lowerBinding :: MonadTc m => Binding -> m Binding
lowerBinding (Binding v tau occinfo e l) = do
    e' <- lowerExp True e
    return $ Binding v tau occinfo e' l

lowerBindings :: MonadTc m => Rec Binding -> m a -> m (Rec Binding, a)
lowerBindings (NonRec b@(Binding v tau _ _ _)) m = do
    b'  <- lowerBinding b
    a   <- extendVars [(v, tau)] m
    return (NonRec b', a)

lowerBindings (Rec bs) m =
    extendVars (map boundVar bs) $ do
    bs' <- mapM lowerBinding bs
    a   <- m
    return (Rec bs', a)
\end{code}

|lowerExp| takes a flag indicating whether or not the expression occurs in a
binding context (is it being bound to a variable) and an expression to lower and
returns the lowered expression.

\begin{code}
lowerExp  ::  forall m . MonadTc m
          =>  Bool
          ->  Exp
          ->  m Exp
lowerExp bound e = lower f'
  where
    f :: Exp
    es :: [Exp]
    f : es = unfoldAppExp e

    f' :: Exp
    taus :: [Type]
    (f', taus) = unfoldTyAppExp f

    lowerConArgs :: Con -> m ([(Var, Type, Exp)], [Exp])
    lowerConArgs con
        | bound      = trivializeConExp con es
        | otherwise  = do  es' <- mapM (lowerExp False) es
                           return ([], es')

    lower :: Exp -> m Exp
    lower e@(LitExp _ _) = return $ appsE (tyappsE e taus) es

    lower (ConExp con _) = do
        (binds, es')         <- lowerConArgs con
        (binds_eta, es_eta)  <- etaExpandConExp con taus es'
        return $
            lamsE binds_eta $
            letsE binds $
            appsE (tyappsE (conE con) taus) (es' ++ es_eta)

    lower e@(VarExp _ _) = return $ appsE (tyappsE e taus) es

    lower (LamExp v tau e l) =
        extendVars [(v, tau)] $ do
        e' <- lowerExp False e
        return $ appsE (tyappsE (LamExp v tau e' l) taus) es

    lower (AppExp _ _ _) = panic $ text "lower: saw AppExp"

    lower (TyLamExp a kappa e l) =
        extendTyVars [(a, kappa)] $ do
        e' <- lowerExp bound e
        return $ appsE (tyappsE (TyLamExp a kappa e' l) taus) es

    lower (TyAppExp _ _ _) = panic $ text "lower: saw TyAppExp"

    lower (LetExp bs e _) = do
        (bs', e') <- lowerBindings bs (lowerExp False e)
        return $ appsE (tyappsE (letE bs' e') taus) es

    lower (CaseExp e v alts l) = do
        tau    <- tcExp e
        e'     <- lowerExp False e
        alts'  <- extendVars [(v, tau)] (mapM lowerAlt alts)
        return $ appsE (tyappsE (CaseExp e' v alts' l) taus) es

    lower (CastExp e tau l) = do
        e' <- lowerExp False e
        return $ appsE (tyappsE (CastExp e' tau l) taus) es
\end{code}

\begin{code}
trivialize  ::  MonadTc m
            =>  [Var]
            ->  [Exp]
            ->  m ([(Var, Type, Exp)], [Exp])
trivialize vs es = go vs es ([], [])
  where
    go  ::  MonadTc m
        =>  [Var]
        ->  [Exp]
        ->  ([(Var, Type, Exp)], [Exp])
        ->  m ([(Var, Type, Exp)], [Exp])
    go  _   []        (binds, args) =
        return (reverse binds, reverse args)

    go  vs  (e : es)  (binds, args)
        | isTrivial e  =
            go vs es (binds, e : args)
        | otherwise    = do
            tau  <- tcExp e
            e'   <- lowerExp False e
            go vs' es ((v, tau, e') : binds, varE v : args)
      where
        v    = head vs
        vs'  = tail vs
\end{code}

\begin{code}
trivializeConExp  ::  MonadTc m
                  =>  Con
                  ->  [Exp]
                  ->  m ([(Var, Type, Exp)], [Exp])
trivializeConExp con es = do
    vs <- fresh v
    trivialize vs es
  where
    v :: Var
    v = varAt "v" (fromLoc (getLoc con))
\end{code}

\begin{code}
lowerConArgs  ::  MonadTc m
              =>  Bool
              ->  Con
              ->  [Exp]
              ->  m ([(Var, Type, Exp)], [Exp])
lowerConArgs  True   con  es  = trivializeConExp con es
lowerConArgs  False  _    es  = do
    es' <- mapM (lowerExp False) es
    return ([], es')
\end{code}

\begin{code}
etaExpandConExp  ::  MonadTc m
                 =>  Con
                 ->  [Type]
                 ->  [Exp]
                 ->  m ([(Var, Type)], [Exp])
etaExpandConExp con taus es = do
    xs                      <-  fresh x
    (abinds, _, sigmas, _)  <-  lookupCon con >>= unpackConTy con
    let theta               =   Map.fromList $ map fst abinds `zip` taus
    phi                     <-  return (Set.union (free theta)) `ap` inscopeTyVars
    let sigmas'             =   subst theta phi sigmas
    let taus_eta            =   drop (length es) sigmas'
    let xs_eta              =   take (length taus_eta) xs
    let es_eta              =   map varE xs_eta
    return ((xs_eta `zip` taus_eta), es_eta)
  where
    x :: Var
    x = varAt "x" (fromLoc (getLoc con))
\end{code}

\begin{code}
lowerAlt  ::  forall m . MonadTc m
          =>  Alt
          ->  m Alt
lowerAlt (Alt p e) = go p e
  where
    go ::  forall m . MonadTc m
       =>  Pat
       ->  Exp
       ->  m Alt
    go p@(LitPat _ _) e = do
        e' <- lowerExp False e
        return $ Alt p e'

    go p@(VarPat (wv, tau_p) _) e =
        extendWildVars [(wv, tau_p)] $ do
        e' <- lowerExp False e
        return $ Alt p e'

    go p@(ConPat _ ptyvs pvs _) e =
        extendWildTyVars ptyvs $
        extendWildVars pvs $ do
        e' <- lowerExp False e
        return $ Alt p e'
\end{code}
