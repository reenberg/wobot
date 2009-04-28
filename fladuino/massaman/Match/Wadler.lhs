%if False
\begin{code}
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
-- Module      :  Match.Wadler
-- Copyright   :  (c) Harvard University 2007-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Match.Wadler where

import Control.Monad (foldM,
                      replicateM)
import Control.Monad.Trace
import Maybe (catMaybes)

import Check.Hs.Monad
import Control.Monad.Exception
import Data.Loc
import Data.Name
import qualified Language.F as F
import Match
import Text.PrettyPrint.Mainland
\end{code}
%endif

\section{Match Compilation}

The code here is based on Phil Wadler's description of pattern-matching
compilation in {\it The Implementation of Functional Programming
Languages}~\cite{wadler87matching}. \invariant{everything in {\tt Equation} has
been undecorated}.

\begin{code}
compileMatch :: MonadTc r m => [F.Var] -> [Equation r m] -> F.Exp -> m F.Exp
\end{code}

We want to perform a right fold over the continuations. Since @foldM@ is a left
fold, we have to reverse the list over which we are folding.

\begin{code}
compileMatch [] qs def =
    do  let conts = map getCont qs
        foldM (\e cont -> cont e) def (reverse conts)
  where
    getCont :: Equation r m -> (F.Exp -> m F.Exp)
    getCont  (Equation [] _ cont)  = cont
    getCont  _                     = internalErr $
                                     text "getCont:" <>
                                     text "equation has remaining patterns!"
\end{code}

\begin{code}
compileMatch us qs def
    | all isVar qs  = matchVar  us  qs  def
    | all isLit qs  = matchLit  us  qs  def
    | all isCon qs  = matchCon  us  qs  def
    | otherwise     = matchMix  us  qs  def
\end{code}

\begin{code}
matchVar :: MonadTc r m => [F.Var] -> [Equation r m] -> F.Exp -> m F.Exp
matchVar (u : us) qs def =
    traceNest $
    do  traceMatch $ text "matchVar default: " <+> ppr def
        traceMatch $ text "us:" <+> (spread $ map ppr (u : us))
        traceMatch $ text "qs:" <+> (nest 13 $ stack $ map ppr qs)
        let qs' = map (rewriteq (F.VarExp u internalLoc)) qs
        traceMatch $ text "us':" <+> (spread $ map ppr us)
        traceMatch $ text "qs':" <+> (nest 13 $ stack $ map ppr qs')
        exp <- compileMatch us qs' def
        traceMatch $ text "matchVar match: " <+> ppr def
        return exp
  where
    rewriteq  ::  MonadTc r m
              =>  F.Exp
              ->  Equation r m
              ->  Equation r m
    rewriteq  _     (Equation ((WildNPat, _) : ps) ty cont) =
        Equation ps ty cont

    rewriteq  uexp  (Equation ((AsNPat v _ WildNPat, _) : ps) ty cont) =
        Equation ps ty cont'
      where
        cont' e =
            do  e' <- cont e
                return $ subst1 uexp v e'

    rewriteq _ _ = internalErr $
                   text "rewriteq: called on non-variable pattern"

matchVar [] _ _ = internalErr $
                  text "matchVar: called on empty variable list"
\end{code}

\begin{code}
matchLit :: MonadTc r m => [F.Var] -> [Equation r m] -> F.Exp -> m F.Exp
matchLit (u : us) qs def =
    traceNest $
    do  traceMatch $ text "matchLit default: " <+> ppr def
        alts <- mapM (\q -> matchAlt (u : us) q def) qs
        defty <- compress (headPatTy (head qs)) >>= coerceAst
        let defpat = F.VarPat (F.WildVar, defty) internalLoc
        return $ F.CaseExp (F.VarExp u internalLoc) u (alts ++ [F.Alt defpat def]) internalLoc
  where
    rewriteq  ::  MonadTc r m
              =>  F.Exp
              ->  Equation r m
              ->  (F.Pat, Equation r m)
    rewriteq uexp  (Equation ((AsNPat v _ (LitNPat lit), _) : ps) ty cont) =
        (F.LitPat lit internalLoc, Equation ps ty cont')
      where
        cont' e = do  e' <- cont e
                      return $ subst1 uexp v e'

    rewriteq _     (Equation ((LitNPat lit, _) : ps) ty cont) =
        (F.LitPat lit internalLoc, Equation ps ty cont)

    rewriteq _ _ = internalErr
                   $ text "rewriteq: called on non-literal pattern"

    matchAlt  ::  MonadTc r m
              =>  [F.Var]
              ->  Equation r m
              ->  F.Exp
              ->  m F.Alt
    matchAlt (u : us) q def =
        do  let (pat, q')  = rewriteq (F.VarExp u internalLoc) q
            subexp <- compileMatch us [q'] def
            traceMatch $ text "matchAlt match: " <+> ppr subexp
            return (F.Alt pat subexp)

    matchAlt _ _ _ = internalErr $
                     text "matchAlt: called on empty variable list"

matchLit [] _ _ = internalErr $
                  text "matchLit: called on empty variable list"
\end{code}

\begin{code}
matchCon :: MonadTc r m => [F.Var] -> [Equation r m] -> F.Exp -> m F.Exp
matchCon (u : us) qs def =
    traceNest $
    do  traceMatch $ text "us:" <+> (spread $ map ppr (u : us))
        traceMatch $ text "qs:" <+> (nest 13 $ stack $ map ppr qs)
        let con = getCon (head qs)
        alts <-
            case con of
              (F.TupleCon _) ->
                  do  alt <- matchAlt con (u : us) qs def
                      return [alt]
              con ->
                  do  cons <-  coerceAst con >>= conTyCon
                               >>= constructors >>= mapM coerceAst
                      mapM (\con -> matchAlt con (u : us) (chooseCon qs con) def) cons
        defty <- compress (headPatTy (head qs)) >>= coerceAst
        let defpat = F.VarPat (F.WildVar, defty) internalLoc
        return $ F.CaseExp (F.VarExp u internalLoc) u (catMaybes alts ++ [F.Alt defpat def]) internalLoc
  where
    chooseCon :: [Equation r m] -> F.Con -> [Equation r m]
    chooseCon qs con = filter (\q -> getCon q == con) qs

    rewriteq  ::  MonadTc r m
              =>  F.Exp
              ->  Equation r m
              ->  Equation r m
    rewriteq uexp  (Equation ((AsNPat v _ (ConNPat _ binds), _) : ps) ty cont) =
        Equation (binds ++ ps) ty cont'
      where
        cont' e = do  e' <- cont e
                      return $ subst1 uexp v e'

    rewriteq _     (Equation ((ConNPat _ binds, _) : ps) ty cont) =
        Equation (binds ++ ps) ty cont

    rewriteq _ _ = internalErr $
                   text "rewriteq: called on non-constructor pattern"

    matchAlt  ::  MonadTc r m
              =>  F.Con
              ->  [F.Var]
              ->  [Equation r m]
              ->  F.Exp
              ->  m (Maybe F.Alt)
    matchAlt _    _         []  _    = return Nothing

    matchAlt con  (u : us)  qs  def  =
        do  let tys           = getConTys (head qs)
            us'               <- replicateM (length tys) uniqueVar
            let qs'           = map (rewriteq (F.VarExp u internalLoc)) qs
            let (us'', qs'')  = renameUs us' qs'
            subexp            <- compileMatch (us'' ++ us) qs'' def
            gtys              <- mapM (\ty -> compress ty >>= coerceAst) tys
            let p             = F.ConPat con [] (map F.TameVar us'' `zip` gtys) internalLoc
            return $ Just (F.Alt p subexp)

    matchAlt _ _ _ _ = internalErr $
                       text "matchAlt: called on empty variable list"

matchCon [] _ _ = internalErr $
                  text "matchCon: called on empty variable list"
\end{code}

We want to perform a right fold over the partitioned equations. Since @foldM@ is
a left fold, we have to flip the function we use in the fold, @match us@, and
reverse the list over which we are folding.

\begin{code}
matchMix :: MonadTc r m => [F.Var] -> [Equation r m] -> F.Exp -> m F.Exp
matchMix us qs def =
    traceNest $
    do  traceMatch $ text "us:" <+> (spread $ map ppr us)
        traceMatch $ text "qs:" <+> (nest 9 $ stack $ map ppr qs)
        foldM  (\e q -> compileMatch us q e)
               def (reverse $ partition qs)
  where
    partition :: [Equation r m] -> [[Equation r m]]
    partition  [] = []
    partition  (q : qs)
        | isVar q    = let (vars, rest) = span isVar qs
                       in
                         (q : vars) : partition rest
        | isLit q    = let (lits, rest) = span isLit qs
                       in
                         (q : lits) : partition rest
        | isCon q    = let (cons, rest) = span isCon qs
                       in
                         (q : cons) : partition rest
        | otherwise  = internalErr $
                       text "matchMix: the impossible happened"
\end{code}
