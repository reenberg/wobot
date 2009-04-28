%if False
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- Module      :  Transform.Hs.Desugar
-- Copyright   :  (c) Harvard University 2007-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Transform.Hs.Desugar (
    Desugar(..)
  ) where

import Control.Monad

import Check.Hs.Monad
import Control.Monad.ContextException
import Data.Loc
import Data.Name
import Language.Hs.Syntax
import Text.PrettyPrint.Mainland
\end{code}
%endif

\section{Desugaring Haskell}

\begin{code}
class Desugar a b where
    desugar :: MonadTc r m => a -> m b

instance Desugar a b => Desugar (Maybe a) (Maybe b) where
    desugar  Nothing   = return Nothing
    desugar  (Just a)  = do  b <- desugar a
                             return $ Just b
\end{code}

\subsection{Desugaring Declarations}

\begin{code}
instance Desugar [Decl] [Decl] where
    desugar decls = liftM concat $ mapM desugar decls
\end{code}

\begin{code}

instance Desugar Decl [Decl] where
    desugar decl@(TypeDecl _ _ _ _) =
        return [decl]

    desugar decl@(DataDecl _ _ _ _ _ _ _) =
        return [decl]

    desugar (ClassDecl ctx tycon tys decls sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            decls' <- desugar decls
            return [ClassDecl ctx tycon tys decls' sloc]

    desugar (InstDecl ctx tycon tys decls sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            decls' <- desugar decls
            return [InstDecl ctx tycon tys decls' sloc]

    desugar decl@(DefaultDecl _ _) =
        return [decl]

    desugar (ExpBindDecl _ _ _) =
        error "desugar: encountered ExpBindDecl"

    desugar (VarBindDecl v ps rhs sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            ps'   <- mapM desugar ps
            rhs'  <- desugar rhs
            return [VarBindDecl v ps' rhs' sloc]
\end{code}

\begin{code}
    desugar (PatBindDecl p rhs sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            p'    <- desugar p
            rhs'  <- desugar rhs
            case p' of
              (AsPat v (WildPat _) _) -> return [VarBindDecl v [] rhs' sloc]
              _ -> do
                  topn             <- uniqueName
                  let topv         = Var topn
                  let topBinding   = PatBindDecl (AsPat topv (WildPat sloc) sloc) rhs' sloc
                  let patBindings  = map (bindPat p' topv) (binders p')
                  return $ topBinding : patBindings
      where
        bindPat :: Pat -> Var -> Var -> Decl
        bindPat pat topv pv =
            PatBindDecl  (AsPat pv (WildPat sloc) sloc)
                         (Rhs [(Nothing, AppExp  (LamExp pat' (VarExp pv sloc) sloc)
                                                 (VarExp topv sloc)
                                                 sloc)]
                              [])
                         sloc
          where
            pat' = wildcardAllBut pv pat

        wildcardAllBut :: Var -> Pat -> Pat
        wildcardAllBut  _   p@(WildPat _)        = p
        wildcardAllBut  pv  (IrrefutPat p sloc)  = IrrefutPat (wildcardAllBut pv p) sloc
        wildcardAllBut  _   p@(LitPat _ _)       = p
\end{code}

\XXX{I think this is wrong... $n+k$ patterns are evil!}

\begin{code}
        wildcardAllBut  pv  p@(NPlusKPat v _ sloc)
            | v == pv    = p
            | otherwise  = WildPat sloc
\end{code}

\begin{code}
        wildcardAllBut  pv  (ConPat con ps sloc) =
            ConPat con (map (wildcardAllBut pv) ps) sloc

        wildcardAllBut  pv  (OpConPat p1 op fix p2 sloc) =
            OpConPat (wildcardAllBut pv p1) op fix (wildcardAllBut pv p2) sloc

        wildcardAllBut  pv  (RecConPat con flds sloc) =
            RecConPat con (vs `zip` map (wildcardAllBut pv) ps) sloc
          where
            (vs, ps) = unzip flds

        wildcardAllBut  pv  p@(AsPat pv' subp _)
            | pv' == pv  = p
            | otherwise  = wildcardAllBut pv subp

        wildcardAllBut  pv  (SigPat p ty sloc) =
            SigPat (wildcardAllBut pv p) ty sloc
\end{code}

\begin{code}
    desugar decl@(SigDecl _ _ _) =
        return [decl]

    desugar decl@(FixityDecl _ _ _ _) =
        return [decl]
\end{code}

\begin{code}
instance Desugar Rhs Rhs where
    desugar (Rhs guards decls) =
        do  guards' <- mapM desugar guards
            decls' <- desugar decls
            return $ Rhs guards' decls'
\end{code}

\begin{code}
instance Desugar (Maybe Exp, Exp) (Maybe Exp, Exp) where
    desugar (Nothing, e) =
        do  e' <- desugar e
            return (Nothing, e')

    desugar (Just guard, e) =
        do  guard'  <- desugar guard
            e'      <- desugar e
            return (Just guard', e')
\end{code}

\subsection{Desugaring Expressions}

\begin{code}
instance Desugar Exp Exp where
    desugar (RecConExp con flds sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            es' <- mapM desugar es
            return $ RecConExp con (vs `zip` es') sloc
      where
        (vs, es) = unzip flds

    desugar (RecUpdateExp e flds sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e'   <- desugar e
            es'  <- mapM desugar es
            return $ RecUpdateExp e' (vs `zip` es') sloc
      where
        (vs, es) = unzip flds

    desugar (ListCompExp e quals sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e'      <- desugar e
            quals'  <- mapM desugar quals
            return $ ListCompExp e' quals' sloc

    desugar (ArithSeqExp from maybe_then maybe_to sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            from'        <- desugar from
            maybe_then'  <- desugar maybe_then
            maybe_to'    <- desugar maybe_to
            return $ ArithSeqExp from' maybe_then' maybe_to' sloc

    desugar (LamExp p e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            p' <- desugar p
            e' <- desugar e
            return $ LamExp p' e' sloc

    desugar (AppExp e1 e2 sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e1' <- desugar e1
            e2' <- desugar e2
            return $ AppExp e1' e2' sloc

    desugar (OpAppExp e1 op p e2 sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e1' <- desugar e1
            op' <- desugar op
            e2' <- desugar e2
            return $ OpAppExp e1' op' p e2' sloc

    desugar (NegAppExp e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e' <- desugar e
            return $ NegAppExp e' sloc

    desugar (LetExp decls e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            decls'  <- desugar decls
            e'      <- desugar e
            return $ LetExp decls' e' sloc

    desugar (IfExp test_e then_e else_e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            test_e'  <- desugar test_e
            then_e'  <- desugar then_e
            else_e'  <- desugar else_e
            return $ IfExp test_e' then_e' else_e' sloc

    desugar (CaseExp e alts sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e'     <- desugar e
            alts'  <- mapM desugar alts
            return $ CaseExp e' alts' sloc

    desugar (LSection e1 op sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            name      <- uniqueName
            let v     = Var name
            e1'       <- desugar e1
            op'       <- desugar op
            let e2    = VarExp v sloc
            return $ LamExp  (AsPat v (WildPat sloc) sloc)
                             (AppExp (AppExp op' e1' sloc) e2 sloc)
                             sloc

    desugar (RSection op e2 sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            name    <- uniqueName
            let v   = Var name
            let e1  = VarExp v sloc
            op'     <- desugar op
            e2'     <- desugar e2
            return $ LamExp  (AsPat v (WildPat sloc) sloc)
                             (AppExp (AppExp op' e1 sloc) e2' sloc)
                             sloc

    desugar (SigExp e ty sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e' <- desugar e
            return $ SigExp e' ty sloc

    desugar (ParExp e _) = desugar e

    desugar e = return e
\end{code}

\begin{code}
instance Desugar Qual Qual where
    desugar (GenQual p e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            p'  <- desugar p
            e'  <- desugar e
            return $ GenQual p' e' sloc

    desugar (LetQual decls sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            decls' <- desugar decls
            return $ LetQual decls' sloc

    desugar (GuardQual e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e' <- desugar e
            return $ GuardQual e' sloc
\end{code}

\begin{code}
instance Desugar Alt Alt where
    desugar (Alt p guards decls) = do
        p'       <- desugar p
        guards'  <- mapM desugar guards
        decls'   <- desugar decls
        return $ Alt p' guards' decls'
\end{code}

\subsection{Desugaring Patterns}

\begin{code}
instance Desugar Pat Pat where
    desugar (LitPat lit sloc@(SrcLoc loc)) =
        withLocContext loc empty $ desugarLitPat lit
      where
        desugarLitPat :: MonadTc r m => Lit -> m Pat
        desugarLitPat  (StringLit s)     = desugar $
                                           foldr (\hd tl -> ConPat (Con builtinCons) [hd, tl] sloc)
                                           (ConPat (Con builtinNil) [] sloc)
                                           (map (\c -> LitPat (CharLit c) sloc) s)
        desugarLitPat  lit               = return $ LitPat lit sloc

    desugar (OpConPat p1 con _ p2 sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            p1' <- desugar p1
            p2' <- desugar p2
            return $ ConPat con [p1', p2'] sloc

    desugar (ConPat con ps sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            ps' <- mapM desugar ps
            return $ ConPat con ps' sloc

    desugar (AsPat v p sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            p' <- desugar p
            return $ AsPat v p' sloc

    desugar (SigPat p ty sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            p' <- desugar p
            return $ SigPat p' ty sloc

    desugar p = return p
\end{code}

\subsection{Desugaring Statements}

\begin{code}
instance Desugar Stmt Stmt where
    desugar (ExpStmt e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e' <- desugar e
            return $ ExpStmt e' sloc

    desugar (PatStmt p e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            p' <- desugar p
            e' <- desugar e
            return $ PatStmt p' e' sloc

    desugar (LetStmt decls sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            decls' <- desugar decls
            return $ LetStmt decls' sloc
\end{code}
