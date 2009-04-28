%if False
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
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
-- Module      :  Transform.Hs.Rename
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Transform.Hs.Rename (
    Rename(..),
    Fixinfix(..)
  ) where

import Control.Monad

import Check.Hs.Monad
import Control.Monad.Exception
import Control.Monad.ContextException
import Data.Loc
import Data.Name
import Language.Hs.Parser
import Language.Hs.Syntax
import Text.PrettyPrint.Mainland
\end{code}
%endif

\section{Renaming Binders}

\begin{code}
class Rename a where
    rename :: (MonadTc r m) => a -> m a

instance (Rename a) => Rename (Maybe a) where
    rename Nothing   = return Nothing
    rename (Just a)  = do  a' <- rename a
                           return $ Just a'

instance (Rename a) => Rename [a] where
    rename as = mapM rename as

instance (Rename a, Rename b) => Rename (a, b) where
    rename (a, b) = do
        a' <- rename a
        b' <- rename b
        return (a', b')
\end{code}

\begin{code}
instance Rename TyCon where
    rename = return

instance Rename Var where
    rename = return

instance Rename Con where
    rename = return
\end{code}

\begin{code}
instance Rename Type where
    rename (TyConTy tycon) = do
        tycon' <- rename tycon
        return $ TyConTy tycon'

    rename ty@(TyVarTy _) =
        return ty

    rename (AppTy ty1 ty2) = do
        ty1' <- rename ty1
        ty2' <- rename ty2
        return $ AppTy ty1' ty2'

    rename (ForAll explicit tvs ctx ty) = do
        ctx'  <- rename ctx
        ty'   <- rename ty
        return $ ForAll explicit tvs ctx' ty'

    rename ty@(AntiType _) =
        return ty

instance Rename Pred where
    rename (ClassPred tycon tys) = do
        tycon'  <- rename tycon
        tys'    <- rename tys
        return $ ClassPred tycon' tys'
\end{code}

\begin{code}
instance Rename ConDecl where
    rename (ConDecl con fix tys sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            tys' <- rename tys
            return $ ConDecl con fix tys' sloc

    rename (OpConDecl ty1 con _ ty2 sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            ty1' <- rename ty1
            fix  <- lookupConFixity con
            ty2' <- rename ty2
            return $ OpConDecl ty1' con fix ty2' sloc

    rename (RecConDecl con fix vs sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            vs' <- rename vs
            return $ RecConDecl con fix vs' sloc

instance Rename [Decl] where
    rename decls = do
        extendConFixities conFixities
        extendVarFixities varFixities
        mapM rename decls
      where
        fixities :: [(Name, OpFixity)]
        fixities = [(n, (fixityToAssoc f, i)) |  FixityDecl ns f i _ <- decls,
                                                 n <- ns]

        conFixities :: [(Con, OpFixity)]
        conFixities = [(Con n, fix) |  (n, fix) <- fixities,
                                       isConid n || isConsym n]

        varFixities :: [(Var, OpFixity)]
        varFixities = [(Var n, fix) |  (n, fix) <- fixities,
                                       isVarid n || isVarsym n]

instance Rename Decl where
    rename (TypeDecl tycon tvs ty sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            ty' <- rename ty
            return $ TypeDecl tycon tvs ty' sloc

    rename (DataDecl dorn ctx tycon tvs condecls derives sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            ctx'       <- rename ctx
            condecls'  <- rename condecls
            derives'   <- rename derives
            return $ DataDecl dorn ctx' tycon tvs condecls' derives' sloc

    rename (ClassDecl ctx tycon tvs decls sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            ctx'    <- rename ctx
            decls'  <- rename decls
            return $ ClassDecl ctx' tycon tvs decls' sloc

    rename (InstDecl ctx tycon tys decls sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            ctx'    <- rename ctx
            decls'  <- rename decls
            return $ InstDecl ctx' tycon tys decls' sloc

    rename (DefaultDecl tys sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            tys' <- rename tys
            return $ DefaultDecl tys' sloc

    rename (ExpBindDecl e rhs sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            (p : ps)  <- fixinfix e >>= checkPats
            rhs'      <- rename rhs
            case p : ps of
              [_] ->
                  return $ PatBindDecl p rhs' sloc
              AsPat v (WildPat _) _ : _ ->
                  return $ VarBindDecl v ps rhs' sloc
              _  ->
                  do   p <- checkPat e
                       return $ PatBindDecl p rhs' sloc

    rename (VarBindDecl v ps rhs sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            ps'   <- rename ps
            rhs'  <- rename rhs
            return $ VarBindDecl v ps' rhs' sloc

    rename (PatBindDecl p rhs sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            p'    <- fixinfix p
            rhs'  <- rename rhs
            return $ PatBindDecl p' rhs' sloc

    rename decl@(SigDecl _ _ _) =
        return decl

    rename decl@(FixityDecl _ _ _ _) =
        return decl
\end{code}

\begin{code}
instance Rename Rhs where
    rename (Rhs guards decls) = do
        guards'  <- rename guards
        decls'   <- rename decls
        return $ Rhs guards' decls'
\end{code}

\begin{code}
instance Rename (Maybe Exp, Exp) where
    rename (Nothing, e) =
        do  e' <- fixinfix e
            return (Nothing, e')

    rename (Just guard, e) =
        do  guard'  <- fixinfix guard
            e'      <- fixinfix e
            return (Just guard', e')
\end{code}

\subsection{Renaming Expressions}

\begin{code}
instance Rename Exp where
    rename e@(WildPatExp _)       = return e
    rename e@(AsPatExp _ _ _)     = return e
    rename e@(IrrefutPatExp _ _)  = return e

    rename e@(LitExp _ _) =
        return e

    rename (VarExp v sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            v' <- rename v
            return $ VarExp v' sloc

    rename (ConExp con sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            con' <- rename con
            return $ ConExp con' sloc

    rename (RecConExp con flds sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            es' <- rename es
            return $ RecConExp con (vs `zip` es') sloc
      where
        (vs, es) = unzip flds

    rename (RecUpdateExp e flds sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e'   <- rename e
            es'  <- rename es
            return $ RecUpdateExp e' (vs `zip` es') sloc
      where
        (vs, es) = unzip flds

    rename (ListCompExp e quals sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e'      <- rename e
            quals'  <- rename quals
            return $ ListCompExp e' quals' sloc

    rename (ArithSeqExp from maybe_then maybe_to sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            from'        <- rename from
            maybe_then'  <- rename maybe_then
            maybe_to'    <- rename maybe_to
            return $ ArithSeqExp from' maybe_then' maybe_to' sloc

    rename (LamExp p e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            p' <- rename p
            e' <- rename e
            return $ LamExp p' e' sloc

    rename (AppExp e1 e2 sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e1' <- rename e1
            e2' <- rename e2
            return $ AppExp e1' e2' sloc

    rename (OpAppExp e1 op _ e2 sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e1'  <- rename e1
            op'  <- rename op
            fix  <- case op of
                      ConExp con _  -> lookupConFixity con
                      VarExp v _    -> lookupVarFixity v
                      _             -> fail "rename: impossible OpAppExp"
            e2'  <- rename e2
            return $ OpAppExp e1' op' fix e2' sloc

    rename (NegAppExp e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e' <- rename e
            return $ NegAppExp e' sloc

    rename (LetExp decls e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            decls'  <- rename decls
            e'      <- rename e
            return $ LetExp decls' e' sloc

    rename (IfExp test_e then_e else_e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            test_e'  <- rename test_e
            then_e'  <- rename then_e
            else_e'  <- rename else_e
            return $ IfExp test_e' then_e' else_e' sloc

    rename (CaseExp e alts sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e'     <- rename e
            alts'  <- rename alts
            return $ CaseExp e' alts' sloc

    rename (DoExp stms sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            stms' <- rename stms
            return $ DoExp stms' sloc

    rename (LSection e1 op sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e1'  <- rename e1
            op'  <- rename op
            return $ LSection e1' op' sloc

    rename (RSection op e2 sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            op'  <- rename op
            e2'  <- rename e2
            return $ RSection op' e2' sloc

    rename (SigExp e ty sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e' <- rename e
            return $ SigExp e' ty sloc

    rename (ParExp e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e' <- rename e
            return $ ParExp e' sloc

    rename e@(AntiExp _ _) =
        return e
\end{code}

\begin{code}
instance Rename Qual where
    rename (GenQual p e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            p'  <- rename p
            e'  <- rename e
            return $ GenQual p' e' sloc

    rename (LetQual decls sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            decls' <- rename decls
            return $ LetQual decls' sloc

    rename (GuardQual e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e' <- rename e
            return $ GuardQual e' sloc
\end{code}

\begin{code}
instance Rename Alt where
    rename (Alt p guards decls) = do
        p'       <- fixinfix p
        guards'  <- rename guards
        decls'   <- rename decls
        return $ Alt p' guards' decls'
\end{code}

\subsection{Renaming Patterns}

\begin{code}
instance Rename Pat where
    rename p@(WildPat _) =
        return p

    rename p@(IrrefutPat _ _) =
        return p

    rename p@(LitPat _ _) =
        return p

    rename p@(NPlusKPat _ _ _)=
        return p

    rename (ConPat con ps sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            ps' <- rename ps
            return $ ConPat con ps' sloc

    rename (OpConPat p1 con _ p2 sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            p1'  <- rename p1
            fix  <- lookupConFixity con
            p2'  <- rename p2
            return $ OpConPat p1' con fix p2' sloc

    rename (RecConPat con vps sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            con' <- rename con
            ps'  <- rename ps
            return $ RecConPat con' (vs `zip` ps') sloc
      where
        (vs, ps) = unzip vps

    rename (AsPat v p sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            p' <- rename p
            return $ AsPat v p' sloc

    rename (SigPat p ty sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            p' <- rename p
            return $ SigPat p' ty sloc
\end{code}

\subsection{Renameing Statements}

\begin{code}
instance Rename Stmt where
    rename (ExpStmt e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e' <- fixinfix e
            return $ ExpStmt e' sloc

    rename (PatStmt p e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            p' <- fixinfix p
            e' <- fixinfix e
            return $ PatStmt p' e' sloc

    rename (LetStmt decls sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            decls' <- rename decls
            return $ LetStmt decls' sloc
\end{code}

\section{Fixing infix expressions}

(Bool,         -- Precedence error
Bool)         -- Associate to the right: a op1 (b op2 c)

\begin{code}
assocRight  ::  OpFixity
            ->  OpFixity
            ->  (Bool, Bool)
assocRight (assoc1, p1) (assoc2, p2)
  = case p1 `compare` p2 of
      GT ->  left
      LT ->  right
      EQ ->  case (assoc1, assoc2) of
               (LeftAssoc, LeftAssoc)    -> left
               (RightAssoc, RightAssoc)  -> right
               _                         -> err
  where
    right  = (False, True)
    left   = (False, False)
    err    = (True, False)
\end{code}

\begin{code}
class Fixinfix a where
    fixinfix :: (MonadTc r m) => a -> m a

instance (Fixinfix a) => Fixinfix (Maybe a) where
    fixinfix Nothing   = return Nothing
    fixinfix (Just a)  = do  a' <- fixinfix a
                             return $ Just a'

instance (Fixinfix a) => Fixinfix [a] where
    fixinfix as = mapM fixinfix as

instance (Fixinfix a, Fixinfix b) => Fixinfix (a, b) where
    fixinfix (a, b) = do
        a' <- fixinfix a
        b' <- fixinfix b
        return (a', b')
\end{code}

\begin{code}
instance Fixinfix Exp where
    fixinfix e@(WildPatExp _)        = return e
    fixinfix e@(AsPatExp _ _ _)     = return e
    fixinfix e@(IrrefutPatExp _ _)  = return e

    fixinfix e@(LitExp _ _) =
        return e

    fixinfix (VarExp v sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            v' <- rename v
            return $ VarExp v' sloc

    fixinfix (ConExp con sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            con' <- rename con
            return $ ConExp con' sloc

    fixinfix (RecConExp con flds sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            es' <- fixinfix es
            return $ RecConExp con (vs `zip` es') sloc
      where
        (vs, es) = unzip flds

    fixinfix (RecUpdateExp e flds sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e'   <- fixinfix e
            es'  <- fixinfix es
            return $ RecUpdateExp e' (vs `zip` es') sloc
      where
        (vs, es) = unzip flds

    fixinfix (ListCompExp e quals sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e'      <- fixinfix e
            quals'  <- rename quals
            return $ ListCompExp e' quals' sloc

    fixinfix (ArithSeqExp from maybe_then maybe_to sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            from'        <- fixinfix from
            maybe_then'  <- fixinfix maybe_then
            maybe_to'    <- fixinfix maybe_to
            return $ ArithSeqExp from' maybe_then' maybe_to' sloc

    fixinfix (LamExp p e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            p' <- fixinfix p
            e' <- fixinfix e
            return $ LamExp p' e' sloc

    fixinfix (AppExp e1 e2 sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e1' <- fixinfix e1
            e2' <- fixinfix e2
            return $ AppExp e1' e2' sloc

    fixinfix (OpAppExp e1 op _ e2 (SrcLoc loc)) =
        withLocContext loc empty $ do
            e1'  <- rename e1
            op'  <- rename op
            fix  <- case op of
                      ConExp con _  -> lookupConFixity con
                      VarExp v _    -> lookupVarFixity v
                      _             -> fail "fixinfix: impossible OpAppExp"
            e2'  <- rename e2
            fixinfixExp e1' op' fix e2'

    fixinfix (NegAppExp e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e' <- fixinfix e
            return $ NegAppExp e' sloc

    fixinfix (LetExp decls e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            decls'  <- rename decls
            e'      <- fixinfix e
            return $ LetExp decls' e' sloc

    fixinfix (IfExp test_e then_e else_e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            test_e'  <- fixinfix test_e
            then_e'  <- fixinfix then_e
            else_e'  <- fixinfix else_e
            return $ IfExp test_e' then_e' else_e' sloc

    fixinfix (CaseExp e alts sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e'     <- fixinfix e
            alts'  <- rename alts
            return $ CaseExp e' alts' sloc

    fixinfix (DoExp stms sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            stms' <- rename stms
            return $ DoExp stms' sloc

    fixinfix (LSection e1 op sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e1'  <- fixinfix e1
            op'  <- fixinfix op
            return $ LSection e1' op' sloc

    fixinfix (RSection op e2 sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            op'  <- fixinfix op
            e2'  <- fixinfix e2
            return $ RSection op' e2' sloc

    fixinfix (SigExp e ty sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e' <- fixinfix e
            return $ SigExp e' ty sloc

    fixinfix (ParExp e sloc@(SrcLoc loc)) =
        withLocContext loc empty $ do
            e' <- fixinfix e
            return $ ParExp e' sloc

    fixinfix e@(AntiExp _ _) =
        return e
\end{code}

\begin{code}
fixinfixExp  ::  (MonadTc r m)
             =>  Exp
             ->  Exp -> OpFixity
             ->  Exp
             ->  m Exp
fixinfixExp e1 op fix e2 =
    go e1 op fix e2
  where
\end{code}

$(e_11 `op1` e_12) `op_2` e_2$

\begin{code}
    go (OpAppExp e11 op1 fix1 e12 _) op2 fix2 _
        | err          = throwException $ PrecedenceError (ppr op1) fix1 (ppr op2) fix2
        | assoc_right  = do  let loc  = getLoc e12 <--> getLoc e2
                             let e2'  = OpAppExp e12 op2 fix2 e2 (SrcLoc loc)
                             fixinfixExp e11 op1 fix1 e2'
        | otherwise    = do  e1' <- fixinfix e1
                             let loc  = getLoc e1' <--> getLoc e2
                             return $ OpAppExp e1' op2 fix2 e2 (SrcLoc loc)
      where
        (err, assoc_right) = assocRight fix1 fix2
\end{code}

$(- \rm{neg_arg}) `op` e_2$

\begin{code}
    go (NegAppExp _ _) op fix _
        | err          = throwException $ PrecedenceError (text "prefix -") negateFixity (ppr op) fix
        | assoc_right  = return $ OpAppExp e1 op fix e2 (SrcLoc loc)
      where
        (err, assoc_right) = assocRight negateFixity fix
        loc = getLoc e1 <--> getLoc e2
\end{code}

$e_1 `op` (- \rm{neg_arg})$

\begin{code}
    go _ op fix (NegAppExp _ _)
        | not assoc_right  = throwException $ PrecedenceError (ppr op) fix (text "prefix -") negateFixity
      where
        (_, assoc_right) = assocRight fix negateFixity
\end{code}

\begin{code}
    go _ _ _ _ = return $ OpAppExp e1 op fix e2 (SrcLoc loc)
      where
        loc = getLoc e1 <--> getLoc e2
\end{code}

\begin{code}
instance Fixinfix Pat where
    fixinfix p@(WildPat _) =
        return p

    fixinfix p@(IrrefutPat _ _) =
        return p

    fixinfix p@(LitPat _ _) =
        return p

    fixinfix p@(NPlusKPat _ _ _) =
        return p

    fixinfix (ConPat con ps sloc@(SrcLoc loc)) =
      withLocContext loc empty $ do
        ps' <- fixinfix ps
        return $ ConPat con ps' sloc

    fixinfix (OpConPat p1 con _ p2 (SrcLoc loc)) =
      withLocContext loc empty $ do
        p1'   <- rename p1
        con'  <- rename con
        fix   <- lookupConFixity con
        p2'   <- rename p2
        fixinfixPat p1' con' fix p2'

    fixinfix (RecConPat con vps sloc@(SrcLoc loc)) =
      withLocContext loc empty $ do
        con' <- rename con
        ps'  <- rename ps
        return $ RecConPat con' (vs `zip` ps') sloc
      where
        (vs, ps) = unzip vps

    fixinfix (AsPat v p sloc@(SrcLoc loc)) =
      withLocContext loc empty $ do
        p' <- fixinfix p
        return $ AsPat v p' sloc

    fixinfix (SigPat p ty sloc@(SrcLoc loc)) =
      withLocContext loc empty $ do
        p' <- fixinfix p
        return $ SigPat p' ty sloc
\end{code}

\begin{code}
fixinfixPat  ::  (MonadTc r m)
             =>  Pat
             ->  Con -> OpFixity
             ->  Pat
             ->  m Pat
fixinfixPat p1 con fix p2 =
    go p1 con fix p2
  where
\end{code}

$(p_11 `op1` p_12) `op_2` p_2$

\begin{code}
    go p1@(OpConPat p11 op1 fix1 p12 _) op2 fix2 p2
        | err          = throwException $ PrecedenceError (ppr op1) fix1 (ppr op2) fix2
        | assoc_right  = do  let loc  = getLoc p12 <--> getLoc p2
                             let p2'  = OpConPat p12 op2 fix2 p2 (SrcLoc loc)
                             fixinfixPat p11 op1 fix1 p2'
        | otherwise    = do  p1' <- fixinfix p1
                             let loc  = getLoc p1' <--> getLoc p2
                             return $ OpConPat p1' op2 fix2 p2 (SrcLoc loc)
      where
        (err, assoc_right) = assocRight fix1 fix2
\end{code}

\begin{code}
    go p1 op fix p2 = return $ OpConPat p1 op fix p2 (SrcLoc loc)
      where
        loc = getLoc p1 <--> getLoc p2
\end{code}
