%if False
\begin{code}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

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
-- Module      :  Language.Hs.Parser.Utils
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.Hs.Parser.Utils
    (checkExp,
     checkPat,
     checkPats,
     checkContext,
     checkData,
     checkClass,
     checkInstance)
  where

import Control.Monad.Error

import Control.Monad.ContextException
import Control.Monad.Exception
import Data.Loc
import Data.Name
import Language.Hs
import Language.Hs.Parser.Exceptions
import Text.PrettyPrint.Mainland
\end{code}
%endif

\section{Parsing Utilities}

\begin{code}
checkExp  ::  MonadContextException m
          =>  Exp
          ->  m Exp
checkExp (WildPatExp _)           =  throwException BadExp
checkExp (AsPatExp _ _ _)         =  throwException BadExp
checkExp (IrrefutPatExp _ _)      =  throwException BadExp
checkExp e@(VarExp _ _)           =  return e
checkExp e@(ConExp _ _)           =  return e
checkExp e@(ListCompExp _ _ _)    =  return e
checkExp e@(ArithSeqExp _ _ _ _)  =  return e
checkExp e@(LitExp _ _)           =  return e
checkExp e@(LamExp _ _ _)         =  return e
checkExp e@(LetExp _ _ _)         =  return e
checkExp e@(IfExp _ _ _ _)        =  return e
checkExp e@(CaseExp _ _ _)        =  return e
checkExp e@(DoExp _ _)            =  return e
checkExp e@(ParExp _ _)           =  return e
checkExp e@(AntiExp _ _)          =  return e

checkExp (RecConExp con flds sloc@(SrcLoc loc)) =
    withLocContext loc empty $
    do  let (vs, es) = unzip flds
        es' <- mapM checkExp es
        return $ RecConExp con (vs `zip` es') sloc

checkExp (RecUpdateExp e flds sloc@(SrcLoc loc)) =
    withLocContext loc empty $
    do  e' <- checkExp e
        let (vs, es) = unzip flds
        es' <- mapM checkExp es
        return $ RecUpdateExp e' (vs `zip` es') sloc

checkExp (OpAppExp e1 op fix e2 sloc@(SrcLoc loc)) =
    withLocContext loc empty $
    do  e1' <- checkExp e1
        op' <- checkExp op
        e2' <- checkExp e2
        return $ OpAppExp e1' op' fix e2' sloc

checkExp (NegAppExp e sloc@(SrcLoc loc)) =
    withLocContext loc empty $
    do  e' <- checkExp e
        return $ NegAppExp e' sloc

checkExp (AppExp e1 e2 sloc@(SrcLoc loc)) =
    withLocContext loc empty $
    do  e1' <- checkExp e1
        e2' <- checkExp e2
        return $ AppExp e1' e2' sloc

checkExp (LSection e1 e2 sloc@(SrcLoc loc)) =
    withLocContext loc empty $
    do  e1' <- checkExp e1
        e2' <- checkExp e2
        return $ LSection e1' e2' sloc

checkExp (RSection e1 e2 sloc@(SrcLoc loc)) =
    withLocContext loc empty $
    do  e1' <- checkExp e1
        e2' <- checkExp e2
        return $ RSection e1' e2' sloc

checkExp (SigExp e ty sloc@(SrcLoc loc)) =
    withLocContext loc empty $
    do  e' <- checkExp e
        return $ SigExp e' ty sloc
\end{code}

\begin{code}
checkPat  ::  MonadContextException m
          =>  Exp
          ->  m Pat
checkPat (WildPatExp sloc)       =  return $ WildPat sloc
checkPat (AsPatExp v e sloc)     =  do  p <- checkPat e
                                        return $ AsPat v p sloc
checkPat (IrrefutPatExp e sloc)  =  do  p <- checkPat e
                                        return $ IrrefutPat p sloc
checkPat (VarExp v sloc)         =  return $ AsPat v (WildPat sloc) sloc
checkPat (ConExp con sloc)       =  return $ ConPat con [] sloc
checkPat (RecUpdateExp _ _ _)    =  throwException BadPat
checkPat (ListCompExp _ _ _)     =  throwException BadPat
checkPat (ArithSeqExp _ _ _ _)   =  throwException BadPat
checkPat (LitExp lit sloc)       =  return $ LitPat lit sloc
checkPat (LamExp  _ _ _)         =  throwException BadPat
checkPat (LetExp _ _ _)          =  throwException BadPat
checkPat (IfExp _ _ _ _)         =  throwException BadPat
checkPat (CaseExp _ _ _)         =  throwException BadPat
checkPat (DoExp _ _)             =  throwException BadPat
checkPat (ParExp e _)            =  checkPat e
checkPat (LSection _ _ _)        =  throwException BadPat
checkPat (RSection _ _ _)        =  throwException BadPat
checkPat (SigExp e ty sloc)      =  do  p <- checkPat e
                                        return $ SigPat p ty sloc

checkPat (RecConExp con flds sloc) =
    do  let (vs, es) = unzip flds
        ps <- mapM checkPat es
        return $ RecConPat con (vs `zip` ps) sloc

checkPat (OpAppExp  (VarExp v@(Var _) _)
                    (VarExp (Var plus) _)
                    _
                    (LitExp (IntegerLit i) _)
                    sloc)
    | plus == prelPlus =
        return $ NPlusKPat v i sloc

checkPat (OpAppExp e1 (ConExp con@(Con _) _) fix e2 sloc) = do
  p1  <- checkPat e1
  p2  <- checkPat e2
  return $ OpConPat p1 con fix p2 sloc

checkPat (OpAppExp _ _ _ _ _) =
    throwException BadPat

checkPat  (NegAppExp (LitExp (IntegerLit i) sloc) _) =
    return $ LitPat (IntegerLit (-i)) sloc

checkPat  (NegAppExp (LitExp (FloatLit f) sloc) _) =
    return $ LitPat (FloatLit (-f)) sloc

checkPat  (NegAppExp _ _) =
    throwException BadPat

checkPat e@(AppExp _ _ _) =
    do  (p : ps) <- checkPats e
        case p of
          ConPat con ps1 sloc ->  return $ ConPat con (ps1 ++ ps) sloc
          _                   ->  throwException BadPat

checkPat (AntiExp _ _) =
    throwException BadPat
\end{code}

\begin{code}
checkPats  ::  MonadContextException m
           =>  Exp
           ->  m [Pat]
checkPats e =
    do  let exps = unfoldAppExp e
        mapM checkPat exps
\end{code}

\begin{code}
checkContext  ::  MonadContextException m
              =>  Type
              ->  m [Pred]
checkContext ty@(AppTy _ _) =
    case unfoldAppTy ty of
      (TyConTy (TupleTyCon _) : tys)  ->  do  preds <- mapM checkPred tys
                                              return preds
      _                               ->  do  pred <- checkPred ty
                                              return [pred]
checkContext ty = do  pred <- checkPred ty
                      return [pred]
\end{code}

\begin{code}
checkPred  ::  Monad m
           =>  Type
           ->  m Pred
checkPred  (TyConTy tycon)   = return $ ClassPred tycon []
checkPred  (TyVarTy _)       = fail "Malformed context"
checkPred  (ForAll _ _ _ _)  = fail "Malformed context"
checkPred  (AntiType _)      = error "checkPred: encountered AntiType"

checkPred ty@(AppTy _ _)    = checkAppTy (unfoldAppTy ty)
  where
    checkAppTy :: Monad m => [Type] -> m Pred
    checkAppTy  (TyConTy tycon : tys)   =  return $ ClassPred tycon tys
    checkAppTy  _                       =  fail "Malformed class"
\end{code}

\begin{code}
checkData  ::  Monad m
           =>  Type
           ->  m (TyCon, [TyVar])
checkData  (TyConTy (tycon))  = return (tycon, [])
checkData  (TyVarTy _)        = fail "Malformed data declaration"
checkData  (ForAll _ _ _ _)   = fail "Malformed data declaration"
checkData  (AntiType _)       = error "checkData: encountered AntiType"

checkData ty@(AppTy _ _) =
    checkAppTy (unfoldAppTy ty)
  where
    checkAppTy :: Monad m => [Type] -> m (TyCon, [TyVar])
    checkAppTy  (TyConTy tycon : tys)   =  do  tvs <- mapM checkTyVar tys
                                               return $ (tycon, tvs)
    checkAppTy  _                       =  fail "Malformed data declaration"

    checkTyVar :: Monad m => Type -> m TyVar
    checkTyVar (TyVarTy tv)      = return tv
    checkTyVar _                 = fail "Malformed data declaration"
\end{code}

\begin{code}
checkClass  ::  Monad m
            =>  Type
            ->  m (TyCon, [TyVar])
checkClass (TyConTy (tycon))  = return (tycon, [])
checkClass (TyVarTy _)        = fail "Malformed class"
checkClass  (ForAll _ _ _ _)  = fail "Malformed class"
checkClass (AntiType _)       = error "checkClass: encountered AntiType"

checkClass ty@(AppTy _ _) =
    checkAppTy (unfoldAppTy ty)
  where
    checkAppTy :: Monad m => [Type] -> m (TyCon, [TyVar])
    checkAppTy  (TyConTy tycon : tys)   =  do  tvs <- mapM checkTyVar tys
                                               return (tycon, tvs)
    checkAppTy  _                       =  fail "Malformed class"

    checkTyVar :: Monad m => Type -> m TyVar
    checkTyVar (TyVarTy tv)      = return tv
    checkTyVar _                 = fail "Malformed class"
\end{code}

\begin{code}
checkInstance  ::  Monad m
            =>  Type
            ->  m (TyCon, [Type])
checkInstance  (TyConTy (tycon))  = return (tycon, [])
checkInstance  (TyVarTy _)        = fail "Malformed class"
checkInstance  (ForAll _ _ _ _)   = fail "Malformed class"
checkInstance  (AntiType _)       = error "checkInstance: encountered AntiType"

checkInstance ty@(AppTy _ _) =
    checkAppTy (unfoldAppTy ty)
  where
    checkAppTy :: Monad m => [Type] -> m (TyCon, [Type])
    checkAppTy  (TyConTy tycon : tys)   =  return (tycon, tys)
    checkAppTy  _                       =  fail "Malformed class"
\end{code}
