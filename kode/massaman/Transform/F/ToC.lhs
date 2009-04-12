%if False
\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
-- Module      :  Transform.F.ToC
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Transform.F.ToC where

import Compiler.Opt
import Control.Monad (ap,
                      filterM,
                      foldM,
                      forM,
                      liftM,
                      when)
import Data.Char (isAlphaNum,
                  isDigit,
                  ord)
import Data.List (intersperse)
import Data.Maybe (catMaybes,
                   mapMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Numeric (showHex)

import qualified Check.F as F.Tc
import Check.F.Check ((===))
import Control.Monad.Exception
import Control.Monad.CGen
import Control.Monad.Ref
import Control.Monad.Trace
import Control.Monad.Unique
import Data.IString
import Data.Loc
import Data.Name
import qualified Language.F as F
import Text.PrettyPrint.Mainland

import Language.C.Syntax
import Language.C.Quote
\end{code}
%endif

\subsection{The C Translation Monad Class}

\begin{code}
data ToCEnv m = ToCEnv
    {  tyvartys :: Map.Map F.TyVar F.Type
    ,  tyapptys :: [F.Type]

    ,  cenums  :: Set.Set F.TyCon
    ,  ctypes  :: Map.Map F.Type Type
    ,  cvars   :: Map.Map F.Var (m CExp)
    }

emptyToCEnv :: ToCEnv m
emptyToCEnv = ToCEnv
    {  tyvartys = Map.empty
    ,  tyapptys = []

    ,  cenums  = Set.empty
    ,  ctypes  = Map.fromList builtinTypes
    ,  cvars   = Map.empty
    }

class (MonadOpts m,
       MonadRef r m,
       MonadTrace m,
       MonadUnique m,
       F.Tc.MonadTc m,
       MonadCGen m)
    => MonadToC r m | m -> r where
    getToCEnv   :: m (ToCEnv m)
    putToCEnv   :: ToCEnv m -> m ()

    getsToCEnv :: (ToCEnv m -> a) -> m a
    getsToCEnv f = getToCEnv >>= \s -> return (f s)

    modifyToCEnv :: (ToCEnv m -> ToCEnv m) -> m ()
    modifyToCEnv f = getToCEnv >>= \s -> putToCEnv (f s)

    tracetoc :: Doc -> m ()
    tracetoc doc = do
        doTrace <- optVal (isFlagSet Opt_d_dump_toc_trace)
        when doTrace $
            trace "traceToC:" doc

    lookupTyVarTy :: F.TyVar -> m F.Type
    lookupTyVarTy tyvar@(F.TyVar n) = do
        maybe_ty <- getsToCEnv (\s -> Map.lookup tyvar (tyvartys s))
        case maybe_ty of
          Nothing  ->  fail $ "unbound type variable `" ++ show n ++ "'"
          Just ty  ->  return ty

    insertTyVarTy :: F.TyVar -> F.Type -> m ()
    insertTyVarTy tyvar kind =
        modifyToCEnv $ \s ->
            s { tyvartys = Map.insert tyvar kind (tyvartys s) }

    extendTyVarTys :: [(F.TyVar, F.Type)] -> m a -> m a
    extendTyVarTys bindings m = do
        old_tyvartys <- getsToCEnv tyvartys
        mapM_ (uncurry insertTyVarTy) bindings
        a <- m
        modifyToCEnv $ \s ->
            s { tyvartys = old_tyvartys }
        return a

    extendWildTyVarTys :: [(F.WildTyVar, F.Type)] -> m a -> m a
    extendWildTyVarTys binds m = extendTyVarTys (mapMaybe tameBinding binds) m
      where
        tameBinding :: (F.WildTyVar, a) -> Maybe (F.TyVar, a)
        tameBinding (F.WildTyVar, _)    = Nothing
        tameBinding (F.TameTyVar v, a)  = Just (v, a)

    tcExp :: F.Exp -> m F.Type
    tcExp e = do
        theta <- getsToCEnv tyvartys
        let phi = free theta
        F.Tc.tcExp (subst theta phi e)

    pushTyAppTy :: F.Type -> m ()
    pushTyAppTy ty = do
        ty' <- compress ty
        modifyToCEnv (\s -> s { tyapptys = ty' : tyapptys s } )

    popTyAppTy :: m F.Type
    popTyAppTy = do
        tys <- getsToCEnv tyapptys
        case tys of
          ty : tys  ->  do  modifyToCEnv (\s -> s { tyapptys = tys } )
                            return ty
          _         ->  fail "internal error: no type vailable in type application"

    bindWildTyVar :: F.WildTyVar -> m a -> m a
    bindWildTyVar tv m = do
        old_tyapptys <- getsToCEnv tyapptys
        when (length old_tyapptys == 0) $
            fail "internal error: not enough type applications"
        modifyToCEnv (\s -> s { tyapptys = tail old_tyapptys } )
        a <- extendWildTyVarTys [(tv, head old_tyapptys)] m
        modifyToCEnv (\s -> s { tyapptys = old_tyapptys })
        return a

    memberCEnum  :: F.TyCon -> m Bool
    memberCEnum tycon =
        getsToCEnv $ \s -> Set.member tycon (cenums s)

    insertCEnum  :: F.TyCon -> m ()
    insertCEnum tycon =
        modifyToCEnv $ \s -> s { cenums = Set.insert tycon (cenums s) }

    lookupCType  :: F.Type -> m (Maybe Type)
    lookupCType ty =
        getsToCEnv $ \s -> Map.lookup ty (ctypes s)

    insertCType  :: F.Type -> Type -> m ()
    insertCType ty cty =
        modifyToCEnv $ \s -> s { ctypes = Map.insert ty cty (ctypes s) }

    lookupCVar :: F.Var -> m (m CExp)
    lookupCVar v@(F.Var n) = do
        maybe_ce <- getsToCEnv (\s -> Map.lookup v (cvars s))
        case maybe_ce of
          Nothing  ->  fail $ "unbound C variable `" ++ show n ++ "'"
          Just ce  ->  return ce

    insertCVar :: F.Var -> m CExp -> m ()
    insertCVar v mcexp =
        modifyToCEnv $ \s -> s { cvars = Map.insert v mcexp (cvars s) }

    extendCVars :: [(F.Var, m CExp)] -> m a -> m a
    extendCVars bindings m = do
        old_cvars <- getsToCEnv cvars
        mapM_ (uncurry insertCVar) bindings
        a <- m
        modifyToCEnv $ \s -> s { cvars = old_cvars }
        return a

    extendWildCVars :: [(F.WildVar, m CExp)] -> m a -> m a
    extendWildCVars binds m = extendCVars (mapMaybe tameBinding binds) m
      where
        tameBinding :: (F.WildVar, m CExp) -> Maybe (F.Var, m CExp)
        tameBinding (F.WildVar, _)     = Nothing
        tameBinding (F.TameVar v, ty)  = Just (v, ty)
\end{code}

\subsection{Builtins}

\begin{code}
builtinCTypedefs :: [Definition]
builtinCTypedefs = [[$cedecl|const unsigned char FALSE = 0;|],
                    [$cedecl|const unsigned char TRUE = 1;|]]
\end{code}

\begin{code}
builtinCVardefs :: [Definition]
builtinCVardefs = []
\end{code}

\begin{code}
builtinTypes ::[(F.Type, Type)]
builtinTypes = [(F.Tc.integerTy,  [$cty|int|]),
                (F.Tc.floatTy,    [$cty|double|]),
                (F.Tc.boolTy,     [$cty|unsigned char|]),
                (F.Tc.charTy,     [$cty|char|]),
                (F.Tc.stringTy,   [$cty|char*|])]
\end{code}

\begin{code}
builtinFuns :: MonadToC r m => [(F.Var, F.Type -> [F.Exp] -> m CExp)]
builtinFuns = [(var "negate", unaryFun $ \e -> return [$cexp|-$exp:e|]),

               (var "==#",  binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 == $exp:e2|]),
               (var "/=#",  binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 != $exp:e2|]),
               (var "<#",   binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 < $exp:e2|]),
               (var "<=#",  binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 <= $exp:e2|]),
               (var ">=#",  binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 >= $exp:e2|]),
               (var ">#",   binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 > $exp:e2|]),
               (var "&&#",  binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 && $exp:e2|]),
               (var "||#",  binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 || $exp:e2|]),
               (var "+#",   binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 + $exp:e2|]),
               (var "-#",   binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 - $exp:e2|]),
               (var "*#",   binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 * $exp:e2|]),
               (var "/#",   binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 / $exp:e2|]),

               (var "==",  binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 == $exp:e2|]),
               (var "/=",  binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 != $exp:e2|]),
               (var "<",   binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 < $exp:e2|]),
               (var "<=",  binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 <= $exp:e2|]),
               (var ">=",  binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 >= $exp:e2|]),
               (var ">",   binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 > $exp:e2|]),
               (var "&&",  binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 && $exp:e2|]),
               (var "||",  binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 || $exp:e2|]),
               (var "+",   binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 + $exp:e2|]),
               (var "-",   binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 - $exp:e2|]),
               (var "*",   binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 * $exp:e2|]),
               (var "/",   binaryFun $ \e1 e2 -> return [$cexp|$exp:e1 / $exp:e2|]),

               (var "seq", cseq),

               (var "undefined",  cundefined),
               (var "error",      cerror)]
  where
    var :: String -> F.Var
    var s = F.Var $ mkName s

unaryFun ::  MonadToC r m => (Exp -> m Exp) -> F.Type -> [F.Exp] -> m CExp
unaryFun  f  ty  [x]   = do  e_x <- transExp Nothing x >>= lower >>= concrete
                             return CLowered `ap` return ty `ap` f e_x
unaryFun  _  _   args  = fail $ "expected 1 argument but got " ++ show (length args)

binaryFun ::  MonadToC r m => (Exp -> Exp -> m Exp) -> F.Type -> [F.Exp] -> m CExp
binaryFun  f  ty  [x, y]  = do  e_x <- transExp Nothing x >>= lower >>= concrete
                                e_y <- transExp Nothing y >>= lower >>= concrete
                                return CLowered `ap` return ty `ap` f e_x e_y
binaryFun  _  _   args    = fail $ "expected 2 arguments but got " ++ show (length args)

cerror ::  MonadToC r m => F.Type -> [F.Exp] -> m CExp
cerror  ty  [x]   = do  e <- transExp Nothing x >>= lower >>= concrete
                        cty <- transType ty
                        addCStm [$cstm|error($exp:e);|]
                        return $ CLowered ty [$cexp|*(($ty:cty *) NULL)|]
cerror  _   args  = fail $ "expected 1 argument but got " ++ show (length args)

cundefined ::  MonadToC r m => F.Type -> [F.Exp] -> m CExp
cundefined  ty  []    = do  cty <- transType ty
                            addCStm [$cstm|undefined();|]
                            return $ CLowered ty [$cexp|*(($ty:cty *) NULL)|]
cundefined  _   args  = fail $ "expected 0 arguments but got " ++ show (length args)

isUnitExp :: F.Exp -> Bool
isUnitExp (F.ConExp (F.TupleCon 0) _)  = True
isUnitExp _                            = False

isUnitTy :: F.Type -> Bool
isUnitTy (F.TyConTy (F.TupleTyCon 0) _)  = True
isUnitTy _                               = False

cseq ::  MonadToC r m => F.Type -> [F.Exp] -> m CExp
cseq ty  [e1, e2]
    | isUnitExp e1 && isUnitExp e2  = return $ CLowered ty $ [$cexp|NULL|]
    | isUnitExp e1                  = do  ce2 <- transExp Nothing e2 >>= lower >>= concrete
                                          return $ CLowered ty $ [$cexp|$exp:ce2|]
    | isUnitExp e2                  = do  ce1 <- transExp Nothing e1 >>= lower >>= concrete
                                          return $ CLowered ty $ [$cexp|$exp:ce1|]
    | otherwise                     = do  ce1 <- transExp Nothing e1 >>= lower >>= concrete
                                          ce2 <- transExp Nothing e2 >>= lower >>= concrete
                                          return $ CLowered ty $ [$cexp|$exp:ce1, $exp:ce2|]

cseq _   args    = fail $ "seq 2 arguments but got " ++ show (length args)
\end{code}

\subsection{Representing C Expressions}

\begin{code}
data CExp  =  CExp F.Type Exp
           |  CLowered F.Type Exp
           |  CPtr F.Type CExp
           |  CData F.Type CExp CExp
           |  CUnboxedData F.Type [CExp]

instance Pretty CExp where
    ppr (CExp _ e)                 = text "CExp" <+> ppr e
    ppr (CLowered _ e)             = text "CLowered" <+> ppr e
    ppr (CPtr _ ce)                = text "CPtr" <+> ppr ce
    ppr (CData _ ce_tag ce_union)  = text "CData" <+> ppr ce_tag <+> ppr ce_union
    ppr (CUnboxedData _ ces)       = text "CUnboxedData" <+> ppr ces
\end{code}

\begin{code}
cexpTy :: CExp -> F.Type
cexpTy  (CExp ty _)          = ty
cexpTy  (CLowered ty _)      = ty
cexpTy  (CPtr ty _)          = ty
cexpTy  (CData ty _ _)       = ty
cexpTy  (CUnboxedData ty _)  = ty

appCExp :: MonadToC r m => CExp -> CExp -> m CExp
appCExp ce1 ce2 =
    do  (_, to_ty) <- case cexpTy ce1 of
                        (F.AppTy (F.AppTy (F.TyConTy (F.TyCon tycon) _) ty1 _) ty2 _)
                            | tycon == builtinArrow -> return (ty1, ty2)
                        _ -> fail $ "appCExp: non-function type: " ++ show ce1 ++ ": " ++ show (cexpTy ce1)
        args <- flattenArgs ce2
        case ce1 of
          _ ->
              do  e1 <- concrete ce1
                  return $ CExp to_ty [$cexp|$exp:e1($args:args)|]

isLowered :: Exp -> Bool
isLowered  (Var _ _)          = True
isLowered  (Const _ _)        = True
isLowered  (UnOp _ e _)       = isLowered e
isLowered  (SizeofExp _ _)    = True
isLowered  (SizeofType _ _)   = True
isLowered  (Cast _ e _)       = isLowered e
isLowered  (Member e _ _)     = isLowered e
isLowered  (PtrMember e _ _)  = isLowered e
isLowered  _                  = False

concrete :: MonadToC r m => CExp -> m Exp
concrete  (CExp _ ce)      =  return ce
concrete  (CLowered _ ce)  =  return ce
concrete  (CPtr _ e)       =  do  ce <- concrete e
                                  return [$cexp|*$exp:ce|]

concrete (CData ty ce_tag ce_union) =
    do  temp     <- transType ty >>= ctemp
        e_tag    <- concrete ce_tag
        e_union  <- concrete ce_union
        addCStm [$cstm|$id:temp.tag = $exp:e_tag;|]
        addCStm [$cstm|$id:temp.u = $exp:e_union;|]
        return [$cexp|$id:temp|]

concrete (CUnboxedData tau ces) =
    if (isUnitTy tau)
    then do  mapM_ concrete ces
             return [$cexp|NULL|]
    else do  temp        <- transType tau >>= ctemp
             (tycon, _)  <- unpackTyCon tau
             cons        <- F.Tc.constructors tycon >>= filterM (liftM not . isSingletonCon tau)
             case cons of
               [con] ->  do  sigmas      <- conArgTys tau con
                             es          <- mapM concrete ces
                             n_con       <- mangle con
                             lbls        <- conArgLabels con
                             is_single_member_con <- isSingleMemberCon tau con
                             forM (zip3 lbls sigmas es) $ \(lbl, sigma, e) ->
                                 do  is_singleton <- isSingletonTy sigma
                                     when (not is_singleton) $
                                        if is_single_member_con
                                        then addCStm [$cstm|$id:temp.$id:n_con = $exp:e;|]
                                        else addCStm [$cstm|$id:temp.$id:lbl = $exp:e;|]
                             return [$cexp|$id:temp|]
               _     ->  do  mapM_ concrete ces
                             return [$cexp|VOID|]

lower :: MonadToC r m => CExp -> m CExp
lower (CExp ty e)
    | ty == F.Tc.unitTy  = return $ CLowered ty e
    | otherwise               = do  temp <- transType ty >>= ctemp
                                    addCStm [$cstm|$id:temp = $exp:e;|]
                                    return $ CLowered ty [$cexp|$id:temp|]

lower e@(CLowered _ _)            =  return e
lower e@(CPtr _ (CLowered _ _))   =  return e
lower (CPtr ty ce)                =  return (CPtr ty) `ap` lower ce
lower (CData ty ce_tag ce_union)  =  return (CData ty) `ap`
                                     lower ce_tag `ap`
                                     lower ce_union
lower (CUnboxedData ty ces)       =  return (CUnboxedData ty) `ap`
                                     mapM lower ces

\end{code}

\begin{code}
cshow :: MonadToC r m => F.Type -> CExp -> m (String, [Exp])
cshow (F.TyConTy (F.TyCon n) _) ce
    | n == prelInteger  = do  e <- concrete ce
                              return ("%d", [e])
    | n == prelChar     = do  e <- concrete ce
                              return ("%c", [e])
    | n == prelBool     = do  e <- concrete ce
                              return ("%s", [[$cexp|$exp:e ? "True" : "False"|]])
cshow _                   _    = return ("", [])

cprint :: MonadToC r m => F.Type -> CExp -> m Stm
cprint ty ce =
    do (stm, _) <-  newScope False $
                    do  (s, es) <- cshow ty ce
                        addCStm [$cstm|printf($string:s, $args:es);|]
       return stm
\end{code}

\begin{code}
isSingletonTy :: MonadToC r m => F.Type -> m Bool
isSingletonTy ty  =
    case (F.unfoldAppTy . snd . F.unfoldForAll) ty of
      (F.TyConTy tycon _ : _) | not (isLitTy ty)  -> isSingletonTyCon ty tycon
      _                                           -> return False

isSingletonTyCon :: MonadToC r m => F.Type -> F.TyCon -> m Bool
isSingletonTyCon ty tycon =
    do  cons <- F.Tc.constructors tycon
        case cons of
          []     -> return False
          [con]  -> isSingletonCon ty con
          _      -> return False

isSingletonCon :: MonadToC r m => F.Type -> F.Con -> m Bool
isSingletonCon ty con =
    do  (_, tyconargs) <- unpackTyCon ty
        tau_con <- F.Tc.lookupCon con
        (abinds, _, sigmas, _) <- F.Tc.unpackConTy con tau_con
        extendTyVarTys (map fst abinds `zip` tyconargs) $
          return and `ap` mapM (\tau -> compress tau >>= isSingletonTy) sigmas
\end{code}

\begin{code}
unpackTyCon :: MonadToC r m => F.Type -> m (F.TyCon, [F.Type])
unpackTyCon ty =
    case (F.unfoldAppTy . snd . F.unfoldForAll) ty of
      F.TyConTy tycon _ : args -> return (tycon, args)
      _ -> fail "internal error: unpackTyCon applied to non-data type"

conArgTys :: MonadToC r m => F.Type -> F.Con -> m [F.Type]
conArgTys ty con =
    do  (_, tyconargs) <- unpackTyCon ty
        ty_con <- F.Tc.lookupCon con
        (abinds, _, sigmas, _) <- F.Tc.unpackConTy con ty_con
        extendTyVarTys (map fst abinds `zip` tyconargs) $
            mapM compress sigmas

conArgLabels :: MonadToC r m => F.Con -> m [String]
conArgLabels con =
    do  lbls <- F.Tc.labels con
        case lbls of
          []  -> do  arity <- F.Tc.conArity con
                     return $ map (\i -> "element" ++ show i) [1..arity]
          _   -> mapM mangle lbls
\end{code}

\begin{code}
isSingleMemberCon :: MonadToC r m => F.Type -> F.Con -> m Bool
isSingleMemberCon ty con =
    do  (_, tyconargs) <- unpackTyCon ty
        tau_con <- F.Tc.lookupCon con
        (abinds, _, sigmas, _) <- F.Tc.unpackConTy con tau_con
        extendTyVarTys (map fst abinds `zip` tyconargs) $
          do  non_singleton_sigmas <- filterM (\tau -> compress tau >>= liftM not . isSingletonTy) sigmas
              return $ length non_singleton_sigmas == 1

isSingleMemberTyCon :: MonadToC r m => F.Type -> F.TyCon -> m Bool
isSingleMemberTyCon ty tycon =
    do  cons <- F.Tc.constructors tycon
        non_singleton_cons <- filterM (liftM not . isSingletonCon ty) cons
        return $ length non_singleton_cons == 1

isUnboxedTyCon :: MonadToC r m => F.TyCon -> m Bool
isUnboxedTyCon tycon =
    do  cons <- F.Tc.constructors tycon
        return $ length cons == 1
\end{code}

\begin{code}
dataTag :: MonadToC r m => CExp -> m Exp
dataTag (CData _ ce_tag _)  =  concrete ce_tag
dataTag (CUnboxedData _ _)  =  fail "internal error: dataTag applied to unboxed type"
dataTag (CPtr _ ce) =
    do  e <- concrete ce
        return [$cexp|$exp:e->tag|]
dataTag ce =
    do  e <- concrete ce
        return [$cexp|$exp:e.tag|]

dataUnion :: MonadToC r m => CExp -> m Exp
dataUnion (CData _ _ ce_union)  =  concrete ce_union
dataUnion (CUnboxedData _ _)    =  fail "internal error: dataUnion applied to unboxed type"
dataUnion (CPtr _ ce) =
    do  e <- concrete ce
        return [$cexp|$exp:e->u|]
dataUnion ce =
    do  e <- concrete ce
        return [$cexp|$exp:e.u|]

dataMembers :: MonadToC r m => F.Con -> CExp -> m [CExp]
dataMembers _ (CUnboxedData _ ces) =
    return ces

dataMembers con ce =
    do  tycon   <- F.Tc.tycon con
        tau     <- compress (cexpTy ce)
        sigmas  <- conArgTys tau con
        n_con   <- mangle con
        lbls    <- conArgLabels con
        is_single_member_tycon  <- isSingleMemberTyCon tau tycon
        is_single_member_con    <- isSingleMemberCon tau con
        forM (lbls `zip` sigmas) $ \(lbl, sigma) ->
            do  is_singleton <- isSingletonTy sigma
                return (CLowered sigma) `ap`
                  if is_singleton
                  then return [$cexp|NULL|]
                  else if is_single_member_tycon
                       then  do  e <- concrete ce
                                 if is_single_member_con
                                   then return [$cexp|$exp:e.$id:n_con|]
                                   else return [$cexp|$exp:e.$id:lbl|]
                       else  do  e_union <- dataUnion ce
                                 if is_single_member_con
                                   then return [$cexp|$exp:e_union.$id:n_con|]
                                   else return [$cexp|$exp:e_union.$id:n_con.$id:lbl|]
\end{code}

\begin{code}
flattenParams :: MonadToC r m => F.Type -> m ([Param], CExp)
flattenParams from_ty =
    do  optc <- optVal (isFlagSet Opt_OptC)
        if optc
          then do  (_, params, [ce]) <- flatten (1, [], []) from_ty
                   return (reverse params, ce)
          else if isUnitTy from_ty
               then return ([], CLowered from_ty [$cexp|NULL|])
               else do  cty <- transType from_ty
                        return ([[$cparam|$ty:cty arg|]], CLowered from_ty [$cexp|arg|])
  where
    flatten  ::  MonadToC r m
             =>  (Int, [Param], [CExp])
             ->  F.Type
             ->  m (Int, [Param], [CExp])
    flatten (i, params, ces) ty =
        case (F.unfoldAppTy . snd . F.unfoldForAll) ty of
          (F.TyConTy tycon _ : _) | not (isLitTy ty) ->
              do  is_unboxed <- isUnboxedTyCon tycon
                  if is_unboxed
                    then do  [con]   <- F.Tc.constructors tycon
                             sigmas  <- conArgTys ty con
                             (i', params', ces') <- foldM flatten (i, params, []) sigmas
                             return (i', params', CUnboxedData ty (reverse ces') : ces)
                    else do  cty <- transType ty
                             let n_arg = "arg" ++ show i
                             if isPtr cty
                                then  return (i + 1,
                                              [$cparam|$ty:cty $id:n_arg|] : params,
                                              CLowered ty [$cexp|$id:n_arg|] : ces)
                                else  return (i + 1,
                                              [$cparam|$ty:cty *$id:n_arg|] : params,
                                              CPtr ty (CLowered ty [$cexp|$id:n_arg|]) : ces)
          _ -> do  cty <- transType ty
                   let n_arg = "arg" ++ show i
                   return (i + 1,
                           [$cparam|$ty:cty $id:n_arg|] : params,
                           CLowered ty [$cexp|$id:n_arg|] : ces)
\end{code}

\begin{code}
flattenArgs :: MonadToC r m => CExp -> m [Exp]
flattenArgs ce =
    do  optc <- optVal (isFlagSet Opt_OptC)
        if optc
          then do  (_, args) <- flatten (1, []) ce
                   return (reverse args)
          else if isUnitTy (cexpTy ce)
               then return []
               else do  e <- lower ce >>= concrete
                        return [e]
  where
    flatten  ::  MonadToC r m
             =>  (Int, [Exp])
             ->  CExp
             ->  m (Int, [Exp])
    flatten (i, args) ce =
        if  isUnitTy (cexpTy ce)
        then  do  e <- lower ce >>= concrete
                  addCStm [$cstm|$exp:e;|]
                  return (i, args)
        else  do  let ty = cexpTy ce
                  ce' <- lower ce
                  case (F.unfoldAppTy . snd . F.unfoldForAll) ty of
                    (F.TyConTy tycon _ : _) | not (isLitTy ty) ->
                        do  is_unboxed <- isUnboxedTyCon tycon
                            if is_unboxed
                              then do  [con] <- F.Tc.constructors tycon
                                       members <- dataMembers con ce'
                                       (i', args') <- foldM flatten (i, args) members
                                       return (i', args')
                              else do  cty <- transType ty
                                       e <- concrete ce'
                                       if isPtr cty
                                          then  return (i + 1, [$cexp|$exp:e|] : args)
                                          else  return (i + 1, [$cexp|&$exp:e|] : args)
                    _ -> do  e <- concrete ce'
                             return (i + 1, e : args)
\end{code}

\subsection{Name Mangling}

\begin{code}
fixCident :: String -> String
fixCident s@(c : _) | isDigit c  = '_' : s
fixCident s                      = s

class Mangle a where
    mangle :: MonadToC r m => a -> m String

instance Mangle a => Mangle [a] where
    mangle as = return concat `ap` mapM mangle as

instance Mangle Char where
    mangle c@'_'        = return [c]
    mangle c
        | isAlphaNum c  = return [c]
        | otherwise     = return $ '_' : showHex (ord c) ""

instance Mangle Name where
    mangle n = do
        ss' <- mapM mangle ss
        return $ concat $ intersperse "+" ss'
      where
        ss :: [String]
        ss = modules (nameSort n) ++ [nameString n]

        modules :: NameSort -> [String]
        modules  (External (ModuleName is))  = splitOn '.' (istringString is)
        modules  (Qual (ModuleName is))      = splitOn '.' (istringString is)
        modules  _                           = []

        splitOn :: Eq a => a -> [a] -> [[a]]
        splitOn c xs =
            case span (/= c) xs of
              (xs, [])        -> [xs]
              (xs, _ : rest)  -> xs : splitOn c rest

instance Mangle F.TyCon where
    mangle (F.TyCon n)
        | nameString n == "Bool"     = return "b"
        | nameString n == "Char"     = return "c"
        | nameString n == "Integer"  = return "i"
        | nameString n == "[]"       = return "l"
        | otherwise                = mangle n
    mangle (F.TupleTyCon 0)      = return $ "u"
    mangle (F.TupleTyCon arity)  = return $ "t" ++ show arity

instance Mangle F.Var where
    mangle (F.Var n) = mangle n

instance Mangle F.Con where
    mangle (F.Con n)           = mangle n
    mangle (F.TupleCon 0)      = return $ "u"
    mangle (F.TupleCon arity)  = return $ "t" ++ show arity

instance Mangle F.Type where
    mangle (F.TyConTy tycon _)         =  mangle tycon
    mangle (F.TyVarTy (F.TyVar n) _)   =  fail $
                                          "unbound type variable `"
                                          ++ show n
                                          ++ "'"
    mangle ty@(F.AppTy _ _ _) =
        case F.unfoldFunTy ty of
          []   -> fail $ "internal error: cannot mangle type " ++ show ty
          [_]  -> do  ss <- mapM mangle $ F.unfoldAppTy ty
                      return $ concat $ intersperse "_" ss
          tys  -> do  ss <- mapM mangle tys
                      return $ concat $ intersperse "to" ss
    mangle ty                  = fail $ "internal error: cannot mangle type " ++ show ty
\end{code}

\subsection{Translation}

\begin{code}
compress :: MonadToC r m => F.Type -> m F.Type
compress ty@(F.TyConTy _ _)             = return ty
compress (F.TyVarTy tv _)               = lookupTyVarTy tv >>= compress
compress (F.AppTy ty1 ty2 sloc)         = return F.AppTy `ap` compress ty1 `ap` compress ty2 `ap` return sloc
compress (F.AppTyFunTy tyfun tys sloc)  = return (F.AppTyFunTy tyfun) `ap` mapM compress tys `ap` return sloc
compress (F.ForAll wtv _ ty _)          = bindWildTyVar wtv $ compress ty
compress (F.SymCo ty sloc)              = return F.SymCo `ap` compress ty `ap` return sloc
compress (F.TransCo ty1 ty2 sloc)       = return F.TransCo `ap` compress ty1 `ap` compress ty2 `ap` return sloc
compress (F.AppCo ty1 ty2 sloc)         = return F.AppCo `ap` compress ty1 `ap` compress ty2 `ap` return sloc
compress (F.LeftCo ty sloc)             = return F.LeftCo `ap` compress ty `ap` return sloc
compress (F.RightCo ty sloc)            = return F.RightCo `ap` compress ty `ap` return sloc
\end{code}

\begin{code}
transType :: MonadToC r m => F.Type -> m Type
transType ty =
    do  ty <- compress ty
        maybe_cty <- lookupCType ty
        case maybe_cty of
          Just cty  -> return cty
          Nothing   -> do  tracetoc $ text "translating type:" <+> ppr ty
                           cty <- transTy ty
                           insertCType ty cty
                           return cty
  where
    transTy :: MonadToC r m => F.Type -> m Type
    transTy ty@(F.TyConTy _ _)  = transTyConTy ty []
    transTy (F.TyVarTy tv _)    = fail $ "internal error: cannot translate type variable " ++ show tv
    transTy ty@(F.AppTy _ _ _)  =
        case ty of
          (F.AppTy (F.AppTy (F.TyConTy (F.TyCon tycon) _) ty1 _) ty2 _)
              | tycon == builtinArrow ->
                  do  cty1 <- transType ty1
                      cty2 <- transType ty2
                      -- cty_name <-  return (concat . intersperse "to") `ap`
                      --              mapM mangle [ty1, ty2]
                      -- addCDecldef [$cedecl|typedef $ty:cty2 (*$id:cty_name)($ty:cty1);|]
                      -- return [$cty|typename $id:cty_name|]
                      return [$cty|$ty:cty2 (*)($ty:cty1)|]
          _ ->
              do  let (tyconty : tys) = F.unfoldAppTy ty
                  transTyConTy tyconty tys

    transTy ty = fail $ "internal error: cannot translate type " ++ show ty

    transTyConTy :: MonadToC r m => F.Type -> [F.Type] -> m Type
    transTyConTy (F.TyConTy (F.TupleTyCon 0) _) _ =
        return [$cty|void|]

    transTyConTy (F.TyConTy tycon sloc) tyargs =
        do  n_ty  <-  return (concat . intersperse "_") `ap`
                      mapM mangle (F.TyConTy tycon sloc : tyargs)
            cons  <-  F.Tc.constructors tycon
            case cons of
              [con]  ->  do  flds <- conFields tyargs con
                             case flds of
                               []  -> return ()
                               _   -> addCDecldef [$cedecl|typedef struct $id:n_ty {
                                                               $sdecls:flds
                                                           } $id:n_ty;|]
              _      ->  do  transTyConEnum tycon cons
                             n_tycon     <-  mangle tycon
                             let n_enum  =   n_tycon ++ "_tag"
                             members <- return catMaybes `ap` mapM (conStruct tyargs) cons
                             case members of
                               []           -> addCDecldef [$cedecl|typedef struct $id:n_ty {
                                                                        typename $id:n_enum tag;
                                                                    } $id:n_ty;|]
                               [(_, flds)]  -> addCDecldef [$cedecl|typedef struct $id:n_ty {
                                                                        typename $id:n_enum tag;
                                                                        $sdecls:flds
                                                                    } $id:n_ty;|]
                               _           -> let structs = map membersToStruct members
                                              in
                                                addCDecldef [$cedecl|typedef struct $id:n_ty {
                                                                         typename $id:n_enum tag;
                                                                         union { $sdecls:structs } u;
                                                                     } $id:n_ty;|]
            defs <- getCDefs
            tracetoc $ text "defs:" <+> ppr defs
            return [$cty|typename $id:n_ty|]
      where
        membersToStruct :: (String, [FieldGroup]) -> FieldGroup
        membersToStruct (_,      [fld])  = fld
        membersToStruct (n_con,  flds)   = [$csdecl|struct $id:n_con {
                                                        $sdecls:flds
                                                    } $id:n_con;|]

        transTyConEnum :: MonadToC r m => F.TyCon -> [F.Con] -> m ()
        transTyConEnum tycon cons =
            do  hasEnum <- memberCEnum tycon
                when (not hasEnum) $
                    do  n_tycon           <-  mangle tycon
                        let n_enum        =   n_tycon ++ "_tag"
                        (cname : cnames)  <-  mapM mangle cons
                        tracetoc $ text "translating tycon:" <+> ppr tycon
                        let enums = map (\n -> [$cenum|$id:n|]) cnames
                        addCDecldef [$cedecl|typedef enum $id:n_enum {
                                                 $id:cname = 0,
                                                 $enums:enums
                                             } $id:n_enum;|]
                        insertCEnum tycon

        conFields :: MonadToC r m => [F.Type] -> F.Con -> m [FieldGroup]
        conFields tyargs con =
            do  lbls   <- conArgLabels con
                tracetoc $ text "con labels:" <+> ppr con <+> ppr lbls
                (abinds, _, sigmas, _)  <-  F.Tc.lookupCon con >>= F.Tc.unpackConTy con
                extendTyVarTys (map fst abinds `zip` tyargs) $
                    do  sigmas' <- mapM compress sigmas
                        tracetoc $ text "con fields:" <+> ppr (lbls `zip` sigmas')
                        fields <- filterM (\(_, ty) -> return not `ap` isSingletonTy ty) $ lbls `zip` sigmas'
                        tracetoc $ text "con fields:" <+> ppr fields
                        case fields of
                          []           ->  return []
                          [(_, ty)]    ->  do  n_con  <- mangle con
                                               cty <- transType ty
                                               return [[$csdecl|$ty:cty $id:n_con;|]]
                          _            ->  mapM  (\(lbl, ty) -> do  cty <- transType ty
                                                                    return [$csdecl|$ty:cty $id:lbl;|])
                                                 fields

        conStruct :: MonadToC r m => [F.Type] -> F.Con -> m (Maybe (String, [FieldGroup]))
        conStruct tyargs con =
            do  flds <- conFields tyargs con
                tracetoc $ text "conFields:" <+> ppr con <+> ppr flds
                case flds of
                  []     ->  return $ Nothing
                  [fld]  ->  return $ Just (error "single field in struct", [fld])
                  flds   ->  do  n_con  <- mangle con
                                 return $ Just (n_con, flds)

    transTyConTy ty _ = fail $ "internal error: transTyConTy applied to non-type constructor type " ++ show ty
\end{code}

\begin{code}
transDecls :: MonadToC r m => [F.Decl] -> m ()
transDecls decls =
    do  mapM_ F.Tc.checkTyConDecl decls
        mapM_ F.Tc.checkTypeDecl decls
        mapM_ F.Tc.checkSigDecl decls
        (stm, (tcenv, tocenv)) <- newScope True $ transBindingDecls decls
        F.Tc.putTcEnv tcenv
        putToCEnv tocenv
        tracetoc $ text "transDecls" <+> ppr stm
        addCInitStm stm
  where
    transBindingDecls :: MonadToC r m => [F.Decl] -> m (F.Tc.TcEnv, ToCEnv m)
    transBindingDecls  [] =
        do  tcenv   <- F.Tc.getTcEnv
            tocenv  <- getToCEnv
            return (tcenv, tocenv)

    transBindingDecls  (F.LetDecl (NonRec b) _ : decls) =
        extendCVars [] $ do
        transBinding b
        F.Tc.extendVars [F.boundVar b] $
            transBindingDecls decls

    transBindingDecls  (F.LetDecl (Rec bs) _ : decls) =
        transBindings bs $ transBindingDecls decls

    transBindingDecls  (_ : decls) =
        transBindingDecls decls
\end{code}

\begin{code}
transBindings :: MonadToC r m => [F.Binding] -> m a -> m a
transBindings bindings m =
    do  tracetoc $ text "translating bindings" <+> stack (map ppr bindings)
        F.Tc.extendVars (map F.boundVar bindings) $
            mapM_ transBinding bindings >> m
\end{code}

\begin{code}
transBinding :: MonadToC r m => F.Binding -> m ()
transBinding (F.Binding v ty _ e _) =
    do  tracetoc $ text "translating binding" <+> ppr v <+> text "::" <+> ppr ty
        top <- isTopContext
        ref <- newRef Map.empty
        let me = if F.isFunTy ty
                 then compress ty >>= memo ref (transFun top)
                 else compress ty >>= memo ref (transVar top)
        insertCVar v me
  where
    memo  ::  MonadToC r m
          =>  r (Map.Map F.Type CExp)
          ->  (String -> F.Type -> m CExp)
          ->  F.Type
          ->  m CExp
    memo ref f ty =
        do  tracetoc $ text "translating binding" <+> ppr v <+> text "at type" <+> ppr ty
            mp <- readRef ref
            case Map.lookup ty mp of
              Just b   -> do  tracetoc $ text " binding" <+> ppr v <+> text "cached"
                              return b
              Nothing  -> do  tracetoc $ text " binding" <+> ppr v <+> text "not cached"
                              cname <-  do  vname   <- mangle v
                                            tyname  <- mangle ty
                                            gensym $ vname ++ "_" ++ tyname
                              let ce = CLowered ty [$cexp|$id:cname|]
                              writeRef ref $ Map.insert ty ce mp
                              b <- f cname ty
                              writeRef ref $ Map.insert ty b mp
                              return b

    transVar :: MonadToC r m => Bool -> String -> F.Type -> m CExp
    transVar top cname ty =
        do  tracetoc $   text "translating variable" <+> ppr v
                     <+> text "at type" <+> ppr ty
                     <+> text "as" <+> ppr cname
            cty      <-  transType ty
            (stm, _) <-  newScope True $
                         do  e <- transExp (Just cname) e >>= concrete
                             addCStm [$cstm|$id:cname = $exp:e;|]
            tracetoc $  text "translating variable" <+> ppr v <+> ppr stm
            if top
              then do  addCDecldef [$cedecl|$ty:cty $id:cname;|]
                       addCInitStm stm
              else do  addCDecl [$cdecl|$ty:cty $id:cname;|]
                       addCStm stm
            return $ CLowered ty [$cexp|$id:cname|]

    transFun :: MonadToC r m => Bool -> String -> F.Type -> m CExp
    transFun _ cname _ =
        do  tracetoc $  text "translating function" <+> ppr v
                     <+> text "at type" <+> ppr ty
                     <+> text "as" <+> ppr cname
            transExp (Just cname) e
\end{code}

\begin{code}
transLit :: MonadToC r m => F.Lit -> m CExp
transLit (F.IntegerLit i)    = return $ CLowered F.Tc.integerTy [$cexp|$int:i|]
transLit (F.FloatLit f)      = return $ CLowered F.Tc.floatTy [$cexp|$double:f|]
transLit (F.CharLit c)       = return $ CLowered F.Tc.charTy [$cexp|$char:c|]
transLit (F.StringLit s)     = return $ CLowered F.Tc.stringTy [$cexp|$string:s|]

transExp :: MonadToC r m => Maybe String -> F.Exp -> m CExp
transExp _ exp@(F.LitExp lit _) =
    traceNest $
    do  tracetoc $ text "translating expression:" <+> ppr exp
        transLit lit

transExp _ exp@(F.VarExp v _) =
    traceNest $
    do  tracetoc $ text "translating expression:" <+> ppr exp
        mce  <- lookupCVar v
        mce

transExp _ exp@(F.ConExp con _) =
    traceNest $
    do  tracetoc $ text "translating expression:" <+> ppr exp
        ty <- tcExp exp
        transConExp con [] ty
\end{code}

\begin{code}
transExp maybe_fname exp@(F.LamExp v ty e _) =
    traceNest $
    do  tracetoc $ text "translating expression:" <+> ppr exp
        fname     <-  case maybe_fname of
                        Just fname  -> return fname
                        Nothing     -> gensym "anon"
        from_ty <- compress ty
        (params, ce_param) <- flattenParams from_ty
        (stm, (to_ty, e)) <-  newScope True $
                              F.Tc.extendVars [(v, from_ty)] $
                              extendCVars [(v, return ce_param)] $
                              do  ce <- transExp Nothing e
                                  e' <- concrete ce
                                  return (cexpTy ce, e')
        to_cty <- transType to_ty
        addCDecldef [$cedecl|$ty:to_cty $id:fname($params:params);|]
        case stm of
          Block decls stms _ ->
              if to_ty == F.Tc.unitTy
              then addCVardef [$cedecl|$ty:to_cty $id:fname($params:params)
                                       { $decls:decls $stms:stms $exp:e; } |]
              else addCVardef [$cedecl|$ty:to_cty $id:fname($params:params)
                                       { $decls:decls $stms:stms return $exp:e; } |]
          _ ->  if to_ty == F.Tc.unitTy
                then addCVardef [$cedecl|$ty:to_cty $id:fname($params:params)
                                         { $stm:stm $exp:e; } |]
                else addCVardef [$cedecl|$ty:to_cty $id:fname($params:params)
                                         { $stm:stm return $exp:e; } |]
        return $ CLowered (from_ty F.--> to_ty) [$cexp|$id:fname|]
\end{code}

\begin{code}
transExp _ exp@(F.AppExp _ _ _) =
    traceNest $
    do  tracetoc $ text "translating application expression:" <+> ppr exp
        ty <- tcExp exp
        let es@(f_inst : args) = F.unfoldAppExp exp
        let (f, _) = F.unfoldTyAppExp f_inst
        case f of
          F.ConExp con _  ->  transConExp con args ty
          F.VarExp v _    ->  case lookup v builtinFuns of
                                Just f   ->  f ty args
                                Nothing  ->  transApp es
          _               ->  transApp es
  where
    transApp :: MonadToC r m => [F.Exp] -> m CExp
    transApp es =
        do  ce : ces <- mapM (transExp Nothing) es
            foldM appCExp ce ces
\end{code}

\begin{code}
transExp maybe_fname exp@(F.TyLamExp a kappa e _) =
    traceNest $
    do  tracetoc $ text "translating expression:" <+> ppr exp
        ty <- popTyAppTy
        F.Tc.extendTyVars [(a, kappa)] $
          extendTyVarTys [(a, ty)] $
          transExp maybe_fname (subst1 ty a e)

transExp maybe_fname exp@(F.TyAppExp e ty _) =
    traceNest $
    do  tracetoc $ text "translating expression:" <+> ppr exp
        pushTyAppTy ty
        transExp maybe_fname e

transExp _ exp@(F.LetExp (NonRec b) e _) =
    traceNest $
    do  tracetoc $ text "translating expression:" <+> ppr exp
        extendCVars [] $ F.Tc.extendVars [F.boundVar b] $ do
            transBinding b
            transExp Nothing e

transExp _ exp@(F.LetExp (Rec bs) e _) =
    traceNest $
    do  tracetoc $ text "translating expression:" <+> ppr exp
        transBindings bs $ transExp Nothing e

transExp _ exp@(F.CaseExp e v alts _) =
    traceNest $
    do  tracetoc $ text "translating expression:" <+> ppr exp
        scrut_ty  <-  tcExp e
        case_ty   <-  tcExp exp
        case_cty  <-  transType case_ty
        scrut_ce  <-  transExp Nothing e >>= lower
        result    <-  if isUnitTy case_ty
                      then return $ error "internal error: unit result is case branch"
                      else ctemp case_cty
        calts     <-  F.Tc.extendVars [(v, cexpTy scrut_ce)] $
                      extendCVars     [(v, return scrut_ce)] $
                      mapM (\(F.Alt p e) -> transAlt scrut_ce result p e) alts
        if isBoolTy scrut_ty
          then transBoolCase scrut_ce (alts `zip` calts)
          else if isLitTy scrut_ty
               then transLitCase calts
               else transDataCase scrut_ce scrut_ty calts
        if isUnitTy case_ty
          then return $ CLowered case_ty [$cexp|NULL|]
          else return $ CLowered case_ty [$cexp|$id:result|]
  where
    transBoolCase :: MonadToC r m => CExp -> [(F.Alt, Stm)] -> m ()
    transBoolCase scrut_ce alts =
        do  scrut_e <- concrete scrut_ce
            addCStm [$cstm|if ($exp:scrut_e) $stm:trueStm else $stm:falseStm|]
      where
        trueStm   = case filter trueAlt alts of
                      ((_, stm) : _)  -> stm
                      _               -> [$cstm|error("No match");|]

        falseStm  = case filter falseAlt alts of
                      ((_, stm) : _)  -> stm
                      _               -> [$cstm|error("No match");|]

        trueAlt :: (F.Alt, Stm) -> Bool
        trueAlt (F.Alt p _, _) =
            case p of
              F.ConPat con _ _ _  -> isTrue con
              _                   -> False

        falseAlt :: (F.Alt, Stm) -> Bool
        falseAlt (F.Alt p _, _) =
            case p of
              F.ConPat con _ _ _  -> isFalse con
              _                   -> False

    transLitCase :: MonadToC r m => [Stm] -> m ()
    transLitCase calts = addCStm $ foldr1 mergeStms calts
      where
        mergeStms :: Stm -> Stm -> Stm
        mergeStms (If teste thens Nothing sloc)  elses  = If teste thens (Just elses) sloc
        mergeStms stm                            _      = stm

    transDataCase :: MonadToC r m => CExp -> F.Type -> [Stm] -> m ()
    transDataCase scrut_ce scrut_ty calts =
        do  let F.TyConTy tycon _ = head $ F.unfoldAppTy $ snd $ F.unfoldForAll scrut_ty
            ncons <- return length `ap` F.Tc.constructors tycon
            if ncons == 1
              then addCStm $ head calts
              else do  e_tag <- dataTag scrut_ce
                       let calts' = intersperse [$cstm|break;|] calts
                       addCStm [$cstm|switch ($exp:e_tag) { $stms:calts' }|]


transExp _ exp@(F.CastExp _ _ _) =
    traceNest $
    do  tracetoc $ text "translating expression:" <+> ppr exp
        fail $ "cannot translate cast expression: " ++ show exp

transConExp :: MonadToC r m => F.Con -> [F.Exp] -> F.Type -> m CExp
transConExp con es ty
    | isTrue con   = return $ CLowered ty [$cexp|1|]
    | isFalse con  = return $ CLowered ty [$cexp|0|]
    | isUnit con   = return $ CLowered F.Tc.unitTy [$cexp|NULL|]
    | otherwise =
        traceNest $
        do  tracetoc $ text "translating constructor expression:" <+> ppr con <+> spread (map ppr es)
            arity <- F.Tc.conArity con
            when (length es /= arity) $
                throwException $ F.Tc.UnsaturatedConstructor con
            sigmas  <- conArgTys ty con
            tycon <- F.Tc.tycon con
            is_unboxed <- isUnboxedTyCon tycon
            if is_unboxed
               then do  es' <- mapM (transExp Nothing) es
                        return $ CUnboxedData ty es'
               else do  temp    <- transType ty >>= ctemp >>= \temp -> return (CLowered ty [$cexp|$id:temp|])
                        n_con   <- mangle con
                        ncons   <- return length `ap` (F.Tc.tycon con >>= F.Tc.constructors)
                        when (ncons > 1) $
                            dataTag temp >>= \tag -> addCStm [$cstm|$exp:tag = $id:n_con;|]
                        members <- dataMembers con temp >>= mapM concrete
                        args    <- mapM (\e -> transExp Nothing e >>= concrete) es
                        mapM_ assignMember (zip3 sigmas members args)
                        return temp
  where
    assignMember :: MonadToC r m => (F.Type, Exp, Exp) -> m ()
    assignMember (ty, mem, arg) =
        do  is_singleton <- compress ty >>= isSingletonTy
            if is_singleton
              then return ()
              else addCStm [$cstm|$exp:mem = $exp:arg;|]
\end{code}

\begin{code}
isTrue :: F.Con -> Bool
isTrue (F.Con n) | n == prelTrue  = True
isTrue _                          = False

isFalse :: F.Con -> Bool
isFalse (F.Con n) | n == prelFalse  = True
isFalse _                           = False

isUnit :: F.Con -> Bool
isUnit (F.TupleCon 0)  = True
isUnit _               = False

isBoolTy :: F.Type -> Bool
isBoolTy ty = ty === F.Tc.boolTy

isLitTy :: F.Type -> Bool
isLitTy ty =  (ty === F.Tc.boolTy) ||
              (ty === F.Tc.integerTy) ||
              (ty === F.Tc.floatTy) ||
              (ty === F.Tc.charTy) ||
              (ty === F.Tc.stringTy)

isStringTy :: F.Type -> Bool
isStringTy ty = ty === F.Tc.stringTy
\end{code}

\begin{code}
transAlt :: MonadToC r m => CExp -> String -> F.Pat -> F.Exp -> m Stm

transAlt scrut result p@(F.LitPat lit _) e =
    traceNest $
    do  tracetoc $ text "translating branch:" <+> ppr p <+> text "->" <+> ppr e
        scrut_e   <-  concrete scrut
        lit_e     <-  transLit lit >>= concrete
        (stm, _)  <-  newScope False $
                      do  rhs_e <- transExp Nothing e >>= concrete
                          addCStm [$cstm|$id:result = $exp:rhs_e;|]
        if isStringTy (cexpTy scrut)
          then return [$cstm|if (!strcmp($exp:scrut_e, $exp:lit_e)) $stm:stm|]
          else return [$cstm|if ($exp:scrut_e == $exp:lit_e) $stm:stm|]

transAlt scrut result p@(F.VarPat (wv, ty) _) e =
    traceNest $
    do  tracetoc $ text "translating branch:" <+> ppr p <+> text "->" <+> ppr e
        scrut_e   <-  lower scrut
        (stm, _)  <-  newScope False $
                      F.Tc.extendWildVars [(wv, ty)] $
                      extendWildCVars [(wv, return scrut_e)] $
                      do  rhs_e <- transExp Nothing e >>= concrete
                          addCStm [$cstm|$id:result = $exp:rhs_e;|]
        if isLitTy (cexpTy scrut)
          then return stm
          else return [$cstm|default: $stm:stm|]

transAlt scrut result p@(F.ConPat con _ wvtys _) e =
    traceNest $
    do  tracetoc $ text "translating branch:" <+> ppr p <+> text "->" <+> ppr e
        con_cname      <- mangle con
        (stm, _)  <-  newScope False $
                      F.Tc.extendWildVars wvtys $
                      do  members <- dataMembers con scrut
                          let wvcbinds = map (\((wv, ty), e) -> (wv, ty, e)) $
                                         wvtys `zip` members
                          extendWildCVars (map (\(wv, _, e) -> (wv, return e)) wvcbinds) $
                              do  rhs_ce  <- transExp Nothing e
                                  rhs_e   <- concrete rhs_ce
                                  if (isUnitTy (cexpTy rhs_ce))
                                    then addCStm [$cstm|$exp:rhs_e;|]
                                    else addCStm [$cstm|$id:result = $exp:rhs_e;|]
        ncons <- return length `ap` (F.Tc.tycon con >>= F.Tc.constructors)
        if isBoolTy (cexpTy scrut) || ncons == 1
          then return stm
          else return [$cstm|case $id:con_cname: $stm:stm|]
\end{code}
