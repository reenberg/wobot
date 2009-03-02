%if False
\begin{code}
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
-- Module      :  Check.F.Monad
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Check.F.Monad (
    TcEnv,
    emptyTcEnv,
    MonadTc(..)
  ) where

import Compiler.Opt
import Control.Monad.Error
import Control.Monad.Trace
import Control.Monad.Unique
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set

import Check.F.Builtin
import Check.F.Exceptions
import Control.Monad.ContextException
import Control.Monad.Exception
import Data.Loc
import Data.Name
import Language.F
import Text.PrettyPrint.Mainland
\end{code}
%endif

\subsection{The Type Checking Monad Class}

\begin{code}
data TcEnv = TcEnv  {  tycons     :: Map.Map  TyCon  Kind
                    ,  cons       :: Map.Map  Con    Type
                    ,  tyconCons  :: Map.Map  TyCon  [Con]
                    ,  conTycons  :: Map.Map  Con    TyCon
                    ,  conLabels  :: Map.Map  Con    [Var]

                    ,  tyvars         :: Map.Map  TyVar  Kind
                    ,  tyvarsInscope  :: Set.Set  TyVar
                    ,  tyfuns         :: Map.Map  TyFun  ([Kind], Kind)
                    ,  vars           :: Map.Map  Var    Type
                    ,  varsInscope    :: Set.Set  Var
                    }

emptyTcEnv :: TcEnv
emptyTcEnv = TcEnv  {  tycons         = Map.fromList builtinTyCons
                    ,  cons           = Map.fromList builtinConstructors
                    ,  tyconCons      = Map.fromList builtinTyConCons
                    ,  conTycons      = Map.fromList builtinConTyCons
                    ,  conLabels      = Map.empty

                    ,  tyvars         = Map.empty
                    ,  tyvarsInscope  = Set.empty
                    ,  tyfuns         = Map.empty
                    ,  vars           = Map.fromList builtinVars
                    ,  varsInscope    = Set.empty
                    }

class (MonadContextException m,
       MonadOpts m,
       MonadTrace m,
       MonadUnique m)
    => MonadTc m where
    getTcEnv   :: m TcEnv
    putTcEnv   :: TcEnv -> m ()

    getsTcEnv :: (TcEnv -> a) -> m a
    getsTcEnv f = getTcEnv >>= \s -> return (f s)

    modifyTcEnv :: (TcEnv -> TcEnv) -> m ()
    modifyTcEnv f = getTcEnv >>= \s -> putTcEnv (f s)

    resetTcEnv :: m ()
    resetTcEnv = putTcEnv emptyTcEnv

    savingTcEnv :: m a -> m a
    savingTcEnv m = do
        env  <- getTcEnv
        a    <- m
        putTcEnv env
        return a

    traceTc :: Doc -> m ()
    traceTc doc = do
        doTrace <- optVal (isFlagSet Opt_d_dump_ftc_trace)
        when doTrace $
            trace "traceFTc:" doc

    traceSimpl :: Doc -> m ()
    traceSimpl doc = do
        doTrace <- optVal (isFlagSet Opt_d_dump_simpl_trace)
        when doTrace $
            trace "traceSimpl:" doc

    lookupTyCon :: TyCon -> m Kind
    lookupTyCon (TupleTyCon n) =
        return $ foldr (:=>) (:*) (replicate n (:*))

    lookupTyCon tycon = do
        maybe_k <- getsTcEnv $ \s -> Map.lookup tycon (tycons s)
        case maybe_k of
          Nothing  ->  throwException $ UnboundTypeConstructor tycon
          Just k   ->  return k

    insertTyCon :: TyCon -> [Con] -> Kind -> m ()
    insertTyCon tycon cons kind = do
        modifyTcEnv $ \s ->
            s  {  tycons     = Map.insert tycon kind (tycons s)
               ,  tyconCons  = Map.insert  tycon cons (tyconCons s)
               ,  conTycons  = foldl'  (\m con -> Map.insert con tycon m)
                                       (conTycons s)
                                       cons
               }

    lookupCon :: Con -> m Type
    lookupCon (TupleCon n) = do
        let tvs          =  take n allTyVars
        let tycon        =  TupleTyCon n
        let tyvarTys     =  [TyVarTy tv internalLoc | tv <- tvs]
        let res_ty       =  appsT (TyConTy tycon internalLoc) tyvarTys
        let tuplecon_ty  =  forallsT (tvs `zip` repeat (:*)) $
                            foldr (-->) res_ty tyvarTys
        return tuplecon_ty

    lookupCon con = do
        maybe_sigma <- getsTcEnv $ \s -> Map.lookup con (cons s)
        case maybe_sigma of
          Nothing     ->  throwException $ UnboundConstructor con
          Just sigma  ->  return sigma

    insertCon :: Con -> Type -> [Var] -> m ()
    insertCon con ty labels =
        modifyTcEnv $ \s ->
            s  {  cons = Map.insert con ty (cons s)
               ,  conLabels = Map.insert con labels (conLabels s)
               }

    extendCons:: [(Con, Type, [Var])] -> m a -> m a
    extendCons bindings m = do
        old_cons       <- getsTcEnv cons
        old_conLabels  <- getsTcEnv conLabels
        forM bindings $ \(con, ty, labels) ->
            insertCon con ty labels
        a <- m
        modifyTcEnv $ \s ->
            s  {  cons       = old_cons
               ,  conLabels  = old_conLabels
               }
        return a

    constructors :: TyCon -> m [Con]
    constructors (TupleTyCon n) =
        return [TupleCon n]

    constructors tycon = do
        maybe_cons <- getsTcEnv $ \s -> Map.lookup tycon (tyconCons s)
        case maybe_cons of
          Nothing    ->  throwException $ UnboundTypeConstructor tycon
          Just cons  ->  return cons

    tycon :: Con -> m TyCon
    tycon (TupleCon n) =
        return $ TupleTyCon n

    tycon con  = do
        maybe_tycon <- getsTcEnv $ \s -> Map.lookup con (conTycons s)
        case maybe_tycon of
          Nothing     ->  throwException $ UnboundConstructor con
          Just tycon  ->  return tycon

    labels :: Con -> m [Var]
    labels con = do
        maybe_lbls <- getsTcEnv $ \s -> Map.lookup con (conLabels s)
        case maybe_lbls of
          Nothing    -> return []
          Just lbls  -> return lbls

    inscopeTyVars :: m (Set.Set TyVar)
    inscopeTyVars = getsTcEnv tyvarsInscope

    lookupTyVar :: TyVar -> m Kind
    lookupTyVar tyvar = do
        maybe_k <- getsTcEnv $ \s -> Map.lookup tyvar (tyvars s)
        case maybe_k of
          Nothing  -> throwException $ UnboundTypeVariable tyvar
          Just k   -> return k

    insertTyVar:: TyVar -> Kind -> m ()
    insertTyVar tyvar kind =
        modifyTcEnv $ \s ->
            s  {  tyvars         = Map.insert tyvar kind (tyvars s)
               ,  tyvarsInscope  = Set.insert tyvar (tyvarsInscope s)
               }

    extendTyVars :: [(TyVar, Kind)] -> m a -> m a
    extendTyVars binds m = do
        old_tyvars <- getsTcEnv tyvars
        old_tyvarsInscope <- getsTcEnv tyvarsInscope
        modifyTcEnv $ \s ->
            s  {  tyvars         = foldl'  (\phi (tv, k) -> Map.insert tv k phi)
                                           (tyvars s) binds
               ,  tyvarsInscope  = foldl'  (\phi (tv, _) -> Set.insert tv phi)
                                           (tyvarsInscope s) binds
               }
        a <- m
        modifyTcEnv $ \s -> s  {  tyvars         = old_tyvars
                               ,  tyvarsInscope  = old_tyvarsInscope
                               }
        return a

    extendWildTyVars  :: [(WildTyVar, Kind)] -> m a -> m a
    extendWildTyVars binds m =
        extendTyVars [(tv, kappa) | (TameTyVar tv, kappa) <- binds] m

    lookupTyFun :: TyFun -> m ([Kind], Kind)
    lookupTyFun tyfun = do
        maybe_ks <- getsTcEnv $ \s -> Map.lookup tyfun (tyfuns s)
        case maybe_ks of
          Nothing   -> throwException $ UnboundTypeFunction tyfun
          Just ks   -> return ks

    insertTyFun :: TyFun -> ([Kind], Kind) -> m ()
    insertTyFun tyfun kind =
        modifyTcEnv $ \s -> s { tyfuns = Map.insert tyfun kind (tyfuns s) }

    extendTyFuns :: [(TyFun, ([Kind], Kind))] -> m a -> m a
    extendTyFuns binds m = do
        old_tyfuns <- getsTcEnv tyfuns
        modifyTcEnv $ \s ->
            s  {  tyfuns = foldl'  (\phi (tyfun, kappa) -> Map.insert tyfun kappa phi)
                                   (tyfuns s) binds
               }
        a <- m
        modifyTcEnv $ \s -> s { tyfuns = old_tyfuns }
        return a

    isInscope :: Var -> m Bool
    isInscope v@(Var _) =
        getsTcEnv $ \s -> Set.member v (varsInscope s)

    inscopeVars :: m (Set.Set Var)
    inscopeVars = getsTcEnv varsInscope

    fresh :: Var -> m [Var]
    fresh v = do
        phi <- inscopeVars
        return (f phi)
      where
        f :: Set.Set Var -> [Var]
        f phi = v' : f (Set.insert v' phi)
          where
            v' = freshname phi v

    lookupVar :: Var -> m Type
    lookupVar v = do
        maybe_sigma <- getsTcEnv (\s -> Map.lookup v (vars s))
        case maybe_sigma of
          Nothing     ->  throwException $ UnboundVariable v
          Just sigma  ->  return sigma

    insertVar :: Var -> Type -> m ()
    insertVar v ty =
        modifyTcEnv $ \s -> s  {  vars         = Map.insert v ty (vars s)
                               ,  varsInscope  = Set.insert v (varsInscope s)
                               }

    extendVars :: [(Var, Type)] -> m a -> m a
    extendVars binds m = do
        old_vars <- getsTcEnv vars
        old_varsInscope <- getsTcEnv varsInscope
        modifyTcEnv $ \s ->
            s  {  vars         = foldl'  (\phi (v, tau) -> Map.insert v tau phi)
                                         (vars s) binds
               ,  varsInscope  = foldl'  (\phi (v, _) -> Set.insert v phi)
                                         (varsInscope s) binds
               }
        a <- m
        modifyTcEnv $ \s -> s  {  vars         = old_vars
                               ,  varsInscope  = old_varsInscope
                               }
        return a

    extendWildVars  :: [(WildVar, Type)] -> m a -> m a
    extendWildVars binds m = extendVars [(v, tau) | (TameVar v, tau) <- binds] m
\end{code}
