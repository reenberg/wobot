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
-- Module      :  Transform.F.Simplify.OccAnal
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Transform.F.Simplify.OccAnal (
    occDecls
  ) where

import Control.Monad (ap,
                      mapAndUnzipM)
import Data.List (foldl',
                  foldl1')
import qualified Data.Set as Set
import qualified Data.Map as Map

import Check.F
import Data.Name
import Language.F
import Text.PrettyPrint.Mainland
import Transform.F.Simplify.Metrics
\end{code}

\begin{code}
data Level = TopLevel | Nested
  deriving (Eq)
\end{code}

\begin{code}
data OccEnv = OccEnv
    {  var_occinfo  :: Map.Map Var OccInfo
    ,  var_scrut    :: Set.Set Var
    ,  var_args     :: [ArgInfo]
    }

occEmpty :: OccEnv
occEmpty = OccEnv
    {  var_occinfo  = Map.empty
    ,  var_scrut    = Set.empty
    ,  var_args     = []
    }

occSingleton :: Var -> OccInfo -> OccEnv
occSingleton v occinfo = OccEnv
    {  var_occinfo  = Map.singleton v occinfo
    ,  var_scrut    = Set.empty
    ,  var_args     = []
    }

occDelete :: Var -> OccEnv -> OccEnv
occDelete v env =
    env  {  var_occinfo  = Map.delete v (var_occinfo env)
         ,  var_scrut    = Set.delete v (var_scrut env)
         }

occDeleteMany :: [Var] -> OccEnv -> OccEnv
occDeleteMany vs env =
    env  {  var_occinfo  = foldl' (flip Map.delete) (var_occinfo env) vs
         ,  var_scrut    = foldl' (flip Set.delete) (var_scrut env) vs
         }

lookupOccInfo :: Var -> OccEnv -> Maybe OccInfo
lookupOccInfo v env =
    Map.lookup v (var_occinfo env)

lookupArgInfo :: Var -> OccEnv -> ArgInfo
lookupArgInfo v env =
    if v `Set.member` var_scrut env
    then Scrutinized
    else NoArgInfo

adjustOccInfo :: (OccInfo -> OccInfo) -> OccEnv -> OccEnv
adjustOccInfo f env =
    env { var_occinfo = Map.map f (var_occinfo env) }

scrutVar :: Var -> OccEnv -> OccEnv
scrutVar v env =
    env { var_scrut = Set.insert v (var_scrut env) }

scrutExp :: Exp -> OccEnv -> OccEnv
scrutExp e env = case getVar e of
                   Nothing ->  env
                   Just v ->   scrutVar v env
  where
    getVar :: Exp -> Maybe Var
    getVar  (VarExp v _)        = Just v
    getVar  (TyLamExp _ _ e _)  = getVar e
    getVar  (TyAppExp e _ _)    = getVar e
    getVar  _                   = Nothing

clearArgs :: OccEnv -> OccEnv
clearArgs env =
    env { var_args = [] }

addArg :: Var -> OccEnv -> OccEnv
addArg v env =
    env { var_args = lookupArgInfo v env : var_args env }

andOcc :: OccEnv -> OccEnv -> OccEnv
andOcc env env' = env
    {  var_occinfo  =  Map.foldWithKey  combineOccInfo
                                        (var_occinfo env)
                                        (var_occinfo env')
    ,  var_scrut    =  (var_scrut env) `Set.union`
                       (var_scrut env')
    }
  where
    combineOccInfo  ::  Var
                    ->  OccInfo
                    ->  Map.Map Var OccInfo
                    ->  Map.Map Var OccInfo
    combineOccInfo v occinfo env =
        case (occinfo, Map.lookup v env) of
          (_, Nothing)  -> Map.insert v occinfo env
          (_, Just _)   -> Map.insert v Many env

orOcc :: OccEnv -> OccEnv -> OccEnv
orOcc env env' = env
    {  var_occinfo  =  Map.foldWithKey  combineOccInfo
                                        (var_occinfo env)
                                        (var_occinfo env')
    ,  var_scrut    =  (var_scrut env) `Set.intersection`
                       (var_scrut env')
    }
  where
    combineOccInfo  ::  Var
                    ->  OccInfo
                    ->  Map.Map Var OccInfo
                    ->  Map.Map Var OccInfo
    combineOccInfo v occinfo env =
        case (occinfo, Map.lookup v env) of
          (_,           Nothing) ->          Map.insert v occinfo env
          (Once,        Just Once) ->        Map.insert v ManyBranch env
          (Once,        Just ManyBranch) ->  Map.insert v ManyBranch env
          (ManyBranch,  Just Once) ->        Map.insert v ManyBranch env
          (ManyBranch,  Just ManyBranch) ->  Map.insert v ManyBranch env
          (_,           Just _) ->           Map.insert v Many env
\end{code}

\begin{code}
occDecls  ::  MonadTc m
          =>  [Var]
          ->  [Decl]
          ->  m [Decl]
occDecls live decls = return snd `ap` occDecl live decls
\end{code}

\begin{code}
occDecl  ::  MonadTc m
         =>  [Var]
         ->  [Decl]
         ->  m (OccEnv, [Decl])
occDecl  _ [] = return (occEmpty, [])

occDecl  live  (d@(DataDecl _ _ _ _) : ds)  = do
    (env, ds') <- occDecl live ds
    return (env, d : ds')

occDecl  live  (d@(TypeDecl _ _ _ _) : ds)  = do
    (env, ds') <- occDecl live ds
    return (env, d : ds')

occDecl  live  (d@(AxiomDecl _ _ _) : ds)  = do
    (env, ds') <- occDecl live ds
    return (env, d : ds')

occDecl  live  (d@(SigDecl _ _ _) : ds)  = do
    (env, ds') <- occDecl live ds
    return (env, d : ds')

occDecl  live  ((LetDecl rec _) : ds) = do
    (env, ds')    <-  occDecl live ds
    (env', rec')  <-  occBindings TopLevel live env rec

    let vbinfo       =   [(v, binfo) | Binding v _ binfo _ _ <- extractRec rec']
    traceSimpl $ nest 4 $ text "bindings:" </>
        stack (map (\(v, bindinfo) -> ppr v <+> ppr bindinfo) vbinfo)

    return (env', letD rec' : ds')
\end{code}

\begin{code}
setOccInfo  ::  MonadTc m
            =>  Level
            ->  [Var]
            ->  OccEnv
            ->  Binding
            ->  m Binding
setOccInfo lvl live env (Binding v tau bindinfo e l) =
    return $ Binding v tau bindinfo' e l
  where
    bindinfo' = bindinfo { bind_occinfo = occinfo }

    occinfo = case lookupOccInfo v env of
                Nothing ->  if lvl == TopLevel && v `elem` live
                            then Many
                            else Dead
                Just x ->   x
\end{code}

\begin{code}
occBinding  ::  forall m . MonadTc m
            =>  Binding
            ->  m (OccEnv, Binding)
occBinding (Binding v tau bindinfo e l) = do
    (env, e')      <-  occExp e
    let bindinfo'  =   bindinfo { bind_arginfo = var_args env }
    return (env, Binding v tau bindinfo' e' l)
\end{code}

\begin{code}
occBindings  ::  forall m . MonadTc m
             =>  Level
             ->  [Var]
             ->  OccEnv
             ->  Rec Binding
             ->  m (OccEnv, Rec Binding)
occBindings lvl live env_rest (NonRec b@(Binding v _ _ _ _)) = do
    (env_e, b')  <-  occBinding b
    let env'     =   (occDelete v env_e) `andOcc` env_rest
    b''          <-  setOccInfo lvl live env' b'
    return (occDelete v env', NonRec b'')

occBindings lvl live env_rest (Rec bs) =
    extendVars (map boundVar bs) $ do
    (envs, bs')  <-  mapAndUnzipM occBinding bs
    let env'     =   foldl' andOcc env_rest envs
    bs''         <-  mapM (setOccInfo lvl live env') bs'
    return (occDeleteMany vs env', Rec bs'')
  where
    vs :: [Var]
    vs = [v | Binding v _ _ _ _ <- bs]
\end{code}

\begin{code}
occExp  ::  MonadTc m
        =>  Exp
        ->  m (OccEnv, Exp)
occExp e@(LitExp _ _) = return (occEmpty, e)
occExp e@(ConExp _ _) = return (occEmpty, e)

occExp e@(VarExp v _) = return (occSingleton v Once, e)

occExp (LamExp v ty ebody l) = do
    (env, ebody')  <-  occExp ebody
    let env'       =   occDelete v $
                       adjustOccInfo adjust $
                       addArg v env
    return (env', LamExp v ty ebody' l)
  where
    adjust :: OccInfo -> OccInfo
    adjust  Once  = OnceInLam
    adjust  x     = x

occExp e@(AppExp _ _ _) = do
    (envs, es')  <-  mapAndUnzipM occExp es
    let env      =   clearArgs $
                     scrutExp f $
                     adjustForApp $
                     foldl1' andOcc envs
    return (env, appsE (head es') (tail es'))
  where
    es@(f : _) = unfoldAppExp e

    adjustForApp :: OccEnv -> OccEnv
    adjustForApp env =
        if isConstructorApp f
        then adjustOccInfo adjust env
        else env

    adjust :: OccInfo -> OccInfo
    adjust  Once  = Many
    adjust  x     = x

occExp (TyLamExp tv k e l) = do
    (env, e') <- occExp e
    return (env, TyLamExp tv k e' l)

occExp (TyAppExp e ty l) = do
    (env, e') <- occExp e
    return (env, TyAppExp e' ty l)

occExp (LetExp binds e l) = do
    (env, e')       <- occExp e
    (env', binds')  <- occBindings Nested [] env binds
    return (clearArgs $ env', LetExp binds' e' l)

occExp (CaseExp e v alts l) = do
    (env_e, e')    <-  occExp e
    (envs, alts')  <-  mapAndUnzipM occAlt alts
    let env        =   clearArgs $
                       scrutExp e $
                       env_e `orOcc` (occDelete v (foldl1' orOcc envs))
    return (env, CaseExp e' v alts' l)

occExp (CastExp e ty l) = do
    (env, e') <- occExp e
    return (clearArgs env, CastExp e' ty l)
\end{code}

\begin{code}
occAlt (Alt p e) = do
    (env, e') <- occExp e
    return (clearArgs env, Alt p e')
\end{code}
