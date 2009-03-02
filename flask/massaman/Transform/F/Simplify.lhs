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
-- Module      :  Transform.F.Simplify
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Transform.F.Simplify (
    module Transform.F.Simplify.Monad,
    simplify,
    costExp
  ) where

import Control.Monad (ap,
                      mapAndUnzipM)
import Data.List (foldl',
                  sortBy)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Check.F
import Control.Monad.Exception
import Control.Monad.Trace
import Data.Loc
import Data.Name
import DepAnal
import Language.F
import Text.PrettyPrint.Mainland
import Transform.F.Simplify.Lower
import Transform.F.Simplify.Metrics
import Transform.F.Simplify.Monad
import Transform.F.Simplify.OccAnal
\end{code}

\begin{code}
simplify  ::  forall m . MonadSimpl m
          =>  [Var]
          ->  (String -> [Decl] -> m ())
          ->  [Decl]
          ->  m [Decl]
simplify live dump decls = do
    traceSimpl $ text "live variables:" <+> commasep (map ppr live)
    maxIterations  <- getMaxIterations
    decls'         <- lower decls
    simpl 0 maxIterations decls'
  where
    lower :: [Decl] -> m [Decl]
    lower decls = do
        decls' <- savingTcEnv $ lowerDecls decls
        dump "lower" decls'
        return decls'

    simpl :: Int -> Int -> [Decl] -> m [Decl]
    simpl n max decls
        | n == max   = return decls
        | otherwise  = do
            traceSimpl $ text "simplifier round" <+> ppr n
            env      <- getSimplEnv
            decls'   <- savingTcEnv $ occDecls live decls
            decls''  <- savingTcEnv $ simplDecls decls'
            env'     <- getSimplEnv
            dump ("simpl." ++ show n) decls''
            if env' == env
              then return decls''
              else simpl (n+1) max decls''
\end{code}

\begin{code}
breakLoops  ::  MonadSimpl m
            =>  [Binding]
            ->  m ([Binding], [Binding])
breakLoops  [b]  = return ([b], [])
breakLoops  bs   = do
    traceSimpl $ text "loop breaker:" <+> ppr vbreaker  </>
        (nest 4 $ text "scored bindings:" </>
              stack [ppr v <> colon <+> ppr score
                         |  (Binding v _ _ _ _, _, _, score) <- scoredDeps])
    (bs', breakers) <-  mapAndUnzipM breakLoops $
                        map extractRec $
                        defGroups [(b, v, vs) | (b, v, vs, _) <- tail scoredDeps]
    return (concat bs', setOccInfo breaker LoopBreaker : concat breakers)
  where
    breaker :: Binding
    vbreaker :: Var
    (breaker, vbreaker, _, _) = head scoredDeps

    scoredDeps :: [(Binding, Var, [Var], Int)]
    scoredDeps =
        sortBy  (\(_, _, _, score1) (_, _, _, score2) ->
                     compare score1 score2)
                [(b, v, vs, score) |  b <- bs,
                                      let [v] = binders b,
                                      let vs = Set.toList (free b),
                                      let score = loopBreakerScore b]

    setOccInfo :: Binding -> OccInfo -> Binding
    setOccInfo (Binding v tau bindinfo e l) occinfo =
        Binding v tau bindinfo' e l
      where
        bindinfo' = bindinfo { bind_occinfo = occinfo }
\end{code}

\begin{code}
type InDecl = Decl
type InBinding = Binding
type InVar = Var
type InExp = Exp
type InAlt = Alt

type OutDecl = Decl
type OutBinding = Binding
type OutVar = Var
type OutExp = Exp
type OutAlt = Alt
\end{code}

\begin{code}
type Subst = Map.Map InVar SubstRng

data SubstRng  =  DoneExp OutExp
               |  SuspExp Subst InExp

type TySubst = Map.Map TyVar Type
\end{code}

\begin{code}
extendSubst :: [(InVar, SubstRng)] -> Subst -> Subst
extendSubst vs theta =
    foldl' (\theta (v, r) -> Map.insert v r theta) theta vs
\end{code}

\begin{code}
data Level = TopLevel | Nested
  deriving (Eq)

data Definition  =  Unknown
                 |  BoundTo OutExp OccInfo Level
                 |  NotAmong [Con]

type InScope = Map.Map OutVar Definition
\end{code}

\begin{code}
extendScope :: [(OutVar, Definition)] -> InScope -> InScope
extendScope vs phi =
    foldl' (\phi (v, def) -> Map.insert v def phi) phi vs

extendWildScope :: [(WildVar, Definition)] -> InScope -> InScope
extendWildScope wvs phi =
    extendScope [(v, def) | (TameVar v, def) <- wvs] phi
\end{code}

\begin{code}
data Context  =  Stop
              |  AppCtx InExp Subst InScope TySubst Context
              |  TyAppCtx Type Context
              |  CaseCtx InVar Type [InAlt] Subst InScope TySubst Context
              |  ArgCtx OutExp Context
\end{code}

\begin{code}
simplDecls :: MonadSimpl m => [Decl] -> m [Decl]
simplDecls decls = do
    mapM_ checkTyConDecl decls
    mapM_ checkTypeDecl decls
    mapM_ checkSigDecl decls
    simplDecl theta phi decls
  where
    theta :: Subst
    theta = Map.empty

    phi :: InScope
    phi = Map.empty
\end{code}

\begin{code}
simplDecl  ::  MonadSimpl m
           =>  Subst
           ->  InScope
           ->  [InDecl]
           ->  m [OutDecl]
simplDecl _ _ [] = return []

simplDecl theta phi (d@(DataDecl _ _ _ _) : ds)  = do
    ds' <- simplDecl theta phi ds
    return $ d : ds'

simplDecl theta phi (d@(TypeDecl _ _ _ _) : ds)  = do
    ds' <- simplDecl theta phi ds
    return $ d : ds'

simplDecl theta phi (d@(AxiomDecl _ _ _) : ds)  = do
    ds' <- simplDecl theta phi ds
    return $ d : ds'

simplDecl theta phi (d@(SigDecl _ _ _) : ds)  = do
    ds' <- simplDecl theta phi ds
    return $ d : ds'

simplDecl theta phi ((LetDecl bs _) : ds) = do
    (maybe_bs', ds') <-  simplBindings TopLevel theta phi bs $
                         \theta' phi' ->
                             simplDecl theta' phi' ds
    case maybe_bs' of
      Nothing   -> return ds'
      Just bs'  -> return $ map letD (reanalyzeDependencies [bs']) ++ ds'
\end{code}

\begin{code}
preInlineUnconditionally  ::  MonadSimpl m
                          =>  Level
                          ->  Subst
                          ->  InScope
                          ->  [InBinding]
                          ->  m ([OutBinding], Subst, InScope)
preInlineUnconditionally  _    theta  phi  []                               =
    return ([], theta, phi)

preInlineUnconditionally  lvl  theta  phi  (b@(Binding _ _ bindinfo _ _) : bs)
    | bind_occinfo bindinfo == Dead = do
    droppedBinding b
    preInlineUnconditionally lvl theta phi bs

preInlineUnconditionally  lvl  theta  phi  (b@(Binding v _ bindinfo e _) : bs)
    | bind_occinfo bindinfo == Once = do
    droppedBinding b
    preInlinedBinding b
    preInlineUnconditionally lvl theta' phi bs
  where
    theta' = extendSubst [(v, SuspExp theta e)] theta

preInlineUnconditionally  lvl  theta  phi  (b@(Binding _ _ _ _ _) : bs)     = do
    (bs', theta'', phi'') <- preInlineUnconditionally lvl theta phi bs
    return (b : bs', theta'', phi'')
\end{code}

\begin{code}
postInlineUnconditionally  ::  forall m . MonadSimpl m
                           =>  Level
                           ->  Subst
                           ->  InScope
                           ->  [InBinding]
                           ->  m ([OutBinding], Subst, InScope)
postInlineUnconditionally _ theta phi [] = return ([], theta, phi)

postInlineUnconditionally lvl theta phi (b@(Binding v tau bindinfo e l) : bs) =
    simplExp theta phi Map.empty e Stop >>= postInline
  where
    occinfo :: OccInfo
    occinfo = bind_occinfo bindinfo

    postInline  ::  OutExp
                ->  m ([OutBinding], Subst, InScope)
    postInline e'
        | occinfo /= LoopBreaker && occinfo /= Many && isTrivial e' = do
            droppedBinding b
            postInlinedBinding b
            (bs', theta'', phi'') <- postInlineUnconditionally lvl theta' phi bs
            return (bs', theta'', phi'')
      where
        theta' = extendSubst [(v, DoneExp e')] theta

    postInline e'
        | v `Map.member` phi = do
            (bs', theta'', phi'') <- postInlineUnconditionally lvl theta' phi' bs
            return (b' : bs', theta'', phi'')
      where
        b'      = Binding v' tau bindinfo e' l
        v'      = freshname phi v
        theta'  = extendSubst [(v, DoneExp (varE v'))] theta
        phi'    = extendScope [(v', BoundTo e' occinfo lvl)] phi

    postInline e' = do
        (bs', theta'', phi'') <- postInlineUnconditionally lvl theta phi' bs
        return (b' : bs', theta'', phi'')
      where
        b'    = Binding v tau bindinfo e' l
        phi'  = extendScope [(v, BoundTo e' occinfo lvl)] phi
\end{code}

\begin{code}
simplBindings'  ::  forall m a . MonadSimpl m
                =>  Level
                ->  Subst
                ->  InScope
                ->  [InBinding]
                ->  (Subst -> InScope -> m a)
                ->  m ([OutBinding], a)
simplBindings' lvl theta phi bs f = do
    pprSimplState (text "simplBindings") theta phi
    (bs', theta', phi')     <- preInlineUnconditionally lvl theta phi bs
    pprSimplState (text "preInlineUnconditionally") theta' phi'
    (bs'', theta'', phi'')  <- postInlineUnconditionally lvl theta' phi' bs'
    pprSimplState (text "postInlineUnconditionally") theta'' phi''
    a <- f theta'' phi''
    return (bs'', a)
\end{code}

\begin{code}
simplBindings  ::  forall m a . MonadSimpl m
               =>  Level
               ->  Subst
               ->  InScope
               ->  Rec InBinding
               ->  (Subst -> InScope -> m a)
               ->  m (Maybe (Rec OutBinding), a)
simplBindings lvl theta phi (NonRec b) f =
    extendVars [boundVar b] $ do
    (bs', a) <- simplBindings' lvl theta phi [b] f
    case bs' of
      [] ->    return (Nothing, a)
      [b'] ->  return (Just (NonRec b'), a)
      _ ->     panic $ text  "Non-recursive binding generated \
                                       \multiple simplified bindings"
\end{code}

\begin{code}
simplBindings lvl theta phi rec f = do
    (nonbreak, break) <- breakLoops (extractRec rec)
    let bs = nonbreak ++ break
    traceSimpl $ nest 4 $ text "broken bindings:" </>
        stack [ppr v | Binding v _ _ _ _ <- bs]
    extendVars (map boundVar bs) $ do
    (bs', a) <- simplBindings' lvl theta phi bs f
    case bs' of
      [] ->  return (Nothing, a)
      _ ->   return (Just (Rec bs'), a)
\end{code}

\begin{code}
simplExp  ::  forall m . MonadSimpl m
          =>  Subst
          ->  InScope
          ->  TySubst
          ->  InExp
          ->  Context
          ->  m OutExp
simplExp  _  _  _ e@(LitExp _ _)  cont  =
    traceSimplExp e cont $
    rebuild e cont

simplExp  _  _  _ e@(ConExp _ _)  cont  =
    traceSimplExp e cont $
    rebuild e cont
\end{code}

\begin{code}
simplExp theta phi psi e@(LamExp v tau e' l) cont =
    traceSimplExp e cont $
    extendVars [(v, tau)] $ do
    tau'  <- simplType psi tau
    e''   <- simplExp theta phi' psi e' Stop
    rebuild (LamExp v tau' e'' l) cont
  where
    phi' = extendScope [(v, Unknown)] phi

simplExp theta phi psi e@(AppExp f a _) cont =
    traceSimplExp e cont $
    simplExp theta phi psi f (AppCtx a theta phi psi cont)

simplExp theta phi psi e@(TyLamExp a kappa e' _) (TyAppCtx tau cont) =
    traceSimplExp e cont $
    extendTyVars [(a, kappa)] $ do
    simplExp theta phi psi' e' cont
  where
    psi' = Map.insert a tau psi

simplExp theta phi psi e@(TyLamExp a kappa e' l) cont =
    traceSimplExp e cont $
    extendTyVars [(a, kappa)] $ do
    e'' <- simplExp theta phi psi e' Stop
    rebuild (TyLamExp a kappa e'' l) cont

simplExp theta phi psi e@(TyAppExp e' tau _) cont =
    traceSimplExp e cont $ do
    tau' <- simplType psi tau
    simplExp theta phi psi e' (TyAppCtx tau' cont)

simplExp theta phi psi e@(LetExp bs e' _) cont =
    traceSimplExp e cont $ do
    (maybe_bs', e'') <-  simplBindings Nested theta phi bs $
                         \theta' phi' ->
                             simplExp theta' phi' psi e' Stop
    case maybe_bs' of
      Nothing   -> rebuild e'' cont
      Just bs'  -> rebuild (foldr letE e'' (reanalyzeDependencies [bs'])) cont

simplExp theta phi psi e@(CaseExp e' v alts _) cont =
    traceSimplExp e cont $ do
    tau <- tcExp e'
    extendVars [(v, tau)] $ do
    tau' <- simplType psi tau
    simplExp theta phi psi e' (CaseCtx v tau' alts theta phi psi cont)

simplExp theta phi psi e@(CastExp e' tau l) cont =
    traceSimplExp e cont $ do
    tau'  <- simplType psi tau
    e''   <- simplExp theta phi psi e' Stop
    rebuild (CastExp e'' tau' l) cont
\end{code}

\begin{code}
simplExp theta phi psi e@(VarExp v _) cont =
    traceSimplExp e cont $ do
    case Map.lookup v theta of
      Just (SuspExp theta' e') ->  simplExp  theta'     phi  psi  e'  cont
      Just (DoneExp e') ->         simplExp  Map.empty  phi  psi  e'  cont
      Nothing ->                   callSiteInline phi v cont
\end{code}

\begin{code}
simplType  ::  forall m . MonadSimpl m
           =>  TySubst
           ->  Type
           ->  m Type
simplType  _    tau@(TyConTy _ _)   =
    return tau

simplType  psi  tau@(TyVarTy alpha _)  =
    case Map.lookup alpha psi of
      Just tau' ->  return tau'
      Nothing ->    return tau

simplType  psi  (AppTy tau1 tau2 l)  = do
    tau1' <- simplType psi tau1
    tau2' <- simplType psi tau2
    return $ AppTy tau1' tau2' l

simplType  psi  (AppTyFunTy f taus l)  = do
    taus' <- mapM (simplType psi) taus
    return $ AppTyFunTy f taus' l

simplType  psi  (ForAll WildTyVar kappa tau l)  = do
    tau' <- simplType psi tau
    return $ ForAll WildTyVar kappa tau' l

simplType  psi  (ForAll (TameTyVar alpha) kappa tau l)  = do
    tau' <- simplType psi' tau
    return $ ForAll WildTyVar kappa tau' l
  where
    psi' = Map.delete alpha psi

simplType  psi  (SymCo tau l)  = do
    tau' <- simplType psi tau
    return $ SymCo tau' l

simplType  psi  (TransCo tau1 tau2 l)  = do
    tau1' <- simplType psi tau1
    tau2' <- simplType psi tau2
    return $ TransCo tau1' tau2' l

simplType  psi  (AppCo tau1 tau2 l)  = do
    tau1' <- simplType psi tau1
    tau2' <- simplType psi tau2
    return $ AppCo tau1' tau2' l

simplType  psi  (LeftCo tau l)  = do
    tau' <- simplType psi tau
    return $ LeftCo tau' l

simplType  psi  (RightCo tau l)  = do
    tau' <- simplType psi tau
    return $ RightCo tau' l
\end{code}

\begin{code}
callSiteInline  ::  MonadSimpl m
                =>  InScope
                ->  Var
                ->  Context
                ->  m OutExp
callSiteInline phi v cont = do
    traceSimpl $ text "inlining" <+> squotes (ppr v) <+> text "at call site"
    case Map.lookup v phi of
      Just (BoundTo rhs occinfo lvl) -> do  traceSimpl $ nest 4 $ line <>
                                                text "    occinfo:" <+> ppr occinfo </>
                                                text "        rhs:" <+> ppr rhs </>
                                                text "someBenefit:" <+>
                                                    ppr (someBenefit rhs lvl cont) </>
                                                text "inlineMulti:" <+>
                                                    ppr (someBenefit rhs lvl cont)
      _ -> return ()
    case Map.lookup v phi of
      Nothing ->                            rebuild (varE v) cont
                                            -- throwExceptionAt l $ UnboundVariable v
      Just (BoundTo rhs occinfo lvl)
          | inline rhs occinfo lvl cont ->  do  inlinedVar v
                                                simplExp Map.empty phi Map.empty rhs cont
      Just _ ->                             rebuild (varE v) cont
\end{code}

\begin{code}
inline  ::  OutExp
        ->  OccInfo
        ->  Level
        ->  Context
        ->  Bool
inline  _    Dead         _    _     =  error "inline: Dead"

inline  _    LoopBreaker  _    _     =  False

inline  _    Once         _    _     =  error "inline: Once"

inline  rhs  OnceInLam    lvl  cont  =  isWhnfOrBot rhs &&
                                        someBenefit rhs lvl cont

inline  rhs  ManyBranch   lvl  cont  =  inlineMulti rhs lvl cont

inline  rhs  Many         lvl  cont  =  isWhnfOrBot rhs &&
                                        inlineMulti rhs lvl cont
\end{code}

\begin{code}
someBenefit  ::  OutExp
             ->  Level
             ->  Context
             ->  Bool
someBenefit rhs _ cont
    | isTrivial rhs                           = True
    | isConstructorApp rhs && isCaseCtx cont  = True
    | isConstructorArgCtx cont                = False
\end{code}

\begin{code}
someBenefit rhs@(LamExp _ _ _ _) lvl cont
    | hasBeneficialArgument nLambdas cont   = True
    | lvl == Nested && fullyApplied         = True
    | otherwise                             = False
  where
    nLambdas :: Int
    nLambdas = countLambdas rhs

    nArgs :: Int
    nArgs = countArgs cont

    fullyApplied :: Bool
    fullyApplied = nLambdas <= nArgs

    countLambdas :: Exp -> Int
    countLambdas (LamExp _ _ e _)  = 1 + countLambdas e
    countLambdas _                 = 0

    countArgs :: Context -> Int
    countArgs  (AppCtx _ _ _ _ cont')  = 1 + countArgs cont'
    countArgs  _                       = 0

    hasBeneficialArgument :: Int -> Context -> Bool
    hasBeneficialArgument  0  _ = False

    hasBeneficialArgument  _  (AppCtx (VarExp v _) _ phi _ _) =
        case Map.lookup v phi of
          Nothing ->       False
          Just Unknown ->  False
          Just _ ->        True

    hasBeneficialArgument  _  (AppCtx e _ _ _ _)
        | not (isTrivial e)            = True

    hasBeneficialArgument  n  (AppCtx _ _ _ _ cont') =
        hasBeneficialArgument (n-1) cont'

    hasBeneficialArgument  _  _ =
        False
\end{code}

\begin{code}
someBenefit _ _ _                             = False
\end{code}

\begin{code}
inlineMulti  ::  OutExp
             ->  Level
             ->  Context
             ->  Bool
inlineMulti  rhs  _  cont
    | isTrivial rhs                           = True
    | isConstructorApp rhs && isCaseCtx cont  = True
    | isConstructorArgCtx cont                = False
    | otherwise                               = False
\end{code}

\begin{code}
isConstructorArgCtx :: Context -> Bool
isConstructorArgCtx  (ArgCtx f _)  | isConstructorApp f  = True
isConstructorArgCtx  _                                   = False
\end{code}

\begin{code}
isCaseCtx :: Context -> Bool
isCaseCtx  (CaseCtx _ _ _ _ _ _ _)  = True
isCaseCtx  (TyAppCtx _ cont)        = isCaseCtx cont
isCaseCtx  _                        = False
\end{code}

\begin{code}
costExp  ::  MonadSimpl m
         =>  Exp
         ->  m Int
costExp (LitExp _ _) =
    return 1

costExp (ConExp _ _) =
    return 1

costExp (VarExp _ _) =
    return 1

costExp (LamExp v tau e _) =
    extendVars [(v, tau)] $ do
    ce <- costExp e
    return $ 1 + ce

costExp (AppExp e1 e2 _) = do
    ce1 <- costExp e1
    ce2 <- costExp e2
    return $ ce1 + ce2

costExp (TyLamExp a kappa e _) =
    extendTyVars [(a, kappa)] $
    costExp e

costExp (TyAppExp e _ _) =
    costExp e

costExp (LetExp bs e _) = do
  cb <- costBindings bs $
        costExp e
  return $ 1 + cb

costExp (CaseExp e v alts _) = do
    tau <- tcExp e
    extendVars [(v, tau)] $ do
    ce     <- costExp e
    calts  <- mapM costAlt alts
    ccon   <- conFamilySize tau
    if isVar e
      then return $ sum calts + ccon
      else return $ ce + sum calts + ccon
  where
    isVar  (VarExp _ _)        = True
    isVar  (TyLamExp _ _ e _)  = isVar e
    isVar  (TyAppExp e _ _ )   = isVar e
    isVar  _                   = False

costExp (CastExp e _ _) =
    costExp e
\end{code}

\begin{code}
costAlt  ::  MonadSimpl m
         =>  Alt
         ->  m Int
costAlt (Alt _ e) = costExp e
\end{code}

\begin{code}
conFamilySize  ::  MonadSimpl m
               =>  Type
               ->  m Int
conFamilySize tau = do
  case unfoldAppTy tau of
    TyConTy tycon _ : _ ->  return length `ap` constructors tycon
    _ ->                    return 0
\end{code}

\begin{code}
costBindings  ::  MonadSimpl m
              =>  Rec Binding
              ->  m Int
              ->  m Int
costBindings (NonRec b) m = do
    cb  <- costBinding b
    cm  <- extendVars [boundVar b] m
    return $ cb + cm

costBindings (Rec bs) m =
    extendVars (map boundVar bs) $ do
    cbs  <- mapM costBinding bs
    cm   <- m
    return $ sum cbs + cm
\end{code}

\begin{code}
costBinding  ::  MonadSimpl m
             =>  Binding
             ->  m Int
costBinding (Binding _ _ _ e _) =
    costExp e
\end{code}

\begin{code}
rebuild  ::  forall m . MonadSimpl m
         =>  OutExp
         ->  Context
         ->  m OutExp
rebuild e  ctx@Stop =
    traceRebuild e ctx $
    return e
\end{code}

\begin{code}
rebuild f  ctx@(AppCtx e theta phi psi cont) =
    traceRebuild e ctx $ do
    simplExp theta phi psi e (ArgCtx f cont)
\end{code}

\begin{code}
rebuild e  ctx@(TyAppCtx tau cont) =
    traceRebuild e ctx $ do
    rebuild (tyappE e tau) cont
\end{code}

\begin{code}
rebuild e  ctx@(CaseCtx v tau alts theta phi psi cont) =
    traceRebuild e ctx $ do
    ncons  <-  case unfoldAppTy tau of
                 TyConTy tycon _ : _ ->  return length `ap` constructors tycon
                 _ ->                    return 0
    alts'  <-  extendVars [(v, tau)] $
               go ncons [] alts
    rebuild (CaseExp e v alts' l) Stop
  where
    l :: SrcLoc
    l = fromLoc (getLoc e)

    go  ::  Int
        ->  [Con]
        ->  [InAlt]
        ->  m [OutAlt]
    go  _      _     [] =
        return []

    go  ncons  seen  _
        | length seen == ncons && ncons /= 0 =
        return []

    go  ncons  seen  (Alt p@(LitPat _ _) e : alts) = do
        e'     <- simplExp theta phi' psi e cont
        alts'  <- go ncons seen alts
        return $ Alt p e' : alts'
      where
        phi' = extendScope [(v, NotAmong seen)] phi

    go  _      seen  (Alt p@(VarPat (wv, tau_p) _) e : _) = do
        tau_p' <- simplType psi tau_p
        extendWildVars [(wv, tau_p')] $ do
        e' <- simplExp theta phi' psi e cont
        return [Alt p e']
      where
        phi' =  extendScope [(v, NotAmong seen)] $
                extendWildScope [(wv, NotAmong seen)] phi

    go  ncons  seen  (Alt (ConPat con ptyvs pvs l) e : alts)
        | con `member` seen  = go ncons seen alts
        | otherwise          = do
            taus'     <-  mapM (simplType psi) taus
            let pvs'  =   wvs `zip` taus'
            extendWildTyVars ptyvs $
                extendWildVars pvs' $ do
                e'     <-  simplExp theta phi' psi e cont
                alts'  <-  go ncons seen' alts
                return $ Alt (ConPat con ptyvs pvs' l) e' : alts'
      where
        (wvs, taus)  = unzip pvs
        seen'        =  con : seen
        phi'         =  extendScope [(v, NotAmong seen)] $
                        extendWildScope (map fst pvs `zip` repeat Unknown) phi
\end{code}

\begin{code}
rebuild e  ctx@(ArgCtx f cont) =
    traceRebuild e ctx $
    rebuild (appE f e) cont
\end{code}

\begin{code}
instance Pretty SubstRng where
    ppr (DoneExp e) = text "DoneExp" <+> ppr e
    ppr (SuspExp theta e) =
        nest 4 $ text "SuspExp" <+/> ppr (Map.toList theta) <+/> ppr e
\end{code}

\begin{code}
instance Pretty Definition where
    ppr Unknown = text "Unknown"
    ppr (BoundTo e _ _) =
        text "BoundTo" <+> ppr e
    ppr (NotAmong cons) =
        text "NotAmong" <+> ppr cons
\end{code}

\begin{code}
instance Pretty Context where
    ppr Stop = text "Stop"
    ppr (AppCtx e _ _ _ cont) =
        text "App" <+> ppr e <+> colon <+/> ppr cont
    ppr (TyAppCtx tau cont) =
        text "TyApp" <+> ppr tau <+>colon <+/> ppr cont
    ppr (CaseCtx v _ alts _ _ _ cont) =
        text "Case" <+> ppr v <+> ppr alts <+> colon <+/> ppr cont
    ppr (ArgCtx f cont) =
        text "Arg" <+> ppr f <+> colon <+/> ppr cont
\end{code}

\begin{code}
pprSimplState  ::  MonadSimpl m
               =>  Doc
               ->  Subst
               ->  InScope
               ->  m ()
pprSimplState desc theta phi =
    traceSimpl $ nest 4 $ desc <> colon </>
        (nest 4 $ text "theta:" </> stack (map ppr (Map.toList theta))) </>
        (nest 4 $ text "  phi:" </> stack (map ppr (Map.toList phi)))
\end{code}

\begin{code}
traceSimplExp  ::  MonadSimpl m
               =>  InExp
               ->  Context
               ->  m OutExp
               ->  m OutExp
traceSimplExp e ctx m = do
    traceSimpl $  text "simplifying expression:" <+> ppr e
                  <+> text "in context" <+> ppr ctx
    e' <- m
    traceSimpl $ text "yielded:" <+> ppr e'
    return e'
\end{code}

\begin{code}
traceRebuild  ::  MonadSimpl m
              =>  OutExp
              ->  Context
              ->  m OutExp
              ->  m OutExp
traceRebuild e ctx m =
    traceNest $ do
    traceSimpl $ text "rebuilding:" <+> ppr e <+> text "in context" <+> ppr ctx
    e' <- m
    traceSimpl $ text "yielded:" <+> ppr e'
    return e'
\end{code}
