%if False
\begin{code}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- Module      :  Check.Hs.Classes
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Check.Hs.Classes where

import Control.Monad.Error
import Data.List ((\\),
                  foldl',
                  sort)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Check.Hs.Bindings
import {-# SOURCE #-} Check.Hs.Check (checkBindingGroups,
                                      checkTypeKind)
import Check.Hs.Monad
import Check.Hs.Types
import Check.F.Bindings
import Control.Monad.ContextException
import Control.Monad.Exception
import Data.Loc
import Data.Name
import qualified Language.F as F
import qualified Language.Hs as H
import Text.PrettyPrint.Mainland

import Util
\end{code}
%endif

\chapter{Type Classes and Instances}

\section{Translating Type Classes}

Consider the following definition of the |Eq| class and its instances for |Int|
and lists:

\begin{spec}
data Int = I# Int#

class Eq where
    (==), (/=) :: a -> a -> Bool
    x /= y  = not (x == y)
    x == y  = not (x /= y)

instance Eq Int where
    (I# x) == (I# y) = x `eqInt#` y

instance Eq a => Eq[a] where
    []    ==  []    = True
    []    ==  y:ys  = False
    x:xs  ==  []    = False
    x:xs  ==  y:ys  = (x == y) && (xs == ys)
\end{spec}

We would like this to compile down to something like the following in our
intermediate language (I've omitted extraneous type annotations to make the code
more readable):

\begin{spec}
data EqD where
    EqD :: (a -> a -> Bool) -> (a -> a -> Bool) -> EqD a

(==) (EqD x _) = x
(/=) (EqD _ x) = x

eqInt = x `eqInt#` y
neInt = not ((==) eqDInt x  y)

eqDInt = EqD eqInt neInt

eqDList eqDa = EqD (eqList eqDa) (neList eqDa)

eqList eqDa  []    []    = True
eqList eqDa  []    y:ys  = False
eqList eqDa  x:xs  []    = False
eqList eqDa  x:xs  y:ys  = ((==) eqDa x y) && ((==) (eq::Dict::List eqDa) xs ys)

neList eqDa xs ys        = not ((==) (eqDList eqDa) xs ys)
\end{spec}

For a class declaration $\mathsf{class} \; C \Rightarrow TC \; t_1 \ldots
t_n$, we must define:

\begin{itemize}

\item The dictionary type. For the |Eq| class:

\begin{spec}
data EqD where
    EqD :: (a -> a -> Bool) -> (a -> a -> Bool) -> EqD a
\end{spec}

\item For each class member, a definition for the corresponding selector
function. For the |Eq| class:

\begin{spec}
(==) (EqD x _) = x
(/=) (EqD _ x) = x
\end{spec}
\end{itemize}

For an instance declaration $\mathsf{instance} \; C \Rightarrow TC \; t_1 \ldots
t_n$, we must define:

\begin{itemize}

\item A function taking dictionaries representing the context $C$ and producing
a class dictionary. For |Eq a => Eq [a]|,

\begin{spec}
eqDList eqDa = EqD (eqList eqDa) (neList eqDa)
\end{spec}

\item For each class member, a function that implements the member. For |Eq a =>
Eq [a]|,

\begin{spec}
eqList eqDa  []    []    = True
eqList eqDa  []    y:ys  = False
eqList eqDa  x:xs  []    = False
eqList eqDa  x:xs  y:ys  = ((==) eqDa x y) && ((==) (eqDList eqDa) xs ys)

neList eqDa xs ys        = not ((==) (eqDList eqDa) xs ys)
\end{spec}

\end{itemize}

Operationally, each class constraint in a context corresponds to a hidden
dictionary parameter. When we instantiate a function with a context, we add the
function's context to the context environment. Since we need a dictionary
parameter for each class constraint, at the same time we generate a fresh
dictionary parameter name for each newly added class constraint and maintain a
mapping from the class constraint to the dictionary parameter. When we perform
context reduction, we then need to add \emhp{definitions} for each of these
dictionary parameters. For the constraints that exist in the reduced context,
this is easy---they each correspond to an extra hidden function parameter. For
the other dictionaries, we must construct them from the dictionaries associated
with the constraints in the reduced context that we receive as hidden
parameters. Every time a

\section{Well-formedness Constraints on Type Classes and Instances}

\begin{defn}[Basic Conditions]

These conditions are taken directly from~\cite{sulzmann07fundep}.

\begin{itemize}

\item The context $C$ of a class or instance declaration can only mention type
variables, not type constructors, and in each individual class constraint all
the type variables are distinct.

\item In an instance declaration $\mathsf{instance} \; C \Rightarrow TC \;
t_1 \ldots t_n$, at least one of the types $t_i$ must not be a type variable.

\item Instance declarations must not overlap. That is, for any two instance
declarations $\mathsf{instance} \; C \Rightarrow TC \; t_1 \ldots t_n$ and
$\mathsf{instance} \; C' \Rightarrow TC \; t_1' \ldots t_n'$ there is no
substitution $\theta$ such that $\theta(t_1) = \theta(t_1'),\ldots,\theta(t_n) =
\theta(t_n')$.

\end{itemize}

\end{defn}

\begin{code}
checkContextBasicCondition  ::  forall r m . MonadTc r m
                            =>  Context r
                            ->  m ()
checkContextBasicCondition = mapM_ checkPred
  where
    checkPred :: Pred r -> m ()
    checkPred (ClassPred _ tys) = do
        checkDuplicates
            (text "Type variable appears more than once in class constraint")
            tvs
        when (not (all isTyVar tys)) $
            fail "Context may only mention type variables"
      where
        tvs :: [TyVar]
        tvs = Set.toList $ free tys
\end{code}

\begin{code}
checkInstanceBasicCondition  ::  forall r m . MonadTc r m
                             =>  H.TyCon -> [H.Type]
                             ->  m ()
checkInstanceBasicCondition _ tys = do
    when (all H.isTyVar tys) $
        fail "Context must mention at least one non-type variables"
\end{code}

\begin{defn}[Patterson Conditions]

These conditions are taken directly from~\cite{sulzmann07fundep}.

\begin{itemize}

\item The context $C$ of a class declaration can only mention type variables,
not type constructors, and in each individual class constraint all the type
variables are distinct.

\item For each instance declaration $\mathsf{instance} \; C \Rightarrow TC \;
t_1 \ldots t_n$:

\begin{itemize}

\item No variable has more occurrences in a type class constraint in the context
$C$ that in the head $TC \; t_1 \ldots t_n$.

\item Each type class constraint in the context $C$ has fewer constructors and
variables (taken together and counting repetitions) than the head.

\end{itemize}

\item Instance declarations must not overlap. That is, for any two instance
declarations $\mathsf{instance} \; C \Rightarrow TC \; t_1 \ldots t_n$ and
$\mathsf{instance} \; C' \Rightarrow TC \; t_1' \ldots t_n'$ there is no
substitution $\theta$ such that $\theta(t_1) = \theta(t_1'),\ldots,\theta(t_n) =
\theta(t_n')$.

\end{itemize}

\end{defn}

\subsection{Testing for Overlap}

\begin{code}
overlap  ::  MonadTc r m
         =>  TyCon -> [Type r]
         ->  TyCon -> [Type r]
         ->  m Bool
overlap tycon1 tys1 tycon2 tys2
    | tycon1 == tycon2 && length tys1 == length tys2  =
        do  tys1' <- instantiate tys1
            tys2' <- instantiate tys2
            (zipWithM_ unify tys1' tys2' >> return True)
                `catchError` \_ ->
                return False
    | otherwise                                      =
        return False
  where
    instantiate  ::  MonadTc r m
                 =>  [Type r]
                 ->  m [Type r]
    instantiate tys = do
      tys'       <-  compress tys
      let tvs    =   Set.toList (free tys') :: [TyVar]
      mtvs       <-  mapM (const newMetaTv) tvs
      let theta  =   Map.fromList $ tvs `zip` mtvs
      let phi    =   free theta
      return $ map (subst theta phi) tys
\end{code}

\begin{code}
data MoreSpecific  =  MoreSpecific
                   |  LessSpecific
                   |  EquallySpecific
                   |  IncomparablySpecific
   deriving (Eq, Ord)

class HowSpecific a where
    howSpecific :: MonadTc r m => a -> a -> m MoreSpecific

instance HowSpecific (Type r) where
    howSpecific  (TyVarTy _)        (TyVarTy _)        = return EquallySpecific
    howSpecific  _                  (TyVarTy _)        = return MoreSpecific
    howSpecific  (TyVarTy _)        _                  = return LessSpecific

    howSpecific  (TyConTy tycon1)   (TyConTy tycon2)
        | tycon1 == tycon2                             = return EquallySpecific

    howSpecific  (AppTy ty1a ty2a)  (AppTy ty1b ty2b)  =
        do  spec1 <- howSpecific ty1a ty1b
            case spec1 of
              MoreSpecific          -> return MoreSpecific
              LessSpecific          -> return LessSpecific
              EquallySpecific       -> howSpecific ty2a ty2b
              IncomparablySpecific  -> return IncomparablySpecific

    howSpecific  _                  _                  =
        panic $ text "howSpecific called on types that don't match"

instance HowSpecific (Pred r) where
    howSpecific (ClassPred tycon1 tys1) (ClassPred tycon2 tys2)
        | tycon1 == tycon2 && length tys1 == length tys2  =
            go EquallySpecific tys1 tys2
        | otherwise                                       =
            panic $
            text "howSpecific called on class constraints that don't match"
      where
        go  spec  []            []            = return spec
        go  spec  (ty1 : tys1)  (ty2 : tys2)  =
            do  spec_ty <- howSpecific ty1 ty2
                case (spec, spec_ty) of
                  (EquallySpecific, _)          -> go spec_ty tys1 tys2
                  (MoreSpecific, LessSpecific)  -> return IncomparablySpecific
                  (MoreSpecific, _)             -> go MoreSpecific tys1 tys2
                  (LessSpecific, MoreSpecific)  -> return IncomparablySpecific
                  (LessSpecific, _)             -> go LessSpecific tys1 tys2
                  (IncomparablySpecific, _)     -> return IncomparablySpecific
        go  _     _             _             =
            panic $
            text "howSpecific did not detect constraints of different sizes"
\end{code}

\section{Entailment}

\begin{code}
antecedents  ::  MonadTc r m
             =>  OpPred r
             ->  m [OpPred r]
antecedents op@(OpPred p@(ClassPred tycon _) v) = do
    cls            <-  lookupClass tycon
    supers         <-  lookupSupers p
    let super_mes  =   [return $ F.appE (F.varE dsel) (F.varE v)
                            | dsel <- cls_dictselectors cls]
    super_ops      <-  extendContext supers (Just super_mes)
    ancestors      <-  mapM antecedents super_ops
    return $ op : concat ancestors
\end{code}

\begin{code}
subgoals  ::  forall r m . MonadTc r m
          =>  OpPred r
          ->  m (Maybe [OpPred r])
subgoals (OpPred p@(ClassPred tycon _) v) = do
    instances  <- lookupInstances tycon
    matches    <- return catMaybes `ap` mapM tryInst instances
    case matches of
      []                      ->  return Nothing
      [(inst, tys, oppreds)]  ->  do
          replacePlaceholder v $ do
              ftys <- compress tys >>= coerceAst
              return $ F.appsE  (F.tyappsE (F.varE (inst_dictf inst)) ftys)
                                [F.varE v | OpPred _ v <- oppreds]
          return $ Just oppreds
      _                       ->  fail "Overlapping instances"
  where
    tryInst :: Instance r -> m (Maybe (Instance r, [Type r], [OpPred r]))
    tryInst inst =
        do  theta            <-  match Map.empty p inst_pred :: m (Theta r)
            let phi          =   free theta
            let super_preds  =   map (subst theta phi) (inst_ctx inst)
            let tys          =   subst theta phi [TyVarTy tv | tv <- inst_tvs inst]
            oppreds          <-  extendContext super_preds Nothing
            return $ Just (inst, tys, oppreds)
        `catchError` \_ ->
            return Nothing
      where
        inst_pred :: Pred r
        inst_pred = ClassPred (inst_tycon inst) (inst_tys inst)
\end{code}

\begin{code}
entail  ::  forall r m . MonadTc r m
        =>  [OpPred r]
        ->  OpPred r
        ->  m Bool
entail ops op@(OpPred p v) = entailedByAntecedents `orElse` entailedByInstance
  where
    entailedByAntecedents :: m Bool
    entailedByAntecedents = do
        oqs <- return concat `ap` mapM antecedents ops
        entailedByOneOf oqs

    entailedByOneOf :: [OpPred r] -> m Bool
    entailedByOneOf  [] =
        return False
    entailedByOneOf  (OpPred p' v' : oqs)
        | p == p'    =  do  replacePlaceholder v (return $ F.varE v')
                            return True
        | otherwise  =  entailedByOneOf oqs

    entailedByInstance :: m Bool
    entailedByInstance = do
        insts <- subgoals op
        case insts of
          Nothing   -> return False
          Just oqs  -> allM (entail ops) oqs
\end{code}

Check that the given operational context entails the given predicates.

\begin{code}
(|=?)  ::  forall r m . MonadTc r m
       =>  OpContext r
       ->  [OpPred r]
       ->  m ()
(|=?) = checkEntailment

checkEntailment  ::  forall r m . MonadTc r m
                 =>  OpContext r
                 ->  [OpPred r]
                 ->  m ()
checkEntailment opctx ops = do
    not_entailed  <-  filterM  (\pred ->  liftM not $ opctx `entail` pred)
                               ops
    when (length not_entailed /= 0) $
        fail $ pretty 80 $
            text "Could not deduce"
            <+> ppr [p | OpPred p _ <- not_entailed]
            <+> text "from the context"
            <+> case opctx of
                  []  -> text "()"
                  _   -> ppr [p | OpPred p _ <- opctx]
\end{code}

\section{Context Reduction}

To perform context reduction when we check data declarations, we need to already
know about all class and instance declarations, so we record all classes and
instances before doing any kind checking. This means we may have recorded some
invalid declarations: classes and instances may not be well-kinded, instances
may declare members not present in their superclass, etc. However, I don't think
the termination properties of context reduction depend on the well-kindedness of
class and instance declarations, so as long as we check the standard conditions
on classes and instances (Coverage, Patterson, etc.) then context reduction
should work just fine. This way we can perform context reduction while delaying
fully checking class and instance declarations until later on; we check class
kindedness during the type declaration kind checking phase, and we check class
and instance declarations as well as instance well-kindedness during the type
checking phase.

\begin{code}
isHnf  ::  forall r m . MonadTc r m
       =>  Pred r
       ->  m Bool
isHnf p@(ClassPred _ [ty]) = hnf ty
  where
    hnf :: Type r -> m Bool
    hnf  (TyConTy _)       = return False
    hnf  (MetaTv _)        = return True
    hnf  (TyVarTy _)       = return True
    hnf  (AppTy ty1 _)     = hnf ty1
    hnf  _                 = panic $ text "isHnf called on predicate:" <> ppr p
isHnf p@(ClassPred _ _)    = panic $ text "isHnf called on predicate:" <> ppr p
\end{code}

\begin{code}
toHnf  ::  forall r m . MonadTc r m
       =>  OpPred r
       ->  m [OpPred r]
toHnf op@(OpPred p _) = do
    hnf <- isHnf p
    if hnf
      then return [op]
      else do  insts <- subgoals op
               case insts of
                 Nothing  -> fail $ "Could not reduce context " ++ show p
                 Just qs  -> toHnfs qs
\end{code}

\begin{code}
toHnfs  ::  forall r m . MonadTc r m
        =>  OpContext r
        ->  m (OpContext r)
toHnfs ps = return concat `ap` mapM toHnf ps
\end{code}

\begin{code}
simplify  ::  forall r m . MonadTc r m
          =>  OpContext r
          ->  m (OpContext r)
simplify = loop []
  where
    loop :: [OpPred r] -> [OpPred r] -> m [OpPred r]
    loop  qs  []        = return $ sort qs
    loop  qs  (p : ps)  = do  entailed <- entail (qs ++ ps) p
                              if entailed
                                then loop qs ps
                                else loop (p : qs) ps
\end{code}

\begin{code}
reduce  ::  forall r m . MonadTc r m
        =>  [OpPred r]
        ->  m [OpPred r]
reduce ps = compress ps >>= toHnfs >>= simplify
\end{code}

\section{Recording classes and instances in the typing environment}

\begin{code}
checkForCyclicClassDecls  ::  MonadTc r m
                          =>  [H.Decl]
                          ->  m ()
checkForCyclicClassDecls classdecls = do
    when (length classdecls > 1) $
        fail $ pretty 80 $
        nest 4 $
        text "Cyclic class declarations:"
        </> stack (map ppr classdecls)
\end{code}

\begin{code}
checkForRedefinedClass  ::  MonadTc r m
                        =>  TyCon
                        ->  m ()
checkForRedefinedClass tycon = do
    defined <- classExists tycon
    when defined $
        fail $ "Class " ++ show tycon ++ " already defined"
\end{code}

\begin{code}
checkForUndefinedClass  ::  MonadTc r m
                        =>  TyCon
                        ->  m ()
checkForUndefinedClass tycon = do
    defined <- classExists tycon
    when (not defined) $
        fail $ "Class " ++ show tycon ++ " not defined"
\end{code}

\begin{code}
checkForUndefinedSuperclasses  ::  forall r m . MonadTc r m
                               =>  Context r
                               ->  m ()
checkForUndefinedSuperclasses ctx = do
    undefinedSuperclasses <- filterM classDoesNotExist ctx
    case undefinedSuperclasses of
      []   -> return ()
      [p]  -> fail $ "Superclass " ++ show p ++ " not defined"
      ps   -> fail $ "Superclasses " ++ show ps ++ " not defined"
  where
    classDoesNotExist :: Pred r -> m Bool
    classDoesNotExist (ClassPred tycon _) = return not `ap` classExists tycon
\end{code}

\begin{code}
addClasses  ::  forall r m . MonadTc r m
            =>  TypeDef
            ->  m ()
addClasses (TypeDef decl) =
    addClass decl

addClasses (RecTypeDef (_, _, classdecls)) = do
    checkForCyclicClassDecls classdecls
    mapM_ addClass classdecls
\end{code}

\begin{code}
addClass  ::  forall r m . MonadTc r m
          =>  H.Decl
          ->  m ()
addClass  (H.ClassDecl ctx tycon@(H.TyCon n) tvs decls _)  = do
    checkDuplicates (text "duplicate type variable") tvs
    ctx'       <-  coerceAst ctx
    tycon'     <-  coerceAst tycon
    tvs'       <-  coerceAst tvs
    ks         <-  mapM (const newMetaKv) tvs
    let pred   =   ClassPred tycon' (map TyVarTy tvs')
    dict_ty    <-  coerceAst pred
    dsels      <-  mapM  (\pred -> liftM F.Var $ dictSelectorName tycon' pred)
                         ctx'
    let cls    =   Class  {  cls_ctx            = ctx'
                          ,  cls_tycon          = tycon'
                          ,  cls_hstvs          = tvs
                          ,  cls_tvs            = tvs'
                          ,  cls_ks             = ks
                          ,  cls_pred           = pred
                          ,  cls_binds          = bindings
                          ,  cls_dict_tycon     = F.TyCon dictn
                          ,  cls_dict_con       = F.Con dictn
                          ,  cls_dict_ty        = dict_ty
                          ,  cls_dictselectors  = dsels
                          }
    checkForRedefinedClass tycon'
    checkForUndefinedSuperclasses ctx'
    checkContextBasicCondition ctx'
    mapM_ checkMemberDecl decls
    insertClass cls
    traceClass $ text "Adding class:" <+> ppr tycon
  where
    dictn :: Name
    dictn = classDictName n

    bindings :: [H.Binding]
    bindings = bindingsFromDecls decls

    checkMemberDecl :: H.Decl -> m ()
    checkMemberDecl  (H.VarBindDecl _ _ _ _)   = return ()
    checkMemberDecl  (H.SigDecl _ _ _)         = return ()
    checkMemberDecl  (H.FixityDecl _ _ _ _)    = return ()

    checkMemberDecl  (H.PatBindDecl _ _ _)     =
        fail "Illegal pattern binding in class declaration"

    checkMemberDecl  _                          =
        fail "Illegal instance declaration"
\end{code}

\begin{code}
addClass  _                                   = return ()
\end{code}

\begin{code}
addInstance  ::  forall r m . MonadTc r m
             =>  H.Decl
             ->  m ()
addInstance  (H.InstDecl ctx tycon tys decls (SrcLoc loc)) =
    withLocContext loc locMsg $ do
    ctx'      <-  coerceAst ctx
    tycon'    <-  coerceAst tycon
    tys'      <-  coerceAst tys
    let tvs   =   Set.toList $ free tys
    tvs'      <-  coerceAst tvs
    ks        <-  mapM (const newMetaKv) tvs
    let pred  =   ClassPred tycon' tys'
    dictf     <-  liftM F.Var $ instanceDictName tycon' tys'
    let inst  =   Instance  {  inst_hsctx  = ctx
                            ,  inst_ctx    = ctx'
                            ,  inst_tycon  = tycon'
                            ,  inst_hstys  = tys
                            ,  inst_tys    = tys'
                            ,  inst_hstvs  = tvs
                            ,  inst_tvs    = tvs'
                            ,  inst_ks     = ks
                            ,  inst_pred   = pred
                            ,  inst_decls  = decls
                            ,  inst_dictf  = dictf
                            }
    checkForUndefinedClass tycon'
    checkForUndefinedSuperclasses ctx'
    checkContextBasicCondition ctx'
    checkForOverlappingInstances tycon' tys'
    mapM_ checkMemberDecl decls
    insertInstance inst
    insertNewInstance inst
  where
    locMsg :: Doc
    locMsg = text "In instance declaration for" <+> spread (ppr tycon : map ppr tys)

    checkMemberDecl :: H.Decl -> m ()
    checkMemberDecl  (H.VarBindDecl _ _ _ _)   = return ()

    checkMemberDecl  (H.SigDecl _ _ _)         =
        fail "Illegal signature in instance declaration"

    checkMemberDecl  (H.FixityDecl _ _ _ _)    =
        fail "Illegal fixity declaration in instance declaration"

    checkMemberDecl  (H.PatBindDecl _ _ _)     =
        fail "Illegal pattern binding in instance declaration"

    checkMemberDecl  _                          =
        fail "Illegal instance declaration"

addInstance  _                                                    =
    return ()
\end{code}

\begin{code}
checkForOverlappingInstances  ::  forall r m . MonadTc r m
                              =>  TyCon
                              ->  [Type r]
                              ->  m ()
checkForOverlappingInstances tycon tys = do
    insts <- lookupInstances tycon
    overlappingInstances <- filterM instanceOverlaps insts
    when (length overlappingInstances /= 0) $
        fail $ pretty 80 $
        text "Overlapping" <+> stack (map ppr overlappingInstances)
  where
    instanceOverlaps :: Instance r -> m Bool
    instanceOverlaps inst = overlap tycon tys (inst_tycon inst) (inst_tys inst)
\end{code}

\section{Checking class and instance declarations}

\begin{code}
checkForExtraDefaults ::  forall r m . MonadTc r m
                      =>  Class r
                      ->  m ()
checkForExtraDefaults cls = do
    when (length extraFuns /= 0) $
        fail $  "Extra default member definitions for " ++
                (pretty 80 $ commasep $ map ppr extraFuns)
  where
    funs :: [H.Var]
    funs = binders (cls_binds cls)

    sigs :: [(H.Var, H.Type)]
    sigs = sigBindings (cls_binds cls)

    extraFuns :: [H.Var]
    extraFuns = funs \\ map fst sigs
\end{code}

\begin{code}
checkClassDeclaration  ::  forall r m . MonadTc r m
                       =>  Class r
                       ->  m (m [F.Decl])
checkClassDeclaration cls = do
    checkForExtraDefaults cls
    sel_tys <- mapM classSelectorTy [ty | (_, ty) <- sigs]
    traceClass $ nest 4 $
        text "Class members for" <+> ppr (cls_tycon cls)
        </> stack [ppr v <+> text "::" <+> ppr ty
                       | ((v, _), ty) <- sigs `zip` sel_tys]
    forM (sigs `zip` sel_tys) $ \((v, _), ty) ->
        insertVar v ty
    return $ do
        ftvs       <-  coerceAst (cls_tvs cls)
        fks        <-  compress (cls_ks cls) >>= coerceAst
        let dsels  =   cls_dictselectors cls
        (sels, sel_ftys, sel_dict_ftys) <- liftM unzip3 $
            mapM classSelector sigs
        (dsel_ftys, dsel_dict_ftys) <- liftM unzip $
            mapM (classDictSelector ftvs fks) (cls_ctx cls)
        traceClass $ nest 4 $
            text "Class selectors for" <+> ppr (cls_tycon cls)
            </> stack [ppr v <+> text "::" <+> ppr ty
                           |  (v, ty) <- (dsels ++ sels)
                              `zip` (dsel_ftys ++ sel_ftys)]
        return $ classDecls  ftvs fks
                             (dsel_dict_ftys ++ sel_dict_ftys)
                             (dsels ++ sels)
                             (dsel_ftys ++ sel_ftys)
  where
    sigs :: [(H.Var, H.Type)]
    sigs = sigBindings (cls_binds cls)

    classSelectorTy  ::  H.Type
                     ->  m (Type r)
    classSelectorTy ty = do
        (_, ctx, qty)  <-  return destructForAll `ap` coerceAst ty
        let tvs        =   sort $ Set.toList $ free [cls_pred cls] `Set.union` free qty
        let kvs        =   repeat (:*)
        return $ ForAll (tvs `zip` kvs) (cls_pred cls : ctx) qty

    classSelector  ::  (H.Var, H.Type)
                   ->  m (F.Var, F.Type, F.Type)
    classSelector (hsv, hsty) = do
        (_, ctx, qty)  <-  return destructForAll `ap` coerceAst hsty
        let tvs        =   sort $ Set.toList $ free [cls_pred cls] `Set.union` free qty
        let kvs        =   repeat (:*)
        let ty         =   ForAll (tvs `zip` kvs) (cls_pred cls : ctx) qty
        gv             <-  coerceAst hsv
        gty            <-  coerceAst ty
        dict_gty       <-  coerceAst qty
        return (gv, gty, dict_gty)

    classDictSelector  ::  [F.TyVar]
                       ->  [F.Kind]
                       ->  Pred r
                       ->  m (F.Type, F.Type)
    classDictSelector ftvs fks dict = do
        dict_gty  <-  coerceAst dict
        let gty   =   F.forallsT  (ftvs `zip` fks)
                                  ((cls_dict_ty cls) F.--> dict_gty)
        return (gty, dict_gty)

    classDecls  ::  [F.TyVar] -> [F.Kind]
                ->  [F.Type]
                ->  [F.Var] -> [F.Type]
                ->  [F.Decl]
    classDecls  ftvs fks
                dict_ftys
                sels sel_ftys =
        classDataDecl ++
        [classSelectorDecl sel sel_ty i
             | (sel, sel_ty, i) <- zip3 sels sel_ftys [1..]]
      where
        classDataDecl :: [F.Decl]
        classDataDecl =
            [F.dataD (cls_dict_tycon cls) data_k
                  [F.conD (cls_dict_con cls) con_gty []]]
          where
            data_k   =  foldr (F.:=>) (F.:*) fks
            con_gty  =  F.forallsT  (ftvs `zip` fks)
                                    (F.funsT dict_ftys (cls_dict_ty cls))

        classSelectorDecl  ::  F.Var -> F.Type -> Int -> F.Decl
        classSelectorDecl sel sel_ty i =
            F.letD (NonRec $
            F.bindD sel sel_ty
            (F.tylamsE  (ftvs `zip` fks)
                        (F.lamE dict (cls_dict_ty cls) $
                          F.caseE (F.varE dict) dict
                           [F.Alt  (F.conP (cls_dict_con cls) [] dictcon_args)
                                   (F.varE x)])))
          where
            dict, x :: F.Var
            dict  = F.var "dict"
            x     = F.var "x"

            dictcon_args :: [(F.WildVar, F.Type)]
            dictcon_args = [(if i == j then F.TameVar x else F.WildVar, ty)
                             | (j, ty) <- [1..] `zip` dict_ftys]
\end{code}

\begin{code}
checkForExtraMembers  ::  forall r m . MonadTc r m
                      =>  Class r
                      ->  Instance r
                      ->  m ()
checkForExtraMembers cls inst =
    when (length extraFuns /= 0) $
        fail $  "Extra member definitions for " ++
                (pretty 80 $ commasep $ map ppr extraFuns)
  where
    funs :: [H.Var]
    funs = binders (inst_decls inst)

    sigs :: [(H.Var, H.Type)]
    sigs = sigBindings (cls_binds cls)

    extraFuns :: [H.Var]
    extraFuns = funs \\ map fst sigs
\end{code}

\begin{code}
checkInstanceKind  ::  forall r m . MonadTc r m
                   =>  Class r
                   ->  Instance r
                   ->  m ()
checkInstanceKind cls inst = do
    extendTyVars (inst_hstvs inst `zip` inst_ks inst) $
        forM (inst_hstys inst `zip` cls_ks cls) $ \(ty, k) ->
            checkTypeKind ty k
    return ()
\end{code}

\begin{code}
instDictParams  ::  forall r m . MonadTc r m
                =>  Class r
                ->  Instance r
                ->  m ([F.Var], [F.Type], [F.Exp])
instDictParams cls inst = do
    theta               <-  match Map.empty (inst_pred inst) (cls_pred cls)
                              :: m (Map.Map TyVar (Type r))
    inst_opctx          <-  extendContext (inst_ctx inst) Nothing
    let super_ctx       =   subst theta Set.empty (cls_ctx cls)
    super_opctx         <-  extendContext super_ctx Nothing
    checkEntailment inst_opctx super_opctx
    getContext >>= reduce
    theta               <-  getPlaceholders
    let dict_params     =   [v | OpPred _ v <- inst_opctx]
    let super_params    =   [v | OpPred _ v <- super_opctx]
    dict_param_tys      <-  coerceAst (inst_ctx inst)
    let superdicts      =   subst theta Set.empty [F.varE v | v <- super_params]
    setContext []
    traceClass $ nest 4 $
        text "instance"
        <+> ppr (foldl' AppTy (TyConTy (inst_tycon inst)) (inst_tys inst))
        </> text "      inst_ctx:" <+> ppr (inst_ctx inst)
        </> text "    inst_opctx:" <+> ppr inst_opctx
        </> text "     super_ctx:" <+> ppr super_ctx
        </> text "   super_opctx:" <+> ppr super_opctx
        </> text "         theta:" <+> ppr (Map.toList theta)
        </> text "   dict_params:" <+> ppr dict_params
        </> text "dict_param_tys:" <+> ppr dict_param_tys
        </> text "    superdicts:" <+> ppr superdicts
    return (dict_params, dict_param_tys, superdicts)
\end{code}

\begin{code}
checkInstDeclaration  ::  forall r m . MonadTc r m
                      =>  Class r
                      ->  Instance r
                      ->  m (m [F.Decl])
checkInstDeclaration cls inst = do
    checkForExtraMembers cls inst
    checkInstanceKind cls inst
    bindings   <-  forM classMembers $ \(v, _) ->
                       findDefinition v >>= rename
    fbindings  <-  forM [v | H.VarBind v _ _ _ <- bindings] $ \v ->
                       coerceAst v
    traceClass $ nest 4 $
        text "instance"
        <+> ppr (foldl' AppTy (TyConTy (inst_tycon inst)) (inst_tys inst))
        </> stack (map ppr bindings)
    (dict_params, dict_param_tys, superdicts)  <-  instDictParams cls inst
    let bindingGroups  =   analyzeBindingDependencies bindings
    (mgbindgroup, _)   <-  checkBindingGroups bindingGroups $ return ()
    return $ do
        bindings      <-  sequence mgbindgroup
        dictfBinding  <-  dictFunction  dict_params dict_param_tys
                                        superdicts fbindings
        return $ [F.letD decl |
            decl <- reanalyzeDependencies (NonRec dictfBinding : bindings)]

  where
    dictFunction  ::  [F.Var] ->  [F.Type] -> [F.Exp]
                  ->  [F.Var]
                  ->  m F.Binding
    dictFunction dict_params dict_param_tys superdicts selectors = do
      ftvs               <-  coerceAst (inst_tvs inst)
      fks                <-  compress (inst_ks inst) >>= coerceAst
      ftys               <-  coerceAst (inst_tys inst)
      inst_dict_ty       <-  coerceAst (inst_pred inst)
      let dict_param_es  =   [F.varE v | v <- dict_params]
      let dictf_gty      =   F.forallsT (ftvs `zip` fks) $
                             F.funsT dict_param_tys inst_dict_ty
      let fselectors     =   map  (\v ->
                                      F.appsE  (F.tyappsE  (F.varE v)
                                                           [F.tyVarT ftv | ftv <- ftvs])
                                               dict_param_es)
                                  selectors
      let body           =    F.tylamsE  (ftvs `zip` fks)
                                         (F.lamsE (dict_params `zip` dict_param_tys) $
                                  F.appsE  (F.tyappsE (F.conE (cls_dict_con cls)) ftys)
                                           (superdicts ++ fselectors))
      traceClass $  text "dictFunction:"
                    <+> ppr (inst_dictf inst)
                    <+> ppr (cls_ctx cls)
                    <+> ppr (inst_ctx inst)
      return $ F.bindD (inst_dictf inst) dictf_gty body

    classMembers :: [(H.Var, H.Type)]
    classMembers = sigBindings $ cls_binds cls

    defaultDefinitions :: [(H.Var, H.Binding)]
    defaultDefinitions = varBindings $ cls_binds cls

    instanceDefinitions :: [(H.Var, H.Binding)]
    instanceDefinitions =  varBindings $ bindingsFromDecls $
                           inst_decls inst
                           ++ [H.SigDecl [v] ty internalLoc | (v, ty) <- classMembers]

    findDefinition :: H.Var -> m H.Binding
    findDefinition v =
      case lookup v instanceDefinitions of
        Just bind  -> return bind
        Nothing    -> case lookup v defaultDefinitions of
                        Just bind  -> return bind
                        Nothing    -> fail $ "No definition for " ++ show v

    theta :: Map.Map H.TyVar H.Type
    theta = Map.fromList ((cls_hstvs cls) `zip` (inst_hstys inst))

    rename :: H.Binding -> m H.Binding
    rename (H.VarBind v (Just ty) fix rhs) = do
        n' <- instanceMemberName v (inst_tys inst)
        traceClass $ nest 4 $
          text "rename:"
          </> ppr v <+> text "::" <+> ppr ty
          </> ppr v <+> text "::" <+> ppr ty'
        return $ H.VarBind (H.Var n') (Just ty') fix rhs
      where
        (tvs, ctx, qty) = H.destructForAll (subst theta (free theta) ty)
        ty' = H.ForAll H.ImplicitForAll tvs (inst_hsctx inst ++ ctx) qty

    rename binding =
        panic $
        text "checkInstDeclaration: bad binding" <+> ppr binding
\end{code}
