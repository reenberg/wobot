%format sigma'
%format sigma1
%format sigma2
%format rho'
%format rho1
%format rho2

%if False
\begin{code}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
-- Module      :  Check.Hs.Check
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Check.Hs.Check where

import Control.Monad.State

import Control.Monad (mapAndUnzipM,
                      replicateM)
import Control.Monad.ContextException
import Control.Monad.Exception
import Control.Monad.Ref
import Control.Monad.Trace
import Data.Ord (comparing)
import Data.List ((\\),
                  foldl',
                  unzip4,
                  zip4)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Compiler.Opt
import qualified Check.F.Builtin as F.Builtin
import Check.Hs.Bindings
import Check.Hs.Builtin
import Check.Hs.Classes
import Check.Hs.Exceptions
import Check.Hs.Monad
import Check.Hs.Types
import Data.Loc as Set
import Data.Name
import qualified Language.F as F
import qualified Language.Hs as H
import Match
import Match.Wadler
import Text.PrettyPrint.Mainland

import Util
\end{code}
%endif

\section{Type Checking}

The code here is based heavily on~\cite{jones07practical}.

\begin{code}
data Expected r a  =  Infer (r a)
                   |  Check a

readExpected :: MonadRef r m => Expected r a -> m a
readExpected (Infer r)   = readRef r
readExpected (Check ty)  = return ty
\end{code}

\subsection{Checking Top-level Declarations}

\begin{code}
checkTopDecls  ::  MonadTc r m
               =>  [H.Decl]
               ->  m [F.Decl]
checkTopDecls topdecls = do
    checkDuplicates (text "multiple declarations of type constructor")
        (binders typedecls :: [H.TyCon])
    checkDuplicates (text "multiple declarations of constructor")
        (binders typedecls :: [H.Con])
    checkDuplicates (text "multiple type signatures for")
        (sigBinders valdecls)
    checkDuplicates (text "multiple declarations of")
        (binders bindings ++ binders typedecls :: [H.Var])
    mapM_ addSynonyms typedeclGroups
    mapM_ addClasses typedeclGroups
    mapM_ addInstance topdecls
    sigbinds           <-  mapM sigBinding freeSigs
    gtypedecls         <-  mapM kcTypeDeclGroup typedeclGroups
    (mgbindings, env)  <-  extendVars sigbinds $
                           checkBindingGroups bindingGroups $
                           getTcEnv
    putTcEnv env
    mginstdecls        <-  processNewInstances checkInstDeclaration
    gsigdecls          <-  mapM coerceSig sigbinds
    gtopdecls          <-  sequence mgbindings
    ginstdecls         <-  sequence mginstdecls
    return $  gsigdecls
          ++  concat gtypedecls
          ++  map (\d -> F.LetDecl d internalLoc) gtopdecls
          ++  concat ginstdecls
  where
    typedeclGroups :: [TypeDef]
    typedeclGroups = analyzeTypeDeclDependencies typedecls

    bindingGroups :: [Rec H.Binding]
    bindingGroups = analyzeBindingDependencies bindings

    bindings :: [H.Binding]
    bindings = bindingsFromDecls valdecls

    boundVars :: [H.Var]
    boundVars = binders bindingGroups

    freeSigs :: [(H.Var, H.Type)]
    freeSigs =  filter (\(v, _) -> not (v `elem` boundVars)) sigs

    typedecls :: [H.Decl]
    typedecls = H.typeDeclarations topdecls

    valdecls :: [H.Decl]
    valdecls = H.valueDeclarations topdecls

    sigs :: [(H.Var, H.Type)]
    sigs = H.sigDeclarations topdecls

    sigBinding  ::  MonadTc r m
                =>  (H.Var, H.Type)
                ->  m (H.Var, Sigma r)
    sigBinding (v, ty) =
        do  sigma <- toSigma ty
            return (v, sigma)

    coerceSig  ::  MonadTc r m
               =>  (H.Var, Type r)
               ->  m F.Decl
    coerceSig (v, ty) =
        do  gv   <- coerceAst v
            gty  <- compress ty >>= coerceAst
            return $ F.SigDecl gv gty internalLoc
\end{code}

\subsection{Type Synonyms}

Our approach to expanding synonyms is slightly different than GHC's. It is
motivated by wanting to maintain the following invariant: no inferred type
should ever contain a synonym, i.e., only user-supplied types will ever contain
a synonym. This requires that class constraints never contain a synonym. Let us
illustrate some of the difficulties with an example where GHC fails to maintain
the invariant:

\begin{spec}
{-# LANGUAGE FlexibleContexts #-}

class Bar a where
    bar :: a -> Foo a
    bar = baz

data (Bar [a]) => Foo a = Foo a

baz :: Bar a => a -> Foo a
baz = Foo

instance Bar (Id a) => Bar [a] where
    bar = undefined

type Id a = a
\end{spec}

Consider the kind-checking step in which the data type |Foo| is kind
checked~\citep[see][\S4.6]{haskell98report}. |Foo| and |Bar| form a dependency
group, but neither depends on the synonym |Id| in a way that can be determined
syntactically, so it is not included in the dependency group. However, |Foo|
\emph{does} depend on |Id| via the instance declaration for |Bar [a]|. GHC
doesn't know that, so it infers the type |(Bar (Id a)) => a -> Foo a| for |Foo|
instead of |(Bar a) => a -> Foo a|.

I believe the solution to the problem of maintaining the above invariant is to
record all synonyms as the first step in type-checking a file. Then, during type
checking, simply expand all synonyms. During kind checking, ignore synonyms
since we'll still catch any kinding errors once the synonyms have been
expanded. We still need to check the following conditions:

\begin{itemize}

\item No duplicate type variables: a synonym's arguments must be linear.

\item No free type variables on the right-hand side of the synonym
definition. It \emph{is} legal to have a synonym take type variable arguments
that are unused!

\item No cyclic synonyms: there are no declaration dependency groups containing
synonyms without any data declarations to break the cycle, and a synonym can't
refer to itself.

\item Synonyms are always fully applied. We check this during synonym expansion.

\end{itemize}

\begin{code}
addSynonyms  ::  forall r m . MonadTc r m
             =>  TypeDef
             ->  m ()
addSynonyms (TypeDef decl) =
    addSynonym decl

addSynonyms (RecTypeDef (typedecls, datadecls, _)) = do
    when (length datadecls == 0) $
        throwException $ CyclicSynonyms typedecls
    mapM_ addSynonym typedecls
\end{code}

\begin{code}
addSynonym  ::  forall r m . MonadTc r m
          =>  H.Decl
          ->  m ()
addSynonym (H.TypeDecl tycon tvs ty _) = do
    checkDuplicates (text "duplicate type variable") tvs
    case free_tvs of
         []  -> return ()
         _   -> throwException $ UnboundSynonymTypeVariables tycon free_tvs
    insertSynonym tycon (tvs, ty)
  where
    free_tvs :: [H.TyVar]
    free_tvs = Set.toList (free ty) \\ tvs

addSynonym _ =
    return ()
\end{code}

\subsection{Kind Checking a Type Declaration Dependency Group}

We kind check declarations as a dependency group---all type declarations that
depend on one another must be in the same dependency group. Each data
constructor in a dependency group is bound to the most general kind it can
possibly have. For example, consider the type declaration:

\begin{spec}
data List a  =  Nil
             |  Cons a (List a)
\end{spec}

\noindent The type constructor |List| will be assigned the kind $k_1 \to {*}$
where $k_1$ is a kind meta-variable that will be filled in by the kind inference
procedure.

Kind checking for a type declaration dependency group is performed by the
|kcTypeDeclGroup| function. Each type constructor and class is bound to the most
general kind it can have, as outlined above; note that we do not bind type
declarations to their kind in a new scope because we're operating at top-level.
Then we perform kind inference on each declaration. This process may not assign
a kind to every kind metavariable, so the last step is to ``zonk'' un-assigned
metavariables, assigning them the kind ${*}$. Zonking a kind is just like path
compression, except unassigned metavariables are assigned the (default) kind
${*}$.

\begin{code}
zonkKind  ::  MonadTc r m
          =>  Kind r
          ->  m (Kind r)
zonkKind k@(:*)             = return k
zonkKind (MetaKv metak)     = do  maybe_k <- readKv metak
                                  case maybe_k of
                                    Nothing  -> do  writeKv metak (:*)
                                                    return (:*)
                                    Just k   -> do  k' <- zonkKind k
                                                    writeKv metak k'
                                                    return k'
zonkKind (k1 :=> k2)        = do  k1' <- zonkKind k1
                                  k2' <- zonkKind k2
                                  return $ k1' :=> k2'
\end{code}

\begin{code}
kcTypeDeclGroup  ::  forall r m . MonadTc r m
                 =>  TypeDef
                 ->  m [F.Decl]
kcTypeDeclGroup (TypeDef (H.TypeDecl _ _ _ _)) =
    return []

kcTypeDeclGroup (TypeDef decl@(H.DataDecl _ _ _ _ _ _ _)) = do
    (tycon, k, cons, binds)  <- dataDeclBinding decl
    mgtypedecls              <- kcDataDecl decl k binds
    zonkKind k
    insertTyCon tycon cons k
    gtypedecls <- mgtypedecls
    return gtypedecls

kcTypeDeclGroup (TypeDef decl@(H.ClassDecl _ _ _ _ _)) = do
    mgclassdecls  <-  kcClassDecl decl
    gclassdecls   <-  mgclassdecls
    return gclassdecls

kcTypeDeclGroup (TypeDef _) =
    panic $ text "saw non-type declaration"
\end{code}

\begin{code}
kcTypeDeclGroup (RecTypeDef (_, datadecls, classdecls)) = do
\end{code}

\begin{code}
    recursiveTypes <- optVal (isFlagSet Opt_RecursiveTypes)
    when (not recursiveTypes) $
        checkIllegalRecursion  (text "illegal recursive data declarations")
                               (binders datadecls :: [H.TyCon])
\end{code}

\begin{code}
    dataBinds <- mapM dataDeclBinding datadecls
    forM_ dataBinds $
        \(tycon, k, cons, _) -> insertTyCon tycon cons k
    mgtypedecls <- forM (datadecls `zip` dataBinds) $
        \(decl, (_, k, _, tvks)) -> kcDataDecl decl k tvks
    mapM_ zonkKind [k | (_, k, _, _) <- dataBinds]
    gtypedecls <- sequence mgtypedecls
\end{code}

\begin{code}
    mgclassdecls  <- mapM kcClassDecl classdecls
    gclassdecls   <- sequence mgclassdecls
\end{code}

\begin{code}
    return $ concat $ gtypedecls ++ gclassdecls
\end{code}

For data declarations we track the kind of the data type, its associated
constructors, and bindings from its type variables to their kinds.

\begin{code}
dataDeclBinding  ::  MonadTc r m
                 =>  H.Decl
                 ->  m (H.TyCon, Kind r, [H.Con], [(H.TyVar, Kind r)])
\end{code}

Before binding a type constructor to its kind, the type variables are checked to
make sure that they are linear, e.g., that there are no duplicated type
variables. Then we create a new kind metavariable $k_1, \ldots, k_n$ for each
type variable $\tau_1, \ldots, \tau_n$; the type constructor then has the kind
$k_1 \to \ldots \to k_n \to {*}$.

\begin{code}
dataDeclBinding (H.DataDecl _ _ tycon tvs condecls _ _) = do
    checkDuplicates (text "duplicate type variable") tvs
    kvs <- mapM (const newMetaKv) tvs
    let k = foldr (:=>) (:*) kvs
    return $ (tycon, k, map getCon condecls, tvs `zip` kvs)
  where
    getCon :: H.ConDecl -> H.Con
    getCon (H.ConDecl con _ _ _)         = con
    getCon (H.OpConDecl _ con _ _ _)     = con
    getCon (H.RecConDecl con _ _ _)      = con

dataDeclBinding _ =
    panic $ text "encountered non-data declaration"
\end{code}

\subsubsection{Kind Checking a Data Declaration}

The real work is done by the |kcDataDecl| function, which kind checks a single
type declaration. By the time it's called, all type contructors in the type
declaration's dependency group have been bound to a kind, so we can just look
them up in the current environment. We can't return a |[F.Decl]| directly
because at this point not all type and kind variables have been resolved, so we
instead return a |m [F.Decl]|. Data declarations may generate more than one
corresponding G declaration due to the presence of field labels.

\begin{code}
kcDataDecl  ::  MonadTc r m
            =>  H.Decl
            ->  Kind r
            ->  [(H.TyVar, Kind r)]
            ->  m (m [F.Decl])
\end{code}

To kind check a data type declaration, we extend the current environment with
the bindings from the type constructor's type variables and their kinds and then
kind check each constructor in the extended environment. We then instantiate the
type constructor's kind at the expected kind. Checking a constructor returns the
constructor and its sigma type, which we bind in the current environment (not
the extended environment in which we kind checked the constructor because then
we couldn't ever use it!).

\begin{code}
kcDataDecl (H.DataDecl _ _ tycon tvs cons _ _) k tvks = do
    (conBindings, mgcondecls) <-  extendTyVars tvks $
                                  mapAndUnzipM (checkCon tycon tvs) cons
    mapM_ (uncurry insertCon) conBindings
    labels <-  liftM concat $ sequence $
               zipWith labelBindings cons (map snd conBindings)
    mglbldecls <- checkConLabels conBindings labels
    return $ do
        gtycon     <- coerceAst tycon
        gk         <- compress k >>= coerceAst
        gcondecls  <- sequence mgcondecls
        glbldecls  <- mglbldecls
        return $ F.DataDecl gtycon gk gcondecls internalLoc : glbldecls
  where
\end{code}

The |checkCon| function extracts the constructor name and constituent types from
a constructor declaration. It does not do any checking of labels---this is done
in the |kcDataDecl| function. Note that we should never see a tuple constructor
here.

\begin{code}
    checkCon  ::  MonadTc r m
              =>  H.TyCon
              ->  [H.TyVar]
              ->  H.ConDecl
              ->  m ((H.Con, Sigma r), m F.ConDecl)
    checkCon tycon tvs (H.ConDecl con@(H.Con _) _ tys _) =
        genCheckCon tycon tvs con tys Nothing

    checkCon tycon tvs (H.OpConDecl ty1 con@(H.Con _) _ ty2 _) =
        genCheckCon tycon tvs con [ty1, ty2] Nothing

    checkCon tycon tvs (H.RecConDecl con@(H.Con _) _ flds _) =
        genCheckCon tycon tvs con (map snd flds) (Just $ map fst flds)

    checkCon _ _ _ =
        panic $ text "cannot redefine tuple constructor"
\end{code}

The |genCheckCon| function checks a constructor definition and returns a binding
from the constructor to its sigma type. First we expand any synonyms. Each
argument to a constructor must have kind ${*}$, so we first ensure that this is
the case by calling |checkTypeKind| for each of the constructor's arguments. We
then convert the type constructor's type variables from Haskell abstract syntax
to type variables in the type checker's representation, and also convert the
types to which the constructor is applied---its arguments---to the type
checker's representation for types. The result type of the constructor is the
type constructor applied to all type variable arguments. To construct the sigma
type of the constructor we fold the function constructor over the argument types
and quantify over the type variables.

\begin{code}
    genCheckCon  ::  MonadTc r m
                 =>  H.TyCon
                 ->  [H.TyVar]
                 ->  H.Con
                 ->  [H.Type]
                 ->  Maybe [H.Var]
                 ->  m ((H.Con, Sigma r), m F.ConDecl)
    genCheckCon tycon tvs con tys maybe_flds = do
        expandedTys <- mapM expandSynonyms tys
        mapM_ (\ty -> checkTypeKind ty (:*)) expandedTys
        tctvs    <- mapM coerceAst tvs
        tvks     <- mapM lookupTyVar tvs
        argtys   <- mapM coerceAst expandedTys
        tctycon  <- coerceAst tycon
        let res_ty =  foldAppTy (TyConTy tctycon) (map TyVarTy tctvs)
        let con_ty =  ForAll (tctvs `zip` tvks) [] $
                      foldr (-->) res_ty argtys
        let gcondecl = do  gcon     <- coerceAst con
                           gcon_ty  <- compress con_ty >>= coerceAst
                           gflds    <- case maybe_flds of
                                         Nothing    -> return []
                                         Just flds  -> coerceAst flds
                           return $ F.ConDecl gcon gcon_ty gflds internalLoc
        return ((con, con_ty), gcondecl)
\end{code}

\begin{code}
    labelBindings  ::  MonadTc r m
                   =>  H.ConDecl
                   ->  Sigma r
                   ->  m [(H.Var, H.Con, Sigma r)]
    labelBindings  (H.ConDecl _ _ _ _)      _ = return []
    labelBindings  (H.OpConDecl _ _ _ _ _)  _ = return []

    labelBindings (H.RecConDecl (H.TupleCon _) _ _ _) _ =
        panic $ text "should never see a tuple constructor"

    labelBindings (H.RecConDecl con@(H.Con _) _ flds _) sigma = do
        let (quals, _, fld_tys, res_ty)  = destructForAllFunTy sigma
        when (length fld_tys /= length flds) $
            panic $ text "bad constructor type"
        checkDuplicates (text "duplicate definition of record label")
            (map fst flds)
        let labels     = map fst flds
        let label_tys  = map (\ty -> ForAll quals [] (res_ty --> ty)) fld_tys
        return $ zip3 labels (repeat con) label_tys
\end{code}

\begin{code}
    checkConLabels  ::  forall r m . MonadTc r m
                    =>  [(H.Con, Sigma r)]
                    ->  [(H.Var, H.Con, Sigma r)]
                    ->  m (m [F.Decl])
    checkConLabels cons labels = do
        mapM_ checkLabelDecl labelDecls
        gselectors <- mapM  (\(lbl, sel_ty) -> trSelector lbl sel_ty
                                               cons conLabels labelCons)
                            (map head labelDecls)
        forM (map head labelDecls) $ \(v, sigma) ->
            insertVar v sigma
        insertConLabels $ Map.toList conLabels
        insertLabelCons $ Map.toList labelCons
        return $ sequence gselectors
      where
        extend :: Ord k => k -> a -> Map.Map k [a] -> Map.Map k [a]
        extend k a m = Map.insert k (a : Map.findWithDefault [] k m) m

        conLabels :: Map.Map H.Con [H.Var]
        conLabels =  foldr (\(fld, con, _) m -> extend con fld m)
                     Map.empty labels

        labelCons :: Map.Map H.Var [H.Con]
        labelCons =  foldr (\(fld, con, _) m -> extend fld con m)
                     Map.empty labels

        labelDecls :: [[(H.Var, Sigma r)]]
        labelDecls =  equivalence (comparing fst) $
                      (map (\(lbl, _, lbl_ty) -> (lbl, lbl_ty)) labels)

        checkLabelDecl :: [(H.Var, Sigma r)] ->  m ()
        checkLabelDecl labels = do
            let (fs, (ty : tys)) = unzip labels
            let binds = map (\b -> (bindingName b, bindingLoc b)) fs
            when (not $ all (== ty) tys) $
                throwException $
                Duplicates  (text "incompatible label declarations")
                            [binds]
\end{code}

\begin{code}
kcDataDecl _ _ _ =
    panic $ text "encountered non-data declaration"
\end{code}

\subsubsection{Kind Checking a Class Declaration}

\begin{code}
kcClassDecl  ::  MonadTc r m
             =>  H.Decl
             ->  m (m [F.Decl])
\end{code}

\begin{code}
kcClassDecl (H.ClassDecl _ tycon _ decls _) = do
    tycon'  <-  coerceAst tycon
    cls     <-  lookupClass tycon'
    extendTyVars (cls_hstvs cls `zip` cls_ks cls) $
        forM (map snd sigs) $ \ty ->
            checkTypeKind ty (:*)
    mapM_ zonkKind (cls_ks cls)
    checkClassDeclaration cls
  where
    sigs :: [(H.Var, H.Type)]
    sigs = H.sigDeclarations decls
\end{code}

\begin{code}
kcClassDecl _ =
    panic $ text "encountered non-class declaration"
\end{code}

\subsection{Kind Checking a Type}

Kind checking a type is almost trivial. \invariant{|kcType| is never called with
a type containing synonyms since they have already been expanded}.

\begin{code}
kcType  ::  MonadTc r m
        =>  H.Type
        ->  Expected r (Kind r)
        ->  m ()
\end{code}

For a non-tuple type constructor we look up the type constructor in the current
environment and instantiate its kind.

\begin{code}
kcType (H.TyConTy tycon) exp_k = do
    k <- lookupTyCon tycon
    instKind k exp_k
\end{code}

Type variables are another pedestrian case---just look up their kind in the
environment.

\begin{code}
kcType (H.TyVarTy tyvar) exp_k = do
    k <- lookupTyVar tyvar
    instKind k exp_k
\end{code}

The only interesting case is type application. We first infer a kind for the
type function being applied and extract the argument and result kinds. We then
check that the argument has the same kind as the argument we just inferred for
the type function and instantiate the expected kind for the whole type
expression with the kind of the type function's result.

\begin{code}
kcType (H.AppTy fun arg) exp_k = do
    fun_kind <- inferTypeKind fun
    (arg_kind, res_kind) <- unifyTyFun fun_kind
    checkTypeKind arg arg_kind
    instKind res_kind exp_k
\end{code}

\begin{code}
kcType (H.ForAll _ _ _ ty) exp_k =
    kcType ty exp_k
\end{code}

\begin{code}
kcType (H.AntiType _) _ =
    panic $ text "AntiType"
\end{code}

We have the familair |check| and |infer| function which are just wrappers for
|kcType|.

\begin{code}
checkTypeKind  ::  MonadTc r m
               =>  H.Type
               ->  Kind r
               ->  m ()
checkTypeKind ty k = kcType ty (Check k)

inferTypeKind  ::  MonadTc r m
               =>  H.Type
               ->  m (Kind r)
inferTypeKind ty = do
    ref <- newRef (internalErr $ text "inferType: empty result")
    kcType ty (Infer ref)
    readRef ref
\end{code}

\subsection{Instantiating a Kind}

Instantiating a kind is simple since we don't have polymorphic kinds. When
checking, we unify the instantiated kind and the expected kind, and when
inferring we just update the expected kind's ref.

\begin{code}
instKind  ::  MonadTc r m
          =>  Kind r
          ->  Expected r (Kind r)
          ->  m ()
instKind k  (Check exp_k)  = unify k exp_k
instKind k  (Infer r)      = writeRef r k
\end{code}

\subsection{Type Checking a Declaration Dependency Group}

The |checkDecls| function type checks a group of declarations and then calls a
continuation action in an environment that has been extended with the
declarations' definitions. The declarations are first split into dependency
groups.

\begin{code}
checkDecls  ::  MonadTc r m
            =>  [H.Decl]
            ->  m a
            ->  m ([m (Rec F.Binding)], a)
checkDecls decls m = do
    checkDuplicates (text "multiple type signatures for")
       (sigBinders decls)
    checkDuplicates (text "multiple declarations of")
       (binders bindings :: [H.Var])
    (gbindings, a) <- checkBindingGroups bindingGroups m
    return (gbindings, a)
  where
    bindingGroups :: [Rec H.Binding]
    bindingGroups = analyzeBindingDependencies bindings

    bindings :: [H.Binding]
    bindings = bindingsFromDecls decls
\end{code}

\begin{code}
class SigBinder a where
    sigBinders :: a -> [H.Var]

instance SigBinder a => SigBinder [a] where
    sigBinders as = concatMap sigBinders as

instance SigBinder H.Decl where
    sigBinders  (H.ClassDecl _ _ _ decls _)  = sigBinders decls
    sigBinders  (H.SigDecl ns _ _)           = ns
    sigBinders  _                             = []
\end{code}

\subsubsection{Checking Binding Groups}

The |checkBindingGroups| function takes the dependency groups and type checks
each successive group in an environment that has been extended with its
predecessor groups' definitions. The last thing it does is call the continuation
action |m|.

\begin{code}
checkBindingGroups  ::  MonadTc r m
                    =>  [Rec H.Binding]
                    ->  m a
                    ->  m ([m (Rec F.Binding)], a)
checkBindingGroups [] m = do
    a <- m
    return ([], a)

checkBindingGroups [b] m = do
    (gdecl, a) <- checkBindingGroup b m
    return ([gdecl], a)

checkBindingGroups (b : bs) m = do
    (gdecl, (gdecls, a)) <-  checkBindingGroup b $
                             checkBindingGroups bs m
    return (gdecl : gdecls, a)
\end{code}

The actually type checking of a binding group is done by the |checkBindingGroup|
function. Because the definitions in a binding group are mutually recursive, we
first extend the environment with bindings from each bound variable to its type
(which probably contains type meta-variables)---these bindings are generated by
the |varBinding| function. In this new environment we then type check each
binding individually. When each binding has been checked, we go back and
generalize the bound variables in the {\it original} environment. Finally, we
bind the properly generalized variables and call the continuation action, |m|.

\begin{code}
checkBindingGroup  ::  forall r m a . MonadTc r m
                   =>  Rec H.Binding
                   ->  m a
                   ->  m (m (Rec F.Binding), a)
checkBindingGroup (NonRec binding) m = do
    (v, ty) <- bindBinding binding
    traceTc $ text "non-recursive binding:" <+> ppr v <+> text "::" <+> ppr ty
    gbinding <-  checkBinding binding ty
    [(v, sigma, mgbinding)] <- generalizeBindings [gbinding]
    a <- extendVars [(v, sigma)] m
    return (return NonRec `ap` mgbinding, a)
\end{code}

\begin{code}
checkBindingGroup (Rec bindings) m = do
    binds <- mapM bindBinding bindings
    traceTc $ nest 4 $ text "recursive bindings:" </>
        stack (map (\(v, ty) -> ppr v <+> text "::" <+> ppr ty) binds)
\end{code}

If recursive functions are disallowed, flag an error if there are any function
bindings in the binding group.

\begin{code}
    recursiveFunctions <- optVal (isFlagSet Opt_RecursiveFunctions)
    when (not recursiveFunctions && length varBindings /= 0) $
        checkIllegalRecursion  (text "illegal recursive function declarations")
                               (binders varBindings :: [H.Var])
\end{code}

If we're in a strict setting, then we can't compile recursive variable bindings
as this would require laziness, so flag an error. \XXX{We need to check for
non-function (single-variable) bindings too.}

\begin{code}
    strict <- optVal (isFlagSet Opt_Strict)
    when (strict && length patBindings /= 0) $
        checkIllegalRecursion  (text "illegal recursive value bindings")
                               (binders patBindings :: [H.Var])
\end{code}

When generating G code, instantiate produces a coercion from one type to another
via type applications. The catch is that we can't know what the proper coercion
is until the type we're instantiating has been fully generalized. To ensure that
the coercion has access to the generalized type when it is run, we add an extra
level of indirection when we assign a variable an initial type prior to running
the type checker over a binding group. The coercion can then compress this type
to get the final, generalized type.

\begin{code}
    gbindings <-  extendVars binds $
                  forM (bindings `zip` binds) $ \(binding, (_, ty)) ->
                      checkBinding binding ty
    (vs, sigmas, mgbindings) <- return unzip3 `ap` generalizeBindings gbindings
\end{code}

Here is where we write a variable's generalized type into the |MetaTv|
associated with the variable so the coercion can use it.

\begin{code}
    forM (binds `zip` sigmas) $ \((_, ty), sigma) ->
        case ty of
          MetaTv mtv  -> writeTv mtv sigma
          _           -> return ()
\end{code}

\begin{code}
    a <- extendVars (vs `zip` sigmas) m
    return (return Rec `ap` sequence mgbindings, a)
  where
    varBindings :: [H.Binding]
    varBindings = [bind | bind@(H.VarBind _ _ _ _) <- bindings]

    patBindings :: [H.Binding]
    patBindings = [bind | bind@(H.PatBind _ _ _ _) <- bindings]
\end{code}

The |bindBinding| function takes a binding, possibly annotated with a type, and
returns a pair consisting of the variable being bound and the type to which it
is being bound. For annotated bindings, we return the sigma type corresponding
to the annotation. For un-annotated bindings, we need to be careful. Coercions,
which are generated when instantiating a variable at a particular type, need to
know both the type of the variable being instantiated and the type at which the
instantiation is ultimately used. The former isn't determined until a binding
group is generalized, but by then we've already generated the monadic actions
that perform the coercions. Our solution is to add an extra level of indirection
to the type of un-annotated bindings, via a meta-variable.

\begin{code}
bindBinding  ::  MonadTc r m
             =>  H.Binding
             ->  m (H.Var, Sigma r)
\end{code}

\begin{code}
bindBinding (H.FixityBind _ _)  = panic $ text "saw fixity binding"
bindBinding (H.SigBind _ _ _)   = panic $ text "saw sig binding"
\end{code}

We need to assign un-annotated variable bindings a type with type variables for
each argument type as well as for the return type because |checkBinding| makes
the assumption that a function's type has that form---this assumption simplified
the code there. That is, if the function to which we're giving a type has $n$
parameters, we give it the type $\tau_1 \to \ldots \to \tau_n \to \tau_{\rm
ret}$ where each $\tau$ is a fresh type meta-variable. We also calculate the
number of arguments to the function and make sure that each branch has the same
number of arguments. Note the extra level of indirection we mentioned earlier:
we create a meta type variable and stuff it into |ty|, but immediately write a
type to it. This allows the coercions we generated to determine the variable's
eventual generalized type by compressing |ty|.

\begin{code}
bindBinding (H.VarBind v Nothing _ alts) = do
    let nargs = length $ fst $ head alts
    when (not (all (== nargs) (map (length . fst) alts))) $
        throwException $ DifferingEquationArgCounts v
    arg_tys         <- replicateM nargs newMetaTv
    ret_ty          <- newMetaTv
    ty@(MetaTv tv)  <- newMetaTv
    writeTv tv $ foldr (-->) ret_ty arg_tys
    return (v, ty)
\end{code}

When a function binding is annotated, we convert the annotation to a sigma type,
which should be a function type, and then unfold the function type to make sure
that there are enough constituent types for the required number of arguments
plus the return type of the function.

\begin{code}
bindBinding (H.VarBind v (Just ty) _ alts) = do
    when (not (all (== nargs) (map (length . fst) alts))) $
        throwException $ DifferingEquationArgCounts v
    v_ty@(ForAll _ _ qty) <- toSigma ty
    let tys = unfoldFunTy qty
    when (length tys < nargs + 1) $
        throwException $ DifferingTypeArgCounts v
    return (v, v_ty)
  where
    nargs = length $ fst $ head alts
\end{code}

The cases for pattern bindings are both trivial.

\begin{code}
bindBinding (H.PatBind v Nothing _ _) = do
    tv <- newMetaTv
    return (v, tv)

bindBinding (H.PatBind v (Just ty) _ _) = do
    v_ty <- toSigma ty
    return (v, v_ty)
\end{code}

After translating binding groups, we need to generalize each binding. Remember
that we only need to generalize bindings that were {\it un}-annotated, as
described above. Fortunately we can just blindly generalize everything, which is
a no-op for annotated bindings. We also have to replace the sigma type in the
generated G code with the generalized type.

\begin{code}
generalizeBindings  ::  forall r m . MonadTc r m
                    =>  [(H.Var, Maybe H.Type, Rho r, F.Type -> m F.Exp)]
                    ->  m [(H.Var, Sigma r, m F.Binding)]
generalizeBindings gbindings = do
    rhos'           <-  compress rhos
    (sigmas, cos)   <-  return unzip `ap` generalize rhos'
    zipWithM_ annCheck sigmas maybe_tys
    let mgbindings  =   map mgbinding (zip4 vs sigmas mgbodys cos)
    return $ zip3 vs sigmas mgbindings
  where
    (vs, maybe_tys, rhos, mgbodys) = unzip4 gbindings

    annCheck :: Sigma r -> Maybe H.Type -> m ()
    annCheck  _       Nothing    = return ()
    annCheck  sigma1  (Just ty)  = do
        sigma2 <- toSigma ty
        ctx <- getContext
        subsCheck sigma1 sigma2
        setContext ctx
        return ()

    mgbinding  ::  (H.Var, Rho r, F.Type -> m F.Exp, F.Exp -> m F.Exp)
               ->  m F.Binding
    mgbinding (v, sigma, mgbody, co) = do
        gv      <- coerceAst v
        gsigma  <- compress sigma >>= coerceAst
        gbody   <- mgbody gsigma >>= co
        return $ F.Binding gv gsigma F.defaultBindInfo gbody internalLoc
\end{code}

\begin{code}
checkBinding  ::  MonadTc r m
              =>  H.Binding
              ->  Sigma r
              ->  m (H.Var, Maybe H.Type, Sigma r, F.Type -> m F.Exp)
\end{code}

\begin{code}
checkBinding (H.FixityBind _ _)  _  = panic $ text "saw fixity binding"
checkBinding (H.SigBind _ _ _)   _  = panic $ text "saw sig binding"
\end{code}

\begin{code}
checkBinding (H.VarBind v maybe_ty _ alts) sigma = do
    traceTc $ text "checking:" <+> ppr v <+> text "::" <+> ppr sigma
    (rho, _, _)      <-  instantiate sigma
    let tys          =   unfoldFunTy rho
    let arg_tys      =   take nargs tys
    let ret_ty       =   foldr1 (-->) (drop nargs tys)
    gus              <-  replicateM nargs (liftM F.Var $ uniqueName)
    galts            <-  forM alts $ \alt ->
                         checkBranch v alt (arg_tys, ret_ty)
    (gus', mgmatch)  <-  trMatch gus galts
    let body _ = do  gmatch   <- mgmatch
                     garg_tys <- mapM (\ty -> compress ty >>= coerceAst) arg_tys
                     return $ F.lamsE (gus' `zip` garg_tys) gmatch
    rho' <- compress rho
    traceTc $ text "checked:" <+> ppr v <+> text "::" <+> ppr rho'
    return $ (v, maybe_ty, rho', body)
  where
    nargs = length $ fst $ head alts
\end{code}

\begin{code}
checkBinding b@(H.PatBind v maybe_ty _ rhs) sigma = do
    traceTc $ text "checking:" <+> ppr b
    (rho, _, _)         <- instantiate sigma
    (mgdecls, gguards)  <- checkRhs rhs rho
    let body ty = do  gdecls  <- sequence mgdecls
                      gbody   <- trGuards gguards (F.Builtin.mkUndefined ty)
                      return $ foldr F.letE gbody gdecls
    return $ (v, maybe_ty, rho, body)
\end{code}

\subsubsection{Type Checking a Branch of a Function Definition}

If a branch has arguments, then it is a branch of a function, so we type check
the body in a new scope which removes any non-top-level bindings from the
environment. Otherwise it is a variable binding, so we don't open a new scope.

\begin{code}
checkBranch  ::  MonadTc r m
             =>  H.Var
             ->  ([H.Pat], H.Rhs)
             ->  ([Rho r], Rho r)
             ->  m (Equation r m)
checkBranch v (pats, rhs) (arg_tys, ret_ty) = do
    traceTc $ nest 4 $
        text "branch:"
        <+> spread (map ppr pats) <+> ppr rhs
        </> stack (map  (\(p, pty) -> ppr p <+> text "::" <+> ppr pty)
                        (pats `zip` arg_tys))
        </> text "::" <+> ppr ret_ty
    (gpats, bindings) <-  mapAndUnzipM  (uncurry checkPat)
                                        (pats `zip` arg_tys)
    let binds = concat bindings
    checkArgVarLinearity (map fst binds)
    (mgbindings, guards) <-  (case pats of
                                [] -> id
                                _  -> withNewScope) $
                             extendVars binds $
                             checkRhs rhs ret_ty
    let ggdexp = \cont -> do  gbindings <- sequence mgbindings
                              gbody <- trGuards guards cont
                              return $ foldr F.letE gbody gbindings
    return $ Equation (gpats `zip` arg_tys) ret_ty ggdexp
  where
    checkArgVarLinearity  ::  MonadTc r m
                          =>  [H.Var]
                          ->  m ()
    checkArgVarLinearity vs =
        let desc =  text "duplicate argument"
                    <+> text "variables"
                    <+> text "in definition of"
                    <+> dquotes (ppr v)
        in
          checkDuplicates desc vs
\end{code}

\subsubsection{Type Checking the Right-hand Sides of a Binding}

\begin{code}
checkRhs  ::  MonadTc r m
          =>  H.Rhs
          ->  Rho r
          ->  m ([m (Rec F.Binding)], [(Maybe (m F.Exp), m F.Exp)])
checkRhs (H.Rhs guards decls) rho =do
    (mgbindings, guards) <-  checkDecls decls $
                             mapM (\gd -> tcGuard gd rho) guards
    return (mgbindings, guards)
\end{code}

\subsubsection{Type Checking Guards}

To type check a guard we just make sure that the guard, if any, has type |Bool|,
and then check the type of the body of the guard.

\begin{code}
tcGuard  ::  MonadTc r m
         =>  (Maybe H.Exp, H.Exp)
         ->  Rho r
         ->  m (Maybe (m F.Exp), m F.Exp)
tcGuard (maybe_test, exp) rho = do
    maybe_gtest <- case maybe_test of
                     Nothing    -> return Nothing
                     Just test  -> do  (bool_rho, _, _) <- instantiate boolTy
                                       gtest <- checkExpRho test bool_rho
                                       return $ Just gtest
    gbody <- checkExpRho exp rho
    return (maybe_gtest, gbody)
\end{code}

\subsection{Type Checking Literals}

\begin{code}
tcLit  ::  MonadTc r m
       =>  H.Lit
       ->  Expected r (Rho r)
       ->  m F.Lit
tcLit (H.IntegerLit i) exp_ty =
    traceTcExp exp_ty (text "integer literal:" <+> ppr i) $ do
    instSigma integerTy exp_ty
    return $ F.IntegerLit i

tcLit (H.FloatLit f) exp_ty =
    traceTcExp exp_ty (text "float literal:" <+> text (show f)) $ do
    instSigma floatTy exp_ty
    return $ F.FloatLit f

tcLit (H.CharLit c) exp_ty =
    traceTcExp exp_ty (text "character literal:" <+> ppr c) $ do
    instSigma charTy exp_ty
    return $ F.CharLit c

tcLit (H.StringLit s) exp_ty =
    traceTcExp exp_ty (text "string literal:" <+> ppr s) $ do
    ty <- case exp_ty of
            Infer r -> readRef r
            Check ty -> return ty
    traceTc $ text "infer:" <+> ppr ty
    instSigma stringTy exp_ty
    return $ F.StringLit s

tcLit (H.AntiInt _)    _  = error "internal error: tcLit saw AntiInt"
tcLit (H.AntiFloat _)  _  = error "internal error: tcLit saw AntiFloat"
\end{code}

\subsection{Type Checking Expressions}

\begin{code}
tcExp  ::  MonadTc r m
       =>  H.Exp
       ->  Expected r (Rho r)
       ->  m (m F.Exp)

tcExp e@(H.LitExp lit sloc) exp_ty =
    traceTcExp exp_ty (text "literal:" <+> ppr e) $
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $ do
    lit' <- tcLit lit exp_ty
    return $ return $ F.LitExp lit' sloc

tcExp e@(H.VarExp v _) exp_ty =
    traceTcExp exp_ty (text "variable expression:" <+> ppr e) $
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $ do
    v_sigma           <- lookupVar v
    (predparams, co)  <- instSigma v_sigma exp_ty
    gv                <- coerceAst v
    return $ do
        e <- co (F.varE gv)
        return $ F.appsE e predparams

tcExp e@(H.ConExp con _) exp_ty =
    traceTcExp exp_ty (text "constructor expression:" <+> ppr e) $
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $ do
    con_sigma         <- lookupCon con
    (predparams, co)  <- instSigma con_sigma exp_ty
    gcon              <- coerceAst con
    return $ do
        e <- co (F.conE gcon)
        return $ F.appsE e predparams
\end{code}

We ``desugar'' record construction and update expressions here in the type
checker instead of earlier because we need access to information about the
datatype to which the constructor belongs.

\begin{code}
tcExp e@(H.RecConExp con bs _) exp_ty =
    traceTcExp exp_ty (text "expression:" <+> ppr e) $
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $ do
    checkDuplicates  (text "multiple uses of label")
                    (map fst bs)
    labels <- lookupConLabels con
    args <- mapM (\i -> pick con i bs H.undefinedE) [0..length labels - 1]
    tcExp (foldl' H.appE (H.ConExp con (fromLoc $ getLoc e)) args) exp_ty

tcExp e@(H.RecUpdateExp eu bs sloc) exp_ty =
    traceTcExp exp_ty (text "expression:" <+> ppr e) $
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $ do
    lbl_cons <- checkLabels (map fst bs)
    alts <- mapM (\con -> alt con bs) lbl_cons
    let default_alt = H.Alt (H.WildPat sloc) [(Nothing, H.errorE "Update error")] []
    tcExp (H.CaseExp eu (alts ++ [default_alt]) sloc) exp_ty
  where
    alt  ::  MonadTc r m
         =>  H.Con
         ->  [(H.Var, H.Exp)]
         ->  m H.Alt
    alt con bs = do
        arity    <- conArity con
        vs       <- replicateM arity (liftM H.Var $ uniqueName)
        let ves  = [H.VarExp v sloc | v <- vs]
        let vps  = [H.AsPat v (H.WildPat sloc) sloc | v <- vs]
        args <- mapM (\(ve, i) -> pick con i bs ve) (ves `zip` [0..])
        return $ H.Alt (H.ConPat con vps sloc)
                   [(Nothing, foldl' H.appE (H.ConExp con (fromLoc $ getLoc e)) args)] []
\end{code}

We always type check the body of a lambda in a new scope which removes any
non-top-level bindings from the environment.

\begin{code}
tcExp e@(H.LamExp p body _) exp_ty@(Infer ref) =
    traceTcExp exp_ty (text "expression:" <+> ppr e) $
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $ do
    (gpat, p_ty, binds)  <-  inferPat p
    (e_ty, gbody)        <-  withNewScope $
                             extendVars binds $
                             inferExpRho body
    writeRef ref (p_ty --> e_ty)
    trLam gpat p_ty gbody e_ty

tcExp e@(H.LamExp p body _) exp_ty@(Check ty) =
    traceTcExp exp_ty (text "expression:" <+> ppr e) $
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $ do
    (arg_ty, res_ty)  <-  unifyFun ty
    (gpat, binds)     <-  checkPat p arg_ty
    gbody             <-  withNewScope $
                          extendVars binds $
                          checkExpRho body res_ty
    trLam gpat arg_ty gbody res_ty
\end{code}

\begin{code}
tcExp e@(H.AppExp _ _ _) exp_ty =
    traceTcExp exp_ty (text "application expression:" <+> ppr e) $
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $
    tcApp (H.unfoldAppExp e) exp_ty

tcExp e@(H.OpAppExp _ _ _ _ _) exp_ty =
    traceTcExp exp_ty (text "operator expression:" <+> ppr e) $
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $
    tcApp (H.unfoldAppExp e) exp_ty

tcExp e@(H.NegAppExp _ _) exp_ty =
    traceTcExp exp_ty (text "negation expression:" <+> ppr e) $
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $
    tcApp (H.unfoldAppExp e) exp_ty
\end{code}

\begin{code}
tcExp e@(H.LetExp decls body _) exp_ty =
    traceTcExp exp_ty (text "expression:" <+> ppr e) $
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $ do
    (mgbindings, mgbody) <-  checkDecls decls $
                             tcExp body exp_ty
    return $ do
        gbindings <- sequence mgbindings
        gbody <- mgbody
        return $ foldr F.letE gbody gbindings

tcExp e@(H.IfExp test_e then_e else_e _) exp_ty =
    traceTcExp exp_ty (text "expression:" <+> ppr e) $
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $ do
    (bool_rho, _, _)  <- instantiate boolTy
    mgtest_e          <- checkExpRho test_e bool_rho
    rho               <- instType exp_ty
    mgthen_e          <- checkExpRho then_e rho
    mgelse_e          <- checkExpRho else_e rho
    _                 <- propagateRho rho exp_ty
    return $ do
        gtest_e <- mgtest_e
        gthen_e <- mgthen_e
        gelse_e <- mgelse_e
        trIf gtest_e gthen_e gelse_e
\end{code}

\begin{code}
tcExp e@(H.CaseExp scrut alts sloc) exp_ty =
    traceTcExp exp_ty (text "expression:" <+> ppr e) $
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $ do
        (scrut_ty, mgscrut) <- inferExpRho scrut
        rho <- instType exp_ty
        galts <- mapM (\alt -> tcAlt alt scrut_ty rho) alts
        [(scrut_sigma, co)] <- generalize [scrut_ty]
        let (quals, _, _) = destructForAll scrut_sigma
        scrut_sigma' <- compress scrut_sigma
        traceTc $ text "scrut_sigma:" <+> ppr scrut_sigma'
        return $ do
            gscrut <- mgscrut
            gcase <- trCase gscrut galts >>= co
            return $ F.tyappsE gcase (replicate (length quals) unitTy)
  where
    unitTy = F.TyConTy (F.TupleTyCon 0) sloc
\end{code}

\XXX{How should we handle predicate parameters here?}

\begin{code}
tcExp e@(H.SigExp sube ty _) exp_ty =
    traceTcExp exp_ty (text "expression:" <+> ppr e) $
    withLocContext (getLoc e) (text "expression:" <+> ppr e) $ do
        (sube_sigma_ty, mgsube)   <- inferSigma sube
        ann_sigma_ty              <- toSigma ty
        (_, co_ann)  <- subsCheck sube_sigma_ty ann_sigma_ty
        (_, co_exp)  <- instSigma ann_sigma_ty exp_ty
        return $ mgsube >>= co_ann >>= co_exp
\end{code}

\begin{code}
tcExp (H.ParExp e _) exp_ty = tcExp e exp_ty

tcExp (H.ListCompExp _ _ _) _    = panic $ text "no support for list comprehensions"
tcExp (H.ArithSeqExp _ _ _ _) _  = panic $ text "no support for arithmetic sequences"
tcExp (H.DoExp _ _) _            = panic $ text "no support for do expressions"

tcExp (H.WildPatExp _) _         = panic $ text "cannot type check WildPatExp"
tcExp (H.AsPatExp _ _ _) _       = panic $ text "cannot type check AsPatExp"
tcExp (H.IrrefutPatExp _ _) _    = panic $ text "cannot type check IrrefutPatExp"

tcExp (H.LSection _ _ _) _       = panic $ text "cannot type check LSection"
tcExp (H.RSection _ _ _) _       = panic $ text "cannot type check RSection"

tcExp (H.AntiExp _ _) _          = panic $ text "cannot type check AntiExp"
\end{code}

The |tcApp| function typechecks an unfolded application expression. We check
unfolded application expressions all-at-once so we can tell if a function is
fully applied. We use a helper function, |tcAppExp|, that takes the following
parameters:

\begin{itemize}
 \item A list of function arguments.

 \item The source location of the function being applied to the arguments.

 \item The type of the function being applied to the arguments.

 \item A monadic computation yielding an elaboration (in F) of the function
being applied to the arguments.

 \item The expected type of the value obtained when applying the function to the
arguments.
\end{itemize}

\noindent It then returns a monadic action computing the elaboration of the
function applied to its arguments.

\begin{code}
tcApp  ::  forall r m . MonadTc r m
       =>  [H.Exp]
       ->  Expected r (Rho r)
       ->  m (m F.Exp)
tcApp  []         _      =  panic $ text "called with no expressions"
tcApp  [_]        _      =  panic $ text "called with a single expression"
tcApp  (f : es)  exp_ty  =  do
    (f_ty, mgf) <- inferExpRho f
    mf <- tcAppExp es (getLoc f) f_ty mgf exp_ty
    closures  <- optVal (isFlagSet Opt_Closures)
    when (not closures) $ do
        ty <- readExpected exp_ty >>= compress
        when (isFunTy ty) $
            fail "Partial application not allowed with -XNoClosures"
    return mf
  where
    tcAppExp  ::  [H.Exp]
              ->  Loc
              ->  Rho r
              ->  m F.Exp
              ->  Expected r (Rho r)
              ->  m (m F.Exp)
    tcAppExp  []        _      _     _    _       =
        panic $ text "called with no expressions"

    tcAppExp  [e]       f_loc  f_ty  mgf  exp_ty  = do
        (e_ty, res_ty)    <- unifyFun f_ty
        mge               <- checkExpRho e e_ty
        (predparams, co)  <- propagateRho res_ty exp_ty
        return $ do
            gf  <- mgf
            ge  <- mge
            e   <- co (F.AppExp gf ge loc)
            return $ F.appsE e predparams
      where
        loc :: SrcLoc
        loc = fromLoc $ f_loc <--> getLoc e

    tcAppExp  (e : es)  f_loc  f_ty  mgf  exp_ty  = do
        (e_ty, res_ty)  <-  unifyFun f_ty
        mge             <-  checkExpRho e e_ty
        let mgfe        =   do  gf  <- mgf
                                ge  <- mge
                                return $ F.AppExp gf ge loc
        tcAppExp es (getLoc e) res_ty mgfe exp_ty
      where
        loc :: SrcLoc
        loc = fromLoc $ f_loc <--> getLoc e
\end{code}

\begin{code}
checkExpRho :: MonadTc r m => H.Exp -> Rho r -> m (m F.Exp)
checkExpRho exp ty = tcExp exp (Check ty)

inferExpRho :: MonadTc r m => H.Exp -> m (Rho r, m F.Exp)
inferExpRho exp = do
    ref <- newRef (internalErr $ text "inferExpRho: empty result")
    gexp <- tcExp exp (Infer ref)
    rho <- readRef ref
    return (rho, gexp)
\end{code}

\subsubsection{Type Checking a Case Alternative}

\begin{code}
tcAlt  ::  MonadTc r m
       =>  H.Alt
       ->  Rho r
       ->  Rho r
       ->  m (Equation r m)
tcAlt alt@(H.Alt pat guards decls) arg_ty rho =
    traceTcExp (Check rho) (text "tcAlt:" <+> ppr alt) $ do
    (gpat, binds) <- checkPat pat arg_ty
    checkDuplicates  (text "multiply defined variables")
                     (map fst binds)
    (mgbindings, gguards) <-  extendVars binds $
                              checkDecls decls $
                              mapM (\gd -> tcGuard gd rho) guards
    let ggdexp cont = do  gbindings <- sequence mgbindings
                          gbody <-  trGuards gguards cont
                          return $ foldr F.letE gbody gbindings
    return $ Equation [(gpat, arg_ty)] rho ggdexp
\end{code}

\begin{code}
instType  ::  MonadTc r m
          =>  Expected r (Type r)
          ->  m (Type r)
instType (Check exp_ty)  =  return exp_ty
instType (Infer r)       =  do  ty <- newMetaTv
                                writeRef r ty
                                return ty
\end{code}

\subsubsection{Instantiating Sigma Types}

\begin{code}
instSigma  ::  MonadTc r m
           =>  Sigma r
           ->  Expected r (Rho r)
           ->  m ([F.Exp], F.Exp -> m F.Exp)
instSigma sigma  (Infer r) = do
    (rho, predparams, co) <- instantiate sigma
    writeRef r rho
    return (predparams, co)

instSigma sigma  (Check exp_ty) = do
    traceTc $ text "subsCheck:" <+> ppr sigma <+> text "<=" <+> ppr exp_ty
    subsCheck sigma exp_ty
\end{code}

\begin{code}
inferSigma :: MonadTc r m => H.Exp -> m (Sigma r, m F.Exp)
inferSigma e = do
    (rho, gexp)  <- inferExpRho e
    [(rho', co)] <- generalize [rho]
    let gexp' = gexp >>= co
    return (rho', gexp')
\end{code}

\begin{code}
subsCheck  ::  MonadTc r m
           =>  Sigma r
           ->  Sigma r
           ->  m ([F.Exp], F.Exp -> m F.Exp)
\end{code}

Rule SKOL

\begin{code}
subsCheck sigma1 sigma2@(ForAll _ _ _) = do
    -- traceClass $ ppr sigma1 <+> text "<=" <+> ppr sigma2
    -- getContext >>= \ctx -> traceClass $ text "context:" <+> ppr ctx
    (skol_tvs, ctx2, rho2)  <-  skolemize sigma2
    opctx                   <-  getContext
    opctx2                  <-  extendContext ctx2 Nothing
    (predparams, co)        <-  subsCheck sigma1 rho2
    opctx'                  <-  compress opctx
    checkEntailment opctx2 opctx'
    esc_tvs                 <-  return (Set.toList . free) `ap` compress sigma1
    let bad_tvs             =   filter (`elem` esc_tvs) skol_tvs
    when (not (null bad_tvs)) $ do
        [psigma1, psigma2] <- pprTypes [sigma1, sigma2]
        throwException $ NotPolymorphicEnough psigma1 psigma2
    return (predparams, co)
\end{code}

Rule INST

We need to save and restore the context because otherwise instantiated
constraints end up left over in the context!

\XXX{How should we handle predicate parameters here?}

\begin{code}
subsCheck sigma1@(ForAll _ _ _) rho2 = do
    old_ctx <- getContext
    (rho1, predparams1, co_rho1)  <- instantiate sigma1
    (_, co_rho2)                  <- subsCheck rho1 rho2
    setContext old_ctx
    return $ (predparams1, \e -> co_rho1 e >>= co_rho2)
\end{code}

Rule MONO. We need to generate a coercion here too.

\begin{code}
subsCheck rho1 rho2 = do
    tau_ungen <- compress rho1
    unify rho1 rho2
    return ([], coerce tau_ungen rho1 rho2)
\end{code}

\subsection{Type Checking Patterns}

Bindings are always for sigma types (since they're instantiated). So, we have to
wrap pattern bindings in an outer |ForAll|.

\begin{code}
tcPat  ::  MonadTc r m
       =>  H.Pat
       ->  Expected r (Rho r)
       ->  m (NPat r, [(H.Var, Sigma r)])
tcPat p@(H.WildPat _) exp_ty@(Infer ref) =
    traceTcExp exp_ty (text "pattern:" <+> ppr p) $
    withLocContext (getLoc p) (text "pattern:" <+> ppr p) $ do
    ty <- newMetaTv
    writeRef ref ty
    return (WildNPat, [])

tcPat p@(H.WildPat _) exp_ty@(Check _) =
    traceTcExp exp_ty (text "pattern:" <+> ppr p) $
    withLocContext (getLoc p) (text "pattern:" <+> ppr p) $
    return (WildNPat, [])

tcPat p@(H.IrrefutPat pat _) exp_ty =
    traceTcExp exp_ty (text "pattern:" <+> ppr p) $
    withLocContext (getLoc p) (text "pattern:" <+> ppr p) $
    tcPat pat exp_ty

tcPat p@(H.LitPat lit _) exp_ty =
    traceTcExp exp_ty (text "pattern:" <+> ppr p) $
    withLocContext (getLoc p) (text "pattern:" <+> ppr p) $ do
    lit' <- tcLit lit exp_ty
    return (LitNPat lit', [])

tcPat p@(H.ConPat con pats _) exp_ty =
    traceTcExp exp_ty (text "pattern:" <+> ppr p) $
    withLocContext (getLoc p) (text "pattern:" <+> ppr p) $ do
    (arg_tys, res_ty) <- instPatCon con
    (gpats, binds) <- mapAndUnzipM (uncurry checkPat) (pats `zip` arg_tys)
    _ <- propagateRho res_ty exp_ty
    gcon <- coerceAst con
    let gconargs = gpats `zip` arg_tys
    return $ (ConNPat gcon gconargs, concat binds)

tcPat p@(H.AsPat v pat _) exp_ty@(Infer ref) =
    traceTcExp exp_ty (text "pattern:" <+> ppr p) $
    withLocContext (getLoc p) (text "pattern:" <+> ppr p) $ do
    ty <- newMetaTv
    writeRef ref ty
    (gpat, binds) <- checkPat pat ty
    gv <- coerceAst v
    return $ (AsNPat gv ty gpat, (v, ty) : binds)

tcPat p@(H.AsPat v pat _) exp_ty@(Check ty) =
    traceTcExp exp_ty (text "pattern:" <+> ppr p) $
    withLocContext (getLoc p) (text "pattern:" <+> ppr p) $ do
    (gpat, binds) <- checkPat pat ty
    gv <- coerceAst v
    return $ (AsNPat gv ty gpat, (v, ty) : binds)

tcPat p@(H.RecConPat con [] sloc) exp_ty =
    traceTcExp exp_ty (text "pattern:" <+> ppr p) $
    withLocContext (getLoc p) (text "pattern:" <+> ppr p) $ do
    arity <- conArity con
    tcPat (H.ConPat con (replicate arity (H.WildPat sloc)) sloc) exp_ty

tcPat p@(H.RecConPat con bs sloc) exp_ty =
    traceTcExp exp_ty (text "pattern:" <+> ppr p) $
    withLocContext (getLoc p) (text "pattern:" <+> ppr p) $ do
    checkLabels (map fst bs)
    labels <- lookupConLabels con
    let ps = map (\f -> fromMaybe (H.WildPat sloc) (lookup f bs)) labels
    tcPat (H.ConPat con ps sloc) exp_ty

tcPat p@(H.SigPat sub_p ty _) exp_ty =
    traceTcExp exp_ty (text "pattern:" <+> ppr p) $
    withLocContext (getLoc p) (text "pattern:" <+> ppr p) $ do
    ann_ty <- toSigma ty
    (gpat, binds) <- checkPat sub_p ann_ty
    _ <- propagateRho ann_ty exp_ty
    return (gpat, binds)

tcPat (H.NPlusKPat _ _ _) _  = panic $ text "no support for n+k patterns"

tcPat (H.OpConPat _ _ _ _ _) _ = panic $ text "cannot type check OpConPat"
\end{code}

\begin{code}
checkPat  ::  MonadTc r m
          =>  H.Pat
          ->  Rho r
          ->  m (NPat r, [(H.Var, Sigma r)])
checkPat pat ty = tcPat pat (Check ty)

inferPat  ::  MonadTc r m
          =>  H.Pat
          ->  m (NPat r, Rho r, [(H.Var, Sigma r)])
inferPat pat = do
    ref <- newRef (internalErr $ text "inferPat: empty result")
    (gpat, bindings) <- tcPat pat (Infer ref)
    rho <- readRef ref
    return (gpat, rho, bindings)
\end{code}

Check that the expected type |exp_ty| is more polymorphic than the pattern type
|pat_ty|.

\begin{code}
propagateRho  ::  MonadTc r m
              =>  Rho r
              ->  Expected r (Rho r)
              ->  m ([F.Exp], F.Exp -> m F.Exp)
propagateRho  ty (Infer r) = do
    ty' <- compress ty
    traceTc $ text "propagateRho inferred:" <+> ppr ty'
    writeRef r ty
    return ([], return)

propagateRho  ty (Check exp_ty) = do
    ty' <- compress ty
    exp_ty' <- compress exp_ty
    traceTc $  text "propagateRho subsCheck:"
               <+> ppr ty' <+> text "<=" <+> ppr exp_ty'
    subsCheck exp_ty ty
\end{code}

\subsubsection{Instantiating Constructors}

\begin{code}
instPatCon  ::  MonadTc r m
            =>  H.Con
            ->  m ([Tau r], Tau r)
instPatCon con = do
    sigma        <- lookupCon con
    (rho, _, _)  <- instantiate sigma
    return $ destructFunTy rho
\end{code}

\subsection{Type Checking Statements}

\begin{code}
tcStmt  ::  MonadTc r m
        =>  H.Stmt
        ->  Expected r (Rho r)
        ->  m (m F.Exp)

tcStmt stmt@(H.ExpStmt exp _) exp_ty =
    traceTcExp exp_ty (text "statement:" <+> ppr stmt) $
    withLocContext (getLoc stmt) (text "statement:" <+> ppr stmt) $
    tcExp exp exp_ty

tcStmt stmt@(H.PatStmt _ exp _) exp_ty =
    traceTcExp exp_ty (text "statement:" <+> ppr stmt) $
    withLocContext (getLoc stmt) (text "statement:" <+> ppr stmt) $
    tcExp exp exp_ty

tcStmt stmt@(H.LetStmt decls sloc) exp_ty =
    traceTcExp exp_ty (text "statement:" <+> ppr stmt) $
    withLocContext (getLoc stmt) (text "statement:" <+> ppr stmt) $ do
    (_, env) <- checkDecls decls $ getTcEnv
    putTcEnv env
    instSigma unitTy exp_ty
    return $ return $ F.ConExp (F.TupleCon 0) sloc
\end{code}

\begin{code}
checkStmtRho :: MonadTc r m => H.Stmt -> Rho r -> m (m F.Exp)
checkStmtRho exp ty = tcStmt exp (Check ty)

inferStmtRho :: MonadTc r m => H.Stmt -> m (Rho r, m F.Exp)
inferStmtRho exp = do
    ref <- newRef (internalErr $ text "inferStmtRho: empty result")
    mgexp <- tcStmt exp (Infer ref)
    rho <- readRef ref
    return (rho, mgexp)
\end{code}

\subsection{Translation to F}

\subsubsection{Translating lambdas}

\begin{code}
trLam  ::  MonadTc r m
       =>  NPat r
       ->  Type r
       ->  m F.Exp
       ->  Type r
       ->  m (m F.Exp)
trLam pat pat_ty body body_ty = do
    let q = Equation [(pat, pat_ty)] body_ty (\_ -> body)
    gv <- liftM F.Var $ uniqueName
    ([gv'], mglambody) <- trMatch [gv] [q]
    return $ do
        gpat_ty   <- compress pat_ty >>= coerceAst
        glambody  <- mglambody
        return $ F.LamExp gv' gpat_ty glambody (fromLoc $ getLoc glambody)
\end{code}

\subsubsection{Translating |if| statements}

\begin{code}
trIf  ::  MonadTc r m
      =>  F.Exp
      ->  F.Exp
      ->  F.Exp
      ->  m F.Exp
trIf gtest gthen gelse = do
    gv <- liftM F.Var $ uniqueName
    return $ F.CaseExp gtest gv
                 [F.Alt (F.ConPat trueCon   [] [] (fromLoc $ getLoc gthen))  gthen,
                  F.Alt (F.ConPat falseCon  [] [] (fromLoc $ getLoc gelse))  gelse]
                  (fromLoc $ getLoc gtest <--> getLoc gthen <--> getLoc gelse)
  where
    trueCon :: F.Con
    trueCon = F.Con prelTrue

    falseCon :: F.Con
    falseCon = F.Con prelFalse
\end{code}

\subsubsection{Translating |case| statements}

\begin{code}
trCase  ::  MonadTc r m
        =>  F.Exp
        ->  [Equation r m]
        ->  m F.Exp
trCase test qs = do
    gv <- case getVar test of
            Just gv  -> return gv
            Nothing  -> liftM F.Var $ uniqueName
    (_, mgcasebody) <- trMatch [gv] qs
    gcasebody <- mgcasebody
    case gcasebody of
      F.CaseExp _ v alts sloc  -> return $ F.CaseExp test v alts sloc
      _                        -> return gcasebody
  where
    getVar :: F.Exp -> Maybe F.Var
    getVar (F.VarExp (F.Var v) _)  = Just $ F.Var v
    getVar _                       = Nothing
\end{code}

\subsubsection{Translating matches}

\begin{code}
trMatch  ::  MonadTc r m
         =>  [F.Var]
         ->  [Equation r m]
         ->  m ([F.Var], m F.Exp)
trMatch us qs = do
    let (ptys, ty)  =  (\(Equation ps ty _) -> (map snd ps, ty)) (head qs)
    let defq        =  Equation (repeat WildNPat `zip` ptys) ty return
    let (us', qs')  =  renameUs us (qs ++ [defq])
    let mgexp       =  do  gty <- compress ty >>= coerceAst
                           compileMatch us' qs' (F.Builtin.mkError gty "No match")
    return (us', mgexp)
\end{code}

\subsubsection{Translating guards}

\begin{code}
trGuards  ::  MonadTc r m
          =>  [(Maybe (m F.Exp), m F.Exp)]
          ->  (F.Exp -> m F.Exp)
trGuards  []                   = return
trGuards  ((Nothing, me) : _)  = const me

trGuards  ((Just mgd, mgthen) : gds) =
    \cont -> do  gd <- mgd
                 gthen <- mgthen
                 gelse <- trGuards gds cont
                 trIf gd gthen gelse
\end{code}

\subsubsection{Translating field labels}

\begin{code}
trSelector  ::  MonadTc r m
            =>  H.Var
            ->  Type r
            ->  [(H.Con, Sigma r)]
            ->  Map.Map H.Con [H.Var]
            ->  Map.Map H.Var [H.Con]
            ->  m (m F.Decl)
trSelector lbl sel_ty cons conLabels labelCons = do
    let (quals, _, _, ret_ty) = destructForAllFunTy $ (snd . head) cons
    mgalts <- mapM  (\(con, con_ty) ->
                         trSelectorCase  lbl sel_ty con con_ty
                                         conLabels labelCons)
                    cons
    return $ do
        gv        <- liftM F.Var $ uniqueName
        galts     <- sequence mgalts
        gsel_ty   <- compress sel_ty >>= coerceAst
        glam_ty   <- compress ret_ty >>= coerceAst
        let glam  = F.LamExp  gv
                              glam_ty
                              (F.CaseExp (F.VarExp gv internalLoc) gv galts internalLoc)
                              internalLoc
        gsel      <- foldM  (\gexp (tv, k) ->  do  gtv <- coerceAst tv
                                                   gk  <- compress k >>= coerceAst
                                                   return $ F.TyLamExp gtv gk gexp internalLoc)
                            glam quals
        glbl <- coerceAst lbl
        return $  F.LetDecl (NonRec $
                             F.Binding glbl gsel_ty F.defaultBindInfo gsel internalLoc)
                  internalLoc
\end{code}

\begin{code}
trSelectorCase  ::  MonadTc r m
                =>  H.Var
                ->  Type r
                ->  H.Con
                ->  Sigma r
                ->  Map.Map H.Con [H.Var]
                ->  Map.Map H.Var [H.Con]
                ->  m (m F.Alt)
trSelectorCase lbl sel_ty con con_ty conLabels labelCons = do
    let (_, _, fld_tys, _)  = destructForAllFunTy con_ty
    let (_, _, _, lbl_ty)   = destructForAllFunTy sel_ty
    return $ do
        gcon  <- coerceAst con
        gtys  <- mapM (\ty -> compress ty >>= coerceAst) fld_tys
        if con `elem` (Map.findWithDefault [] lbl labelCons)
          then do  gv <- liftM F.Var $ uniqueName
                   let gpvs = map  (\lbl' ->  if lbl == lbl'
                                              then F.TameVar gv
                                              else F.WildVar)
                                   (Map.findWithDefault [] con conLabels)
                   return $ F.Alt  (F.ConPat gcon [] (gpvs `zip` gtys) internalLoc)
                                   (F.VarExp gv internalLoc)
          else do  glbl_ty <- compress lbl_ty >>= coerceAst
                   let gpvs = map (const F.WildVar) (Map.findWithDefault [] con conLabels)
                   return $ F.Alt  (F.ConPat gcon [] (gpvs `zip` gtys) internalLoc)
                                   (F.Builtin.mkUndefined glbl_ty)
\end{code}

This is just the |pick| function from the Haskell report.

\begin{code}
pick  ::  MonadTc r m
      =>  H.Con
      ->  Int
      ->  [(H.Var, H.Exp)]
      ->  H.Exp
      ->  m H.Exp
pick con i bs d = do
    labels <- lookupConLabels con
    when (length labels <= i) $
         panic $ text "bad label index"
    let label = labels !! i
    return $ foldl' (\d (f, v) -> if f == label then v else d) d bs
\end{code}

\subsection{Tracing the execution of the type checker}

\begin{code}
traceTcExp  ::  (Pretty ty, Compress r ty, MonadTc r m)
            =>  Expected r ty
            ->  Doc
            ->  m a
            ->  m a
traceTcExp tc_exp doc m = do
    traceTc $ text "checking" <+> doc
    d <- traceDepth
    setTraceDepth (d + 1)
    a <- m
    setTraceDepth d
    ty   <- readExpected tc_exp
    ty'  <- compress ty
    traceTc $ doc <+> text "::" <+> ppr ty'
    return a
\end{code}

\subsection{Instantiation and Generalization}

Instantiating a type returns the instantiated type and a coercion.

\begin{code}
instantiate  ::  forall r m . MonadTc r m
             =>  Sigma r
             ->  m (Rho r, [F.Exp], F.Exp -> m F.Exp)
instantiate ty = do
    ty'                   <-  compress ty
    let (tvks, ctx, qty)  =   destructForAll ty'
    let tvs               =   map fst tvks
    mtvs                  <-  mapM (const newMetaTv) tvs
    let theta             =   Map.fromList $ tvs `zip` mtvs
    let phi               =   free theta
    let ctx'              =   subst theta phi ctx
    let rho               =   subst theta phi qty
    oppreds               <-  extendContext ctx' Nothing
    let predparams        =   [v | OpPred _ v <- oppreds]
    traceTc $ nest 4 $
             text "instantiating"
        </>  text "context:" <+> ppr ctx'
        </>  text "   type:" <+> ppr ty'
        </>  text "    rho:" <+> ppr rho
    return (rho, [F.VarExp v internalLoc | v <- predparams], coerce ty' ty rho)
\end{code}

The |coerce| function generates a type coercion from |tau_gen|, the generalized
type of the expression we need to coerce, to |tau_use|, the type at which the
expression is used. The original, ungeneralized, type assigned to the expression
at type-checking time in |tau_ungen|, and is used only to generate trace
information.

\begin{code}
coerce  ::  forall r m . MonadTc r m
        =>  Type r
        ->  Type r
        ->  Type r
        ->  F.Exp
        ->  m F.Exp
coerce tau_ungen tau_gen tau_use e = do
    tau_ungen'  <- compress tau_ungen
    tau_gen'    <- compress tau_gen
    tau_use'    <- compress tau_use
    case tau_gen' of
      ForAll tvks _ tau -> do
          theta :: Map.Map TyVar (Type r)  <-  match Map.empty tau_use' tau
          let phi                          =   free theta
          let taus :: [Type r]             =   subst theta phi
                                               [TyVarTy tv | (tv, _) <- tvks]
          ftaus                            <-  coerceAst taus
          when False $ traceTc $ nest 4 $
                 text "instantiation coercion:"
            </>  text "ungeneralized type:" <+> ppr tau_ungen'
            </>  text "generalized type:" <+> ppr tau_gen'
            </>  text "use type:" <+> ppr tau_use'
            </>  text "theta:" <+> ppr (Map.toList theta)
          return $ F.tyappsE e ftaus
      _ -> return e
\end{code}

Generalization has a subtle issue when we're also elaborating: the problem with
elaboration is that we need to actually bind free metavariables with concrete
type variables rather than just substituting fresh type variables for
metavariables in the type we're generalizing because the metavariables also
exist in closures used by the delayed monadic actions that generate the
elaborated terms! This means that when we generalize a dependency group, we need
to be careful because the terms' types may share the same meta-variable. If we
blindly generalize each type in turn, a later type may already have some of its
meta-variables bound, so we can't properly quantify it by just looking for all
free meta-variables! Instead we first collect the free metavariables for each
type we're generalizing, and then for each type and its associated free
metavariables, we bind any metavariables that are \emph{not yet} bound to a
fresh type variable and then quantify the type over all (now bound)
metavariables.

\begin{code}
generalize  ::  forall r m . MonadTc r m
            =>  [Rho r]
            ->  m [(Sigma r, F.Exp -> m F.Exp)]
generalize rhos = do
    rhos'  <- compress rhos
    gamma  <- getContext >>= reduce
    traceTc $ nest 4 $ text "generalizing types:"  </>
        stack [ppr gamma <+> ppr rho | rho <- rhos']
    mtvs_env             <-  getVarTypes >>= metaTyVars
    mtvs_rho             <-  forM rhos' $ \rho -> do
                                 mtvs <- metaTyVars [rho]
                                 return $ mtvs \\ mtvs_env
    sigmas               <-  gen gamma allTyVars (mtvs_rho `zip` rhos')
    theta                <-  getPlaceholders
    let gamma_remaining  =   [opred |  opred@(OpPred pred _) <- gamma,
                                       not (relevant (concat mtvs_rho) pred)]
    setContext gamma_remaining
    case Map.toList theta of
      [] ->  return ()
      x ->   traceTc $ text "theta:" <+> ppr x
    when (length gamma_remaining /= 0) $ traceClass $
        text "remaining context:" <+> ppr gamma_remaining
    return [(sigma, f theta) | (sigma, f) <- sigmas]
  where
    relevant :: [MetaTv r] -> Pred r -> Bool
    relevant mtvs pred = any (`elem` mtvs) (Set.toList $ free pred)

    gen  ::  OpContext r
         ->  [TyVar]
         ->  [([MetaTv r], Rho r)]
         ->  m [(Sigma r, Map.Map F.Var F.Exp -> F.Exp -> m F.Exp)]
    gen  _      _             []                    = return []

    gen  gamma  alphas_avail  ((mtvs, rho) : rhos)  = do
        mtvs' <- filterM isUnbound mtvs
        forM (mtvs' `zip` alphas_avail) $ \(mtv, alpha) ->
            writeTv mtv (TyVarTy alpha)
        alphas              <-  mapM justTyVar mtvs
        rho'                <-  compress rho
        gamma_local'        <-  reduce gamma_local
        let local_preds     =   [pred | OpPred pred _ <- gamma_local']
        let dict_params     =   [v | OpPred _ v <- gamma_local']
        dict_param_tys      <-  coerceAst local_preds
        let forall_binders  =   alphas `zip` repeat (:*)
        let sigma = forallT (alphas `zip` repeat (:*)) local_preds rho'
        traceTc $ nest 4 $ text "generalizing" <+> ppr rho </>
            text "generalized type:" <+> ppr sigma </>
            case gamma_local' of
              [] ->  empty
              _ ->   text "    gamma_local':" <+> ppr gamma_local' </>
                     text " dict_params_tys:" <+> ppr dict_param_tys </>
                     text "     dict_params:" <+> ppr dict_params
        let co theta e = do
            gforall_binders <- mapM coerceAst forall_binders
            return $
                F.tylamsE gforall_binders $
                 F.lamsE  (dict_params `zip` dict_param_tys)
                          (subst theta Set.empty e)
        rhos' <- gen gamma (drop (length mtvs) alphas_avail) rhos
        return $ (sigma, co) : rhos'
      where
        gamma_local :: OpContext r
        gamma_local = [opred |  opred@(OpPred pred _) <- gamma,
                                relevant mtvs pred]

        isUnbound  ::  MetaTv r
                   ->  m Bool
        isUnbound (Meta _ ref) = do
            maybe_tau <- readRef ref
            case maybe_tau of
              Nothing ->  return True
              Just _ ->   return False

        justTyVar  ::  MetaTv r
                   ->  m TyVar
        justTyVar (Meta _ ref) = do
            maybe_tau <- readRef ref
            case maybe_tau of
              Just (TyVarTy tv) ->  return tv
              _ ->                  panic $
                                    text  "unbound metavariable \
                                          \when generalizing"
\end{code}
