\section{Type Checking Monad}

%if False
\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
-- Module      :  Check.Hs.Monad
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Check.Hs.Monad where

import Control.Monad (forM,
                      forM_,
                      liftM,
                      mapAndUnzipM,
                      when)
import Control.Monad.Unique
import Data.Char (toLower)
import Data.Ord (comparing)
import Data.List ((\\),
                  foldl',
                  foldl1',
                  intersect,
                  nub,
                  sort)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Compiler.Opt
import Check.Hs.Builtin
import Check.Hs.Exceptions
import Check.Hs.Types
import Control.Monad.ContextException
import Control.Monad.Exception
import Control.Monad.Ref
import Control.Monad.Trace
import Data.Loc
import Data.Name
import qualified Language.F as F
import Language.F.Pretty ()
import qualified Language.Hs as H
import Text.PrettyPrint.Mainland

import Fladuino.Util
\end{code}
%endif

\subsection{The Type Checking Monad Type}

\begin{code}
data Class r  =  Class  {  cls_ctx            :: Context r
                        ,  cls_tycon          :: TyCon
                        ,  cls_hstvs          :: [H.TyVar]
                        ,  cls_tvs            :: [TyVar]
                        ,  cls_ks             :: [Kind r]
                        ,  cls_pred           :: Pred r
                        ,  cls_binds          :: [H.Binding]
                        ,  cls_dict_tycon     :: F.TyCon
                        ,  cls_dict_con       :: F.Con
                        ,  cls_dict_ty        :: F.Type
                        ,  cls_dictselectors  :: [F.Var]
                        }
    deriving (Eq, Ord)

data Instance r  =  Instance  {  inst_hsctx       :: H.Context
                              ,  inst_ctx         :: Context r
                              ,  inst_tycon       :: TyCon
                              ,  inst_hstys       :: [H.Type]
                              ,  inst_tys         :: [Type r]
                              ,  inst_hstvs       :: [H.TyVar]
                              ,  inst_tvs         :: [TyVar]
                              ,  inst_ks          :: [Kind r]
                              ,  inst_pred        :: Pred r
                              ,  inst_decls       :: [H.Decl]
                              ,  inst_dictf       :: F.Var
                              }
    deriving (Eq, Ord)
\end{code}

\begin{code}
data OpPred r = OpPred (Pred r) F.Var

type OpContext r = [OpPred r]

instance Eq (OpPred r) where
    (OpPred p1 _) == (OpPred p2 _) = p1 == p2

instance Ord (OpPred r) where
    compare (OpPred p1 _) (OpPred p2 _) = compare p1 p2

instance Compress r (OpPred r) where
    compress (OpPred pred v) = do
      pred' <- compress pred
      return $ OpPred pred' v

instance Pretty (OpPred r) where
    ppr (OpPred pred v) = text "<" <> ppr pred <> text "," <> ppr v <> text ">"

    pprList  []   = empty
    pprList  [p]  = ppr p <+> text "=>"
    pprList  ps   = commasep (map ppr ps) <+> text "=>"
\end{code}

\begin{code}
data TcEnv r m = TcEnv
    {  synonyms       :: Map.Map H.TyCon  ([H.TyVar], H.Type)

    ,  classes        :: Map.Map TyCon (Class r)
    ,  classesOrder   :: [Class r]
    ,  instances      :: Map.Map TyCon [Instance r]
    ,  newInstances   :: Map.Map TyCon [Instance r]
    ,  classDefaults  :: [Type r]

    ,  tycons         :: Map.Map H.TyCon  (Kind r)
    ,  cons           :: Map.Map H.Con    (Sigma r)
    ,  tyconCons      :: Map.Map H.TyCon  [H.Con]
    ,  conTycons      :: Map.Map H.Con    H.TyCon
    ,  conLabels      :: Map.Map H.Con    [H.Var]
    ,  labelCons      :: Map.Map H.Var    [H.Con]

    ,  tyvars         :: Map.Map H.TyVar (Kind r)

    ,  context        :: OpContext r
    ,  placeholders   :: Map.Map F.Var (m F.Exp)

    ,  conFixities    :: Map.Map H.Con OpFixity
    ,  varFixities    :: Map.Map H.Var OpFixity

    ,  scopeLevel     :: Int
    ,  topVars        :: Map.Map H.Var (Sigma r)
    ,  vars           :: Map.Map H.Var (Sigma r)
    }

emptyTcEnv :: TcEnv r m
emptyTcEnv = TcEnv  {  synonyms  = Map.empty

                    ,  classes        = Map.empty
                    ,  instances      = Map.empty
                    ,  classesOrder   = []
                    ,  newInstances   = Map.empty
                    ,  classDefaults  = []

                    ,  tycons     = Map.fromList builtinTyCons
                    ,  cons       = Map.fromList builtinConstructors
                    ,  tyconCons  = Map.fromList builtinTyConCons
                    ,  conTycons  = Map.fromList builtinConTyCons
                    ,  conLabels  = Map.empty
                    ,  labelCons  = Map.empty

                    ,  tyvars     = Map.empty

                    ,  context       = []
                    ,  placeholders  = Map.empty

                    ,  conFixities     = Map.fromList H.builtinConFixities
                    ,  varFixities     = Map.fromList H.builtinVarFixities

                    ,  scopeLevel = 0
                    ,  topVars    = Map.fromList builtinVars
                    ,  vars       = Map.fromList builtinVars
                    }
\end{code}

\begin{code}
class (MonadContextException m,
       MonadOpts m,
       MonadRef r m,
       MonadTrace m,
       MonadUnique m)
    => MonadTc r m | m -> r where
    getTcEnv   :: m (TcEnv r m)
    putTcEnv   :: TcEnv r m -> m ()

    getsTcEnv :: (TcEnv r m -> a) -> m a
    getsTcEnv f = getTcEnv >>= \s -> return (f s)

    modifyTcEnv :: (TcEnv r m -> TcEnv r m) -> m ()
    modifyTcEnv f = getTcEnv >>= \s -> putTcEnv (f s)

    traceTc :: Doc -> m ()
    traceTc doc = do
        doTrace <- optVal (isFlagSet Opt_d_dump_tc_trace)
        when doTrace $ trace "traceTc:" doc

    traceMatch :: Doc -> m ()
    traceMatch doc = do
        doTrace <- optVal (isFlagSet Opt_d_dump_match_trace)
        when doTrace $
            trace "traceMatch:" doc

    traceClass :: Doc -> m ()
    traceClass doc = do
        doTrace <- optVal (isFlagSet Opt_d_dump_class_trace)
        when doTrace $
            trace "traceClass:" doc

    uniqueName  :: m Name
    uniqueName = do
        u <- newUnique
        return $ mkName $ "tc:" ++ show u

    isSynonym :: H.TyCon -> m Bool
    isSynonym tycon =
        getsTcEnv $ \s -> Map.member tycon (synonyms s)

    lookupSynonym  :: H.TyCon -> m ([H.TyVar], H.Type)
    lookupSynonym (H.TupleTyCon _) =
        fail "tuple type constructor cannot be a type alias"

    lookupSynonym tycon@(H.TyCon _) = do
        maybe_def <- getsTcEnv $ \s -> Map.lookup tycon (synonyms s)
        case maybe_def of
          Nothing   ->  throwException $ UnboundTypeConstructor tycon
          Just def  ->  return def

    insertSynonym :: H.TyCon -> ([H.TyVar], H.Type) -> m ()
    insertSynonym tycon def =
        modifyTcEnv $ \s ->
            s { synonyms = Map.insert tycon def (synonyms s) }

    insertClass :: Class r -> m ()
    insertClass cls =
      modifyTcEnv $ \s ->
          s  {  classes       = Map.insert  (cls_tycon cls) cls (classes s)
             ,  classesOrder  = cls : classesOrder s
             ,  instances     = Map.insert  (cls_tycon cls) [] (instances s)
             ,  newInstances  = Map.insert  (cls_tycon cls) [] (newInstances s)
             }

    classExists :: TyCon -> m Bool
    classExists tycon =
        getsTcEnv $ \s -> Map.member tycon (classes s)

    lookupClass :: TyCon -> m (Class r)
    lookupClass tycon = do
        maybe_cls <- getsTcEnv $ \s -> Map.lookup tycon (classes s)
        case maybe_cls of
          Nothing   ->  fail $ "Unknown class " ++ show tycon
          Just cls  ->  return cls

    insertInstance :: Instance r -> m ()
    insertInstance inst = do
        insts       <-  getsTcEnv instances
        let insts'  =   inst : Map.findWithDefault [] (inst_tycon inst) insts
        modifyTcEnv $ \s ->
            s { instances = Map.insert (inst_tycon inst) insts' (instances s) }

    insertNewInstance :: Instance r -> m ()
    insertNewInstance inst = do
        insts       <-  getsTcEnv newInstances
        let insts'  =   inst : Map.findWithDefault [] (inst_tycon inst) insts
        modifyTcEnv $ \s ->
            s { newInstances = Map.insert (inst_tycon inst) insts' (newInstances s) }

    processNewInstances :: (Class r -> Instance r -> m a) -> m [a]
    processNewInstances m = do
        classes  <- getsTcEnv classesOrder
        insts    <- getsTcEnv newInstances
        modifyTcEnv $ \s -> s { newInstances = Map.empty }
        liftM concat $ sequence $ (flip map) (reverse classes) $ \cls ->
            forM (insts Map.! cls_tycon cls) $ \inst ->
                m cls inst

    lookupSupers :: Pred r -> m [Pred r]
    lookupSupers (ClassPred tycon tys) = do
        cls        <-  lookupClass tycon
        let theta  =   Map.fromList (cls_tvs cls `zip` tys)
        let phi    =   free theta
        return $ map (subst theta phi) (cls_ctx cls)

    lookupInstances :: TyCon -> m [Instance r]
    lookupInstances tycon = do
        maybe_insts <- getsTcEnv $ \s -> Map.lookup tycon (instances s)
        case maybe_insts of
          Nothing     ->  fail $ "Unknown class " ++ show tycon
          Just insts  ->  return insts

    lookupTyCon :: H.TyCon -> m (Kind r)
    lookupTyCon (H.TupleTyCon n) =
        return $ foldr (:=>) (:*) (replicate n (:*))

    lookupTyCon tycon@(H.TyCon _) = do
        maybe_kind <- getsTcEnv $ \s -> Map.lookup tycon (tycons s)
        case maybe_kind of
          Nothing    ->  throwException $ UnboundTypeConstructor tycon
          Just kind  ->  return kind

    insertTyCon :: H.TyCon -> [H.Con] -> Kind r -> m ()
    insertTyCon tycon cons kind =
        modifyTcEnv $ \s ->
            s  {  tycons     = Map.insert tycon kind (tycons s)
               ,  tyconCons  = Map.insert  tycon cons (tyconCons s)
               ,  conTycons  = foldl'  (\m con -> Map.insert con tycon m)
                                       (conTycons s)
                                       cons
               }

    lookupTyVar :: H.TyVar -> m (Kind r)
    lookupTyVar tyvar = do
        maybe_kind <- getsTcEnv $ \s -> Map.lookup tyvar (tyvars s)
        case maybe_kind of
          Nothing    ->  throwException $ UnboundTypeVariable tyvar
          Just kind  ->  return kind

    insertTyVar :: H.TyVar -> Kind r -> m ()
    insertTyVar tyvar kind =
        modifyTcEnv $ \s -> s { tyvars = Map.insert tyvar kind (tyvars s) }

    extendTyVars :: [(H.TyVar, Kind r)] -> m a -> m a
    extendTyVars bindings m = do
        old_tyvars <- getsTcEnv tyvars
        forM_ bindings $ \(tyvar, kind) ->
            insertTyVar tyvar kind
        a <- m
        modifyTcEnv $ \s -> s { tyvars = old_tyvars }
        return a

    getContext :: m (OpContext r)
    getContext =  getsTcEnv context

    setContext :: (OpContext r) -> m ()
    setContext [] =
        modifyTcEnv $ \s ->
            s  {  context       = []
               ,  placeholders  = Map.empty
               }

    setContext ctx =
        modifyTcEnv $ \s -> s { context = ctx }

    extendContext :: Context r -> Maybe [m F.Exp] -> m [OpPred r]
    extendContext ctx maybe_mes = do
        opctx    <-  mapM oppred ctx
        let vs   =   [v | OpPred _ v <- opctx]
        let mes  =   case maybe_mes of
                       Nothing   -> [return (F.VarExp v sloc) | v <- vs]
                       Just mes  -> mes
        modifyTcEnv $ \s ->
            s  {  context       = opctx ++ context s
               ,  placeholders  = foldl'  (\m (v, me) -> Map.insert v me m)
                                          (placeholders s)
                                          (vs `zip` mes)
               }
        return opctx
      where
        sloc :: SrcLoc
        sloc = SrcLoc internalLoc

        oppred :: Pred r -> m (OpPred r)
        oppred pred = do
            v <- predParam pred
            return $ OpPred pred v

    lookupOpPred :: Pred r -> m (OpPred r)
    lookupOpPred pred = do
        ctx <- getContext
        case go pred ctx of
          Nothing  ->  fail $ "Cannot find predicate parameter for " ++ show pred
          Just v   ->  return v
      where
        go :: Pred r -> [OpPred r] -> Maybe (OpPred r)
        go  _    []                                    =  Nothing
        go  key  (op@(OpPred p _) : ops)  | key == p   =  Just op
                                          | otherwise  =  go key ops

    getPlaceholders :: m (Map.Map F.Var F.Exp)
    getPlaceholders = do
        theta   <- getsTcEnv placeholders
        theta'  <- sequence [me >>= \e -> return (v, e)
                                | (v, me) <- Map.toList theta]
        return $ Map.fromList theta'

    insertPlaceholder :: F.Var -> m F.Exp -> m ()
    insertPlaceholder v me =
        modifyTcEnv $ \s ->
            s { placeholders =  Map.insert v me (placeholders s) }

    replacePlaceholder :: F.Var -> m F.Exp -> m ()
    replacePlaceholder v me =
        modifyTcEnv $ \s ->
            s  {  placeholders =  Map.insert v me $
                                  Map.map sub (placeholders s)
               }
      where
        sub me0 = do
            e0  <- me0
            e   <- me
            return $ subst1 e v e0

    lookupCon :: H.Con -> m (Sigma r)
    lookupCon (H.TupleCon n) =
        return tuplecon_sigma
      where
        tvs             =  take n allTyVars
        tycon           =  TupleTyCon n
        tyvarTys        =  map TyVarTy tvs
        res_ty          =  foldAppTy (TyConTy tycon) tyvarTys
        tuplecon_sigma  =  ForAll (tvs `zip` repeat (:*)) [] $
                           foldr (-->) res_ty tyvarTys

    lookupCon con = do
        maybe_sigma <- getsTcEnv $ \s -> Map.lookup con (cons s)
        case maybe_sigma of
          Nothing     ->  throwException $ UnboundConstructor con
          Just sigma  ->  return sigma

    insertCon :: H.Con -> Sigma r -> m ()
    insertCon con sigma =
        modifyTcEnv $ \s ->
            s { cons = Map.insert con sigma (cons s) }

    insertConLabels :: [(H.Con, [H.Var])] -> m ()
    insertConLabels cons =
        modifyTcEnv $ \s ->
            s { conLabels = foldl' insert (conLabels s) cons }
      where
        insert m (con, lbls) = Map.insert con lbls m

    lookupConLabels :: H.Con -> m [H.Var]
    lookupConLabels con = do
        maybe_lbls <- getsTcEnv $ \s -> Map.lookup con (conLabels s)
        case maybe_lbls of
          Nothing    -> return []
          Just lbls  -> return lbls

    insertLabelCons :: [(H.Var, [H.Con])] -> m ()
    insertLabelCons lbls =
        modifyTcEnv $ \s ->
            s { labelCons = foldl' insert (labelCons s) lbls }
      where
        insert m (lbl, cons) = Map.insert lbl cons m

    lookupLabelCons :: H.Var -> m [H.Con]
    lookupLabelCons lbl = do
        maybe_cons <- getsTcEnv $ \s -> Map.lookup lbl (labelCons s)
        case maybe_cons of
          Nothing    -> return []
          Just cons  -> return cons

    lookupLabelTyCon :: H.Var -> m (H.TyCon)
    lookupLabelTyCon lbl = do
        cons              <- lookupLabelCons lbl
        (tycon : tycons)  <- mapM conTyCon cons
        when (not $ all (== tycon) tycons) $
            fail "label used in multiple type declarations"
        return tycon

    lookupVar :: H.Var -> m (Sigma r)
    lookupVar v@(H.Var _) = do
        maybe_sigma <- getsTcEnv $ \s -> Map.lookup v (vars s)
        case maybe_sigma of
          Nothing     ->  throwException $ UnboundVariable v
          Just sigma  ->  return sigma

    lookupVar (H.AntiVar _)    = fail "lookupVar: AntiVar"
    lookupVar (H.AntiVarId _)  = fail "lookupVar: AntiVarId"

    insertVar :: H.Var -> Sigma r -> m ()
    insertVar v sigma =
        modifyTcEnv $ \s -> s { vars =  Map.insert v sigma (vars s) }

    extendVars :: [(H.Var, Sigma r)] -> m a -> m a
    extendVars bindings m = do
        old_vars <- getsTcEnv vars
        forM bindings $ \(v, sigma) ->
            insertVar v sigma
        a <- m
        modifyTcEnv $ \s -> s { vars = old_vars }
        return a

    withNewScope :: m a -> m a
    withNewScope m = do
        closures  <- optVal (isFlagSet Opt_Closures)
        if closures then m else inNewScope m
      where
        inNewScope :: MonadTc r m => m a -> m a
        inNewScope m = do
            old_vars  <- getsTcEnv vars
            level     <- getsTcEnv scopeLevel
            when (level == 0) $
                modifyTcEnv $ \s ->
                    s  {  topVars = vars s }
            modifyTcEnv $ \s ->
                s  {  scopeLevel = level + 1
                   ,  vars = topVars s
                   }
            traceTc $ text "scope level:" <+> ppr (level + 1)
            a <- m
            traceTc $ text "scope level:" <+> ppr level
            modifyTcEnv $ \s ->
                s  {  scopeLevel = level
                   ,  vars = old_vars
                   }
            return a

    getVarTypes :: m [Sigma r]
    getVarTypes =
        getsTcEnv $ \s -> Map.elems (vars s)

    constructors :: H.TyCon -> m [H.Con]
    constructors (H.TupleTyCon n) =
        return [H.TupleCon n]

    constructors tycon = do
        maybe_cons <- getsTcEnv $ \s -> Map.lookup tycon (tyconCons s)
        case maybe_cons of
          Nothing    ->  throwException $ UnboundTypeConstructor tycon
          Just cons  ->  return cons

    conTyCon :: H.Con -> m (H.TyCon)
    conTyCon (H.TupleCon n) =
        return $ H.TupleTyCon n

    conTyCon con = do
        maybe_tycon <- getsTcEnv $ \s -> Map.lookup con (conTycons s)
        case maybe_tycon of
          Nothing     ->  throwException $ UnboundConstructor con
          Just tycon  ->  return tycon

    conArity :: H.Con -> m Int
    conArity con = do
        con_ty <- lookupCon con
        let (_, _, arg_tys, _) = destructForAllFunTy con_ty
        return $ length arg_tys

    lookupConFixity :: H.Con -> m OpFixity
    lookupConFixity con =
        getsTcEnv $ \s ->
            Map.findWithDefault H.defaultFixity con (conFixities s)

    lookupVarFixity :: H.Var -> m OpFixity
    lookupVarFixity v =
        getsTcEnv $ \s ->
            Map.findWithDefault H.defaultFixity v (varFixities s)

    extendConFixities :: [(H.Con, OpFixity)] -> m ()
    extendConFixities bindings =
        forM_ bindings $ \(con, fix) ->
            modifyTcEnv $ \s ->
                s { conFixities = Map.insert con fix (conFixities s) }

    clearVarFixities :: [H.Var] -> m ()
    clearVarFixities vs =
        modifyTcEnv $ \s ->
            s { varFixities = foldl' (flip Map.delete) (varFixities s) vs }

    extendVarFixities :: [(H.Var, OpFixity)] -> m ()
    extendVarFixities bindings =
        forM_ bindings $ \(v, fix) ->
            modifyTcEnv $ \s -> s { varFixities = Map.insert v fix (varFixities s) }
\end{code}

\begin{code}
pprKind  ::  (MonadTc r m)
         =>  Kind r
         ->  m Doc
pprKind k = do
    k' <- compress k
    return $ ppr k'
\end{code}

\begin{code}
pprTypes  ::  forall r m . (MonadTc r m)
          =>  [Type r]
          ->  m [Doc]
pprTypes tys = do
    tys' <- mapM compress tys
    let fvs      = Set.toList $ free tys'
    let mvs      = Set.toList $ occurs tys'
    let binders  =  (map TyVarTy $
                         take (length mvs) (allTyVars \\ fvs))
    let theta :: Map.Map (MetaTv r) (Type r)
        theta    =  Map.fromList $ mvs `zip` binders
    let phi      =  free theta
    let tys''    =  map (subst theta phi) tys'
    return $ map ppr tys''
\end{code}

\begin{code}
instance Pretty (Class r) where
    ppr cls =
        text "class"
        <+> case cls_ctx cls of
              []   -> empty
              ctx  -> ppr ctx <+> text "=>"
        <+> spread (ppr (cls_tycon cls) : map ppr (cls_tvs cls))
        <+> nest 4 (text "where {" </> semiseplines (map ppr (cls_binds cls)))
        </> text "}"

instance Pretty (Instance r) where
    ppr inst =
        text "instance"
        <+> case (inst_ctx inst) of
              []   -> empty
              ctx  -> ppr ctx <+> text "=>"
        <+> spread (ppr (inst_tycon inst) : map ppr (inst_tys inst))
        <+> nest 4 (text "where {" </> semiseplines (map ppr (inst_decls inst)))
        </> text "}"
\end{code}

\begin{code}
checkDuplicates  ::  (NameBinding b, Ord b, MonadTc r m)
                 =>  Doc
                 ->  [b]
                 ->  m ()
checkDuplicates desc bs =
    case filter  (\x -> length x /= 1)
                 (equivalence (comparing fst) binds) of
      []    ->  return ()
      dups  ->  throwException $ Duplicates desc dups
  where
    binds = map (\b -> (bindingName b, bindingLoc b)) bs
\end{code}

\begin{code}
checkIllegalRecursion  ::  (NameBinding b, Ord b, MonadTc r m)
                       =>  Doc
                       ->  [b]
                       ->  m ()
checkIllegalRecursion  _     []   =  return ()
checkIllegalRecursion  _     [_]  =  return ()
checkIllegalRecursion  desc  bs   =  throwException $ IllegalRecursion desc binds
  where
    binds = map (\b -> (bindingName b, bindingLoc b)) bs
\end{code}

\begin{code}
checkLabels  ::  MonadTc r m
             =>  [H.Var]
             ->  m [H.Con]
checkLabels labels = do
    when (length labels == 0) $
        panic $ text "no labels to check"
    checkDuplicates (text "multiple uses of label") labels
    (tycon : tycons) <- mapM lookupLabelTyCon labels
    when (not (all (== tycon) tycons)) $
        fail "labels belong to more than one data type"
    lbl_cons <- liftM (foldl1' intersect) $ mapM lookupLabelCons labels
    when (length lbl_cons == 0) $
        fail "no constructor has all specified labels"
    return lbl_cons
\end{code}

\subsection{Utilities}

\begin{code}
newMetaKv :: MonadTc r m => m (Kind r)
newMetaKv = do
    uniq <- newUnique
    kref <- newRef Nothing
    return $ MetaKv $ MetaK uniq kref

readKv :: MonadTc r m => MetaKv r -> m (Maybe (Kind r))
readKv (MetaK _ ref) = readRef ref

writeKv :: MonadTc r m => MetaKv r -> Kind r -> m ()
writeKv (MetaK _ ref) k = writeRef ref (Just k)
\end{code}

\begin{code}
newMetaTv :: MonadTc r m => m (Tau r)
newMetaTv = do
    uniq <- newUnique
    tref <- newRef Nothing
    return $ MetaTv $ Meta uniq tref

readTv :: MonadTc r m => MetaTv r -> m (Maybe (Tau r))
readTv (Meta _ ref) = readRef ref

writeTv :: MonadTc r m => MetaTv r -> Tau r -> m ()
writeTv (Meta _ ref) ty = writeRef ref (Just ty)
\end{code}

The |metaTyVars| function returns a list of all meta type variables in order of
occurrence. When generalizing, we use it instead of |free| for this reason. If
we used |free|, our generalized types would be equivalent up to alpha conversion
to what we generate now, but they would look odd, e.g., |foldr :: (b -> a -> a)
-> a -> [b] -> a|.

\begin{code}
metaTyVars  ::  forall r m . MonadTc r m
            =>  [Type r]
            ->  m [MetaTv r]
metaTyVars tys = do
    tys' <- mapM compress tys
    return $ nub $ concatMap go tys'
  where
    go :: Type r -> [MetaTv r]
    go (TyConTy _ )        = []
    go (TyVarTy _)         = []
    go (MetaTv mtv)        = [mtv]
    go (AppTy tau1 tau2)   = go tau1 ++ go tau2
    go (ForAll _ ctx tau)  = tvs_ctx ++ go tau
      where
        tvs_ctx = concat [go ty | ClassPred _ tys <- ctx, ty <- tys]
\end{code}

For reasons I don't completely understand, the type variable |r| has to be
lexically scoped here to get the type class method |subst| resolved.

\begin{code}
newSkolemTyVar :: MonadTc r m => TyVar -> m TyVar
newSkolemTyVar tyvar = do
    uniq <- newUnique
    return $ SkolemTyVar (tyVarName tyvar) uniq

skolemize  ::  forall r m . (CanSubst (Type r) TyVar (Type r), MonadTc r m)
           =>  Sigma r
           ->  m ([TyVar], Context r, Tau r)
skolemize ty@(ForAll binds ctx qty) = do
    traceTc $ text "skolemizing type:" <+> ppr ty
    skvs <- mapM newSkolemTyVar tvs
    let  theta :: Map.Map TyVar (Type r)
         theta  = Map.fromList $ tvs `zip` map TyVarTy skvs
    let phi    = free theta
    let ctx'   = subst theta phi ctx
    let qty'   = subst theta phi qty
    return (skvs, ctx', qty')
  where
    tvs :: [TyVar]
    tvs = map fst binds

skolemize _ =
    panic $ text "cannot skolemize non-sigma type"
\end{code}

\begin{code}
uniqueVar :: MonadTc r m => m F.Var
uniqueVar = do
    n <- uniqueName
    return $ F.Var n
\end{code}

\subsection{Path Compression}

The {\tt compress} function implements path compression, a.k.a. pruning. It
rewrites chains of references and replaces bound references with the type to
which they refer. The result of compressing a type is an equivalent type that
contains no {\it bound} references.

\begin{code}
class Compress r a where
    compress :: MonadTc r m => a -> m a

instance Compress r a => Compress r [a] where
    compress as = mapM compress as

instance (Compress r a, Compress r b) => Compress r (a, b) where
    compress (a, b) = do
        a' <- compress a
        b' <- compress b
        return (a', b')

instance Compress r a => Compress r (Maybe a) where
    compress Nothing   = return Nothing
    compress (Just a)  = do
        a' <- compress a
        return $ Just a'

instance Compress r a => Compress r (L a) where
    compress (L loc a) = do
        a' <- compress a
        return $ L loc a'
\end{code}

\begin{code}
instance Compress r TyVar where
    compress tv = return tv
\end{code}

\begin{code}
instance Compress r (Kind r) where
    compress k@(:*)             = return k
    compress k@(MetaKv kv)      = do  maybe_k <- readKv kv
                                      case maybe_k of
                                        Nothing  -> return k
                                        Just k   -> do  k' <- compress k
                                                        writeKv kv k'
                                                        return k'
    compress (k1 :=> k2)        = do  k1' <- compress k1
                                      k2' <- compress k2
                                      return $ k1' :=> k2'
\end{code}

\begin{code}
instance Compress r (Type r) where
    compress ty@(TyConTy _)         = return ty
    compress ty@(TyVarTy _)         = return ty
    compress ty@(MetaTv tv)         = do  maybeTy <- readTv tv
                                          case maybeTy of
                                            Nothing  -> return ty
                                            Just ty  -> do  ty' <- compress ty
                                                            writeTv tv ty'
                                                            return ty'
    compress (AppTy ty1 ty2)        = do  ty1' <- compress ty1
                                          ty2' <- compress ty2
                                          return $ AppTy ty1' ty2'
    compress (ForAll binds ctx ty)  = do  binds' <- compress binds
                                          ctx' <- compress ctx
                                          ty' <- compress ty
                                          return $ ForAll binds' ctx' ty'
\end{code}

\begin{code}
instance Compress r (Pred r) where
    compress (ClassPred tycon tys) = do  tys' <- compress tys
                                         return $ ClassPred tycon tys'
\end{code}

\subsection{Unification}

\begin{code}
class Unify r a where
    unify :: MonadTc r m => a -> a -> m ()

instance Unify r (Type r) where
    unify ty@(TyVarTy (TyVar _)) _ = do
        [pty] <- pprTypes [ty]
        panic $ text "unify: unexpected type:" <+> pty

    unify _ ty@(TyVarTy (TyVar _)) = do
        [pty] <- pprTypes [ty]
        panic $ text "unify: unexpected type:" <+> pty

    unify  (TyConTy tc1) (TyConTy tc2)  | tc1 == tc2 = return ()
    unify  (TyVarTy tv1) (TyVarTy tv2)  | tv1 == tv2 = return ()
    unify  (MetaTv tv1)  (MetaTv tv2)   | tv1 == tv2 = return ()

    unify  ty1@(MetaTv _)  ty2             = unifyVar False ty1 ty2
    unify  ty1             ty2@(MetaTv _)  = unifyVar True ty2 ty1

    unify  (AppTy ty1a ty2a) (AppTy ty1b ty2b) = do
        unify ty1a ty1b
        unify ty2a ty2b

    unify ty1 ty2 = do
        [pty1, pty2] <- pprTypes [ty1, ty2]
        throwException $ TypeUnificationError pty1 pty2

unifyVar :: MonadTc r m => Bool -> Tau r -> Tau r -> m ()
unifyVar flip ty1@(MetaTv tv1) ty2 = do
    maybe_ty1 <- readTv tv1
    case maybe_ty1 of
      Just ty1  -> if flip
                   then unify ty2 ty1
                   else unify ty1 ty2
      Nothing   -> unifyUnboundVar flip ty1 ty2
   where
     unifyUnboundVar :: MonadTc r m => Bool -> Tau r -> Tau r -> m ()
     unifyUnboundVar flip ty1@(MetaTv tv1) ty2@(MetaTv tv2) = do
         maybe_ty2 <- readTv tv2
         case maybe_ty2 of
           Just ty2'  -> if flip
                         then unify ty2' ty1
                         else unify ty1 ty2'
           Nothing    -> writeTv tv1 ty2

     unifyUnboundVar _ ty1@(MetaTv tv1) ty2 = do
         tyvars2 <- metaTyVars [ty2]
         when (tv1 `elem` tyvars2) $ do
             [pty1, pty2] <- pprTypes [ty1, ty2]
             throwException $ TypeOccursCheckError pty1 pty2
         writeTv tv1 ty2

     unifyUnboundVar _ _ _ =
         panic $ text "non meta-variable argument"

unifyVar _ _ _ =
         panic $ text "non meta-variable argument"

unifyFun :: MonadTc r m => Tau r -> m (Tau r, Tau r)
unifyFun (AppTy (AppTy (TyConTy (TyCon tycon)) arg) res)
    | tycon == builtinArrow = return (arg, res)
unifyFun tau = do
  arg_ty <- newMetaTv
  res_ty <- newMetaTv
  unify tau (arg_ty --> res_ty)
  return (arg_ty, res_ty)
\end{code}

\begin{code}
instance Unify r (Kind r) where
    unify  (:*)          (:*)                        = return ()
    unify  (MetaKv kv1)  (MetaKv kv2)  | kv1 == kv2  = return ()

    unify  k1@(MetaKv _)  k2             = unifyTyVar False k1 k2
    unify  k1             k2@(MetaKv _)  = unifyTyVar True k2 k1

    unify  (k1a :=> k2a)  (k1b :=> k2b)  = do  unify k1a k1b
                                               unify k2a k2b

    unify  k1 k2 = do
        [pk1, pk2] <- mapM pprKind [k1, k2]
        throwException $ KindUnificationError pk1 pk2

unifyTyVar :: MonadTc r m => Bool -> Kind r -> Kind r -> m ()
unifyTyVar flip k1@(MetaKv kv1) k2 = do
    maybe_k1 <- readKv kv1
    case maybe_k1 of
      Just k1   -> if flip
                   then unify k2 k1
                   else unify k1 k2
      Nothing   -> unifyUnboundTyVar flip k1 k2
   where
     unifyUnboundTyVar :: MonadTc r m => Bool -> Kind r -> Kind r -> m ()
     unifyUnboundTyVar flip k1@(MetaKv kv1) k2@(MetaKv kv2) = do
         maybe_k2 <- readKv kv2
         case maybe_k2 of
           Just k2'  -> if flip
                        then unify k1 k2'
                        else unify k2' k1
           Nothing   -> writeKv kv1 k2
     unifyUnboundTyVar _ (MetaKv kv1) k2 = writeKv kv1 k2
     unifyUnboundTyVar _ _ _ =
         panic $ text "non meta-variable argument"

unifyTyVar _ _ _ =
         panic $ text "non meta-variable argument"

unifyTyFun :: MonadTc r m => Kind r -> m (Kind r, Kind r)
unifyTyFun (arg :=> res)  =  return (arg, res)
unifyTyFun k              =  do  arg_k <- newMetaKv
                                 res_k <- newMetaKv
                                 unify k (arg_k :=> res_k)
                                 return (arg_k, res_k)
\end{code}

\subsection{Matching}

|match theta a b| returns a substitution $\theta'$ such that $a = \theta'(b)$.

\begin{code}
type Theta r = Map.Map TyVar (Type r)
\end{code}

\begin{code}
class Match r a where
    match :: Monad m => Map.Map TyVar (Type r) -> a -> a -> m (Map.Map TyVar (Type r))
\end{code}

\begin{code}
instance Match r a => Match r [a] where
    match  theta  []        []        =  return theta
    match  theta  (a : as)  (b : bs)  =  do  theta' <- match theta a b
                                             match theta' as bs
    match  _      _         _         =  fail "no match"
\end{code}

\begin{code}
instance Match r (Type r) where
    match theta t (TyVarTy tv@(TyVar _))
        | tv `Map.member` theta  =  if theta Map.! tv == t
                                      then return theta
                                      else fail "no match"
        | otherwise              =  return $ Map.insert tv t theta

    match theta (TyConTy tycon1) (TyConTy tycon2)
        | tycon1 == tycon2  = return theta
        | otherwise         = fail "no match"

    match theta (AppTy ty1a ty2a) (AppTy ty1b ty2b) =
        do  theta' <- match theta ty1a ty1b
            match theta' ty2a ty2b

    match _ _ _  = fail "no match"
\end{code}

\begin{code}
instance Match r (Pred r) where
    match theta (ClassPred tycon1 tys1) (ClassPred tycon2 tys2)
        | tycon1 == tycon2  = match theta tys1 tys2
        | otherwise         = fail "no match"
\end{code}

\subsection{Type synonym expansion}

\begin{code}
class ExpandSynonyms a where
    expandSynonyms :: MonadTc r m => a -> m a

instance ExpandSynonyms a => ExpandSynonyms [a] where
    expandSynonyms as = mapM expandSynonyms as

instance ExpandSynonyms H.Type where
    expandSynonyms ty@(H.TyConTy tycon) =
        do  isASynonym <- isSynonym tycon
            if isASynonym
              then expandSynonym [ty]
              else return ty

    expandSynonyms ty@(H.TyVarTy _) =
        return ty

    expandSynonyms ty@(H.AppTy ty1 ty2)  =
        case tys of
          H.TyConTy tycon : _ -> do
              isAlias <- isSynonym tycon
              if isAlias
                  then expandSynonym tys
                  else expandApp ty1 ty2
          _ -> expandApp ty1 ty2
      where
        tys :: [H.Type]
        tys = H.unfoldAppTy ty

        expandApp :: MonadTc r m => H.Type -> H.Type -> m H.Type
        expandApp ty1 ty2 = do
            ty1' <- expandSynonyms ty1
            ty2' <- expandSynonyms ty2
            return $ H.AppTy ty1' ty2'

    expandSynonyms (H.ForAll ex tvs ctx ty) = do
        ctx' <- expandSynonyms ctx
        ty' <- expandSynonyms ty
        return $ H.ForAll ex tvs ctx' ty'

    expandSynonyms (H.AntiType _) = error "expandSynonyms: AntiType"

instance ExpandSynonyms H.Pred where
    expandSynonyms (H.ClassPred tycon tys) = do
        tys' <- expandSynonyms tys
        return $ H.ClassPred tycon tys'
\end{code}

\begin{code}
expandSynonym :: MonadTc r m => [H.Type] -> m H.Type
expandSynonym (H.TyConTy tycon : tyargs) = do
    (tyvars, aliasedTy) <- lookupSynonym tycon
    let nargs = length tyvars
    when (length tyargs < nargs) $
        throwException $ PartiallyAppliedSynonym tycon nargs (length tyargs)
    let theta  = Map.fromList $ tyvars `zip` take nargs tyargs
    let phi    = free theta
    let expandedTy = foldl'  H.AppTy
                             (subst theta phi aliasedTy)
                             (drop nargs tyargs)
    expandSynonyms expandedTy

expandSynonym _ = panic $ text "expandSynonym called on non-alias type"
\end{code}

\subsection{Conversion from Massaman Abstract Syntax}

\begin{code}
class CoerceAst a b where
    coerceAst :: MonadTc e m => a -> m b

instance CoerceAst a b => CoerceAst [a] [b] where
    coerceAst as = mapM coerceAst as

instance (CoerceAst a a', CoerceAst b b') => CoerceAst (a, b) (a', b') where
    coerceAst (a, b) = do  a' <- coerceAst a
                           b' <- coerceAst b
                           return (a', b')

instance CoerceAst a b => CoerceAst (L a) (L b) where
    coerceAst (L loc a) = do  b <- coerceAst a
                              return $ L loc b
\end{code}

\begin{code}
instance CoerceAst H.TyVar TyVar where
    coerceAst (H.TyVar n) = return $ TyVar n

instance CoerceAst TyVar H.TyVar where
    coerceAst (TyVar n) = return $ H.TyVar n

    coerceAst (SkolemTyVar _ _) =
        panic $ text "encountered SkolemTyVar"

instance CoerceAst H.TyCon TyCon where
    coerceAst (H.TyCon n)       = return $ TyCon n
    coerceAst (H.TupleTyCon n)  = return $ TupleTyCon n

instance CoerceAst TyCon H.TyCon where
    coerceAst (TyCon n)       = return $ H.TyCon n
    coerceAst (TupleTyCon n)  = return $ H.TupleTyCon n
\end{code}

\begin{code}
instance CoerceAst H.Var F.Var where
    coerceAst (H.Var n)        = return $ F.Var n
    coerceAst (H.AntiVar _)    = error "coerceAst: AntiVar"
    coerceAst (H.AntiVarId _)  = error "coerceAst: AntiVarId"

instance CoerceAst H.TyVar F.TyVar where
    coerceAst (H.TyVar n) = return $ F.TyVar n

instance CoerceAst H.TyCon F.TyCon where
    coerceAst (H.TyCon n)       = return $ F.TyCon n
    coerceAst (H.TupleTyCon n)  = return $ F.TupleTyCon n

instance CoerceAst H.Con F.Con where
    coerceAst (H.Con n)       = return $ F.Con n
    coerceAst (H.TupleCon n)  = return $ F.TupleCon n

instance CoerceAst F.Con H.Con where
    coerceAst (F.Con n)       = return $ H.Con n
    coerceAst (F.TupleCon n)  = return $ H.TupleCon n
\end{code}

\begin{code}
instance CoerceAst H.Type (Type r) where
    coerceAst ty = do  expandedTy <- expandSynonyms ty
                       coerceAst' expandedTy
      where
        coerceAst' (H.TyConTy tycon) = do
            tycon' <- coerceAst tycon
            return $ TyConTy tycon'

        coerceAst' (H.TyVarTy tyvar) = do
            tyvar' <- coerceAst tyvar
            return $ TyVarTy tyvar'

        coerceAst' (H.AppTy ty1 ty2) = do
            ty1' <- coerceAst ty1
            ty2' <- coerceAst ty2
            return $ AppTy ty1' ty2'

        coerceAst' (H.ForAll _ _ ctx ty) = do
            ctx'     <-  coerceAst ctx
            ty'      <-  coerceAst ty
            let tvs  =   sort $ Set.toList $ free [ty']
            return $ ForAll (tvs `zip` repeat (:*)) ctx' ty'

        coerceAst' (H.AntiType _) =
            error "coerceAst': AntiType"
\end{code}

\begin{code}
instance CoerceAst H.Pred (Pred r) where
    coerceAst (H.ClassPred tycon tys) = do
        gtycon  <- coerceAst tycon
        gtys    <- coerceAst tys
        return $ ClassPred gtycon gtys
\end{code}

\begin{code}
instance CoerceAst TyVar F.TyVar where
    coerceAst (TyVar n) =
        return $ F.TyVar n

    coerceAst (SkolemTyVar _ _) =
        panic $ text "encountered SkolemTyVar"

instance CoerceAst TyCon F.TyCon where
    coerceAst (TyCon n)       = return $ F.TyCon n
    coerceAst (TupleTyCon n)  = return $ F.TupleTyCon n
\end{code}

\begin{code}
instance CoerceAst (Kind r) F.Kind where
    coerceAst (:*) = return (F.:*)

    coerceAst (MetaKv _) =
        panic $ text "encountered MetaKv"

    coerceAst (k1 :=> k2) = do
        k1' <- coerceAst k1
        k2' <- coerceAst k2
        return $ k1' F.:=> k2'
\end{code}

\begin{code}
instance CoerceAst (Type r) F.Type where
    coerceAst (TyConTy tycon) = do
        tycon' <- coerceAst tycon
        return $ F.TyConTy tycon' internalLoc

    coerceAst (TyVarTy tyvar) = do
        tyvar' <- coerceAst tyvar
        return $ F.TyVarTy tyvar' internalLoc

    coerceAst mtv@(MetaTv _) =
        panic $ text "encountered MetaTv:" <> ppr mtv

    coerceAst (AppTy ty1 ty2) = do
        ty1' <- coerceAst ty1
        ty2' <- coerceAst ty2
        return $ F.AppTy ty1' ty2' internalLoc

    coerceAst (ForAll binds ctx ty) = do
        (tvs, ks)  <-  mapAndUnzipM coerceAst binds
        ctx'       <-  coerceAst ctx
        ty'        <-  coerceAst ty
        let ty''   =   foldr1 (F.-->) (ctx' ++ [ty'])
        return $ F.forallsT (tvs `zip` ks) ty''
\end{code}

\begin{code}
instance CoerceAst (Pred r) F.Type where
    coerceAst (ClassPred (TyCon n) tys) = do
        let tycon' = TyCon (classDictName n)
        coerceAst $ foldl' AppTy (TyConTy tycon') tys

    coerceAst (ClassPred (TupleTyCon _) _) =
        panic $ text "tuple type constructor in class predicate"
\end{code}

\begin{code}
toSigma :: MonadTc r m => H.Type -> m (Sigma r)
toSigma  (H.ForAll _ _ ctx ty) = do
    ctx'     <-  coerceAst ctx
    ty'      <-  coerceAst ty
    let tvs  =   sort $ Set.toList $ free [ty']
    -- kvs      <-  replicateM (length tvs) newMetaKv
    let kvs  =  repeat (:*)
    return $ ForAll (tvs `zip` kvs) ctx' ty'

toSigma  ty = do
    ty'      <-  coerceAst ty
    let tvs  =   sort $ Set.toList $ free [ty']
    -- kvs      <-  replicateM (length tvs) newMetaKv
    let kvs  =  repeat (:*)
    return $ ForAll (tvs `zip` kvs) [] ty'
\end{code}

\begin{code}
classDictName :: Name -> Name
classDictName = nameToName $ \s ->
    s ++ "D"

dictSelectorName :: MonadUnique m => TyCon -> Pred r -> m Name
dictSelectorName (TyCon n) (ClassPred (TyCon predn) _) =
    return $ flip nameToName n $ \_ ->
        n' ++ predn'
  where
    n'      = classDict n
    predn'  = classDict predn

dictSelectorName (TupleTyCon _) _ =
    error "dictSelectorName: encountered tuple type class constructor"

dictSelectorName _ (ClassPred (TupleTyCon _) _) =
    error "dictSelectorName: encountered tuple type class constraint"

predParam :: MonadUnique m => Pred r -> m F.Var
predParam (ClassPred (TyCon n) tys) = do
    u        <-  newUnique
    return $ F.Var $ flip nameToName n $ \_ ->
        classDict n
        ++ classType (unfoldAppTy (head tys))
        ++ ":" ++ show u

predParam (ClassPred (TupleTyCon _) _) =
    error "predParam: encountered tuple class name"

instanceDictName :: MonadTc r m => TyCon -> [Type r] -> m Name
instanceDictName (TyCon n) [ty] =
    return $ nameToName (\_ -> n' ++ tyn) n
  where
    n', tyn :: String
    n' = classDict n
    tyn = classType $ unfoldAppTy ty

instanceDictName _ _ =
    panic $ text "can only handle a single type parameter"

instanceMemberName :: MonadTc r m => H.Var -> [Type r] -> m Name
instanceMemberName (H.Var n) [ty] = do
    -- u        <-  newUnique
    let mn   =   classMember (nameString n)
    let tyn  =   classType $ unfoldAppTy ty
    return $ nameToName (\_ -> mn ++ tyn) n

instanceMemberName _ _ =
    panic $ text "can only handle a single type parameter"

classMember :: String -> String
classMember  "=="  = "eq"
classMember  "/="  = "ne"
classMember  ">="  = "ge"
classMember  "<="  = "le"
classMember  ">"   = "gt"
classMember  "<"   = "lt"
classMember  "+"   = "add"
classMember  "-"   = "sub"
classMember  "*"   = "mul"
classMember  "/"   = "div"
classMember  s     = s

classType :: [Type r] -> String
classType  [TyVarTy (TyVar v)]                = nameString v
classType  (TyConTy (TyCon n)           : _)  = classTyCon (nameString n)
classType  (TyConTy (TupleTyCon 0)      : _)  = "Unit"
classType  (TyConTy (TupleTyCon 2)      : _)  = "Pair"
classType  (TyConTy (TupleTyCon arity)  : _)  = "T" ++ show arity
classType  _                                  = ""

classTyCon :: String -> String
classTyCon  "[]"  = "List"
classTyCon  "->"  = "Fun"
classTyCon  s     = s

classDict :: Name -> String
classDict n = (toLower c : cs) ++ "D"
  where
    ~(c:cs) = classTyCon $ nameString n
\end{code}
