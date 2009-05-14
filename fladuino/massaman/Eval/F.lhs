\section{The Untyped Evaluator}

%if False
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
-- Module      :  Eval.F
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Eval.F where

import Prelude hiding (exp)

import Control.Exception
import Control.Monad (forM_,
                      when)
import Data.Data
import qualified Data.Map as Map
import qualified List as List

import Compiler.Opt
import Control.Monad.ContextException
import Control.Monad.Exception
import Control.Monad.Ref
import Control.Monad.Trace
import Control.Monad.Unique
import Data.Name
import Language.F
import Text.PrettyPrint.Mainland
\end{code}
%endif

\subsection{The Value Type}

\begin{code}
data Val r m  =  IntegerVal Integer
              |  FloatVal Double
              |  CharVal Char
              |  ConVal Con Int [Val r m] -- Invariant: n + length vals == arity
              |  LambdaVal (EvalEnv r m) Var Exp
              |  PrimVal String (Val r m -> m (Val r m))

instance Eq (Val r m) where
    (IntegerVal n1)        == (IntegerVal n2)        = n1 == n2
    (FloatVal n1)          == (FloatVal n2)          = n1 == n2
    (CharVal c1)           == (CharVal c2)           = c1 == c2
    (ConVal con1 n1 vals1) == (ConVal con2 n2 vals2) = con1 == con2 && n1 == n2
                                                       && vals1 == vals2
    _                      == _                      = False

instance Pretty (Val r m) where
    pprPrec _ (IntegerVal n) = text $ show n
    pprPrec _ (FloatVal n)   = text $ show n
    pprPrec _ (CharVal c)    = text $ show c

    pprPrec _ (ConVal (TupleCon _) 0 vals) =
        parens $ commasep (map ppr vals)

    pprPrec p (ConVal con _ vals) =
        parensIf (length vals /= 0 && p >= 11) $
            ppr con <+> spread (map (pprPrec 11) vals)

    pprPrec _ (LambdaVal _ pat exp)
        = text "\\" <> ppr pat <+> text "->" <+> ppr exp

    pprPrec _ (PrimVal name _) = text name
\end{code}

\subsection{Lifting Haskell Values to the Value Type}

\begin{code}
class Liftable c where
    liftV  :: MonadEval r m => c -> Val r m

    lowerV :: MonadEval r m => Val r m -> m c
    lowerV x = fail $ "cannot lower " ++ show x

instance Liftable Integer where
    liftV = IntegerVal

    lowerV (IntegerVal n)  = return n
    lowerV x               = fail $ "cannot lower " ++ show x

instance Liftable Double where
    liftV = FloatVal

    lowerV (FloatVal n)  = return n
    lowerV x             = fail $ "cannot lower " ++ show x

instance Liftable Char where
    liftV = CharVal

    lowerV (CharVal c)  = return c
    lowerV x            = fail $ "cannot lower " ++ show x

instance Liftable () where
    liftV () = ConVal (TupleCon 0) 0 []

    lowerV (ConVal (TupleCon 0) 0 [])  = return ()
    lowerV x                           = fail $ "cannot lower " ++ show x

instance Liftable Bool where
    liftV True   = ConVal (Con prelTrue) 0 []
    liftV False  = ConVal (Con prelFalse) 0 []

    lowerV (ConVal (Con con) 0 [])
        | con == prelTrue   = return True
        | con == prelFalse  = return True
    lowerV v                = fail $ "cannot lower " ++ show v

instance (Liftable a) => Liftable [a] where
    liftV xs = foldr (\hd tl -> ConVal (Con builtinCons) 0 [hd, tl])
               (ConVal (Con builtinNil) 0 [])
               (map liftV xs)

instance (Liftable a, Liftable b) => Liftable (a, b) where
    liftV (a, b) = ConVal (TupleCon 2) 0 [liftV a, liftV b]
\end{code}

\subsection{The Evaluation Monad}

\begin{code}
data EvalException  =  Undefined
                    |  UnboundCon Con
                    |  UnboundVar Var
                    |  DifferingArgCounts Var
                    |  TypeError String
                    |  FailedMatch
  deriving (Typeable)

instance Pretty EvalException where
    ppr Undefined               =  text "Undefined"
    ppr (UnboundCon c)          =  text "Unbound constructor" <+> ppr c
    ppr (UnboundVar v)          =  text "Unbound variable" <+> ppr v
    ppr (DifferingArgCounts v)  =  text "Not all equations have the same "
                                   <+> text "number of arguments in the"
                                   <+> text "definition of"
                                   <+> dquotes (ppr v)
    ppr (TypeError s)           =  text "Type error:" <+> text s
    ppr FailedMatch             =  text "Failed pattern match"

instance Show EvalException where
    show = pprint

instance Exception EvalException
\end{code}

\begin{code}
data EvalEnv r m = EvalEnv  {  cons  :: Map.Map Con Int,
                               vals  :: Map.Map Var (r (Val r m))
                            }

emptyEvalEnv :: forall r m1 m . (MonadRef r m1, MonadEval r m) => m1 (EvalEnv r m)
emptyEvalEnv =
    do  vals <- mapM refVal builtinVals
        return EvalEnv  {  cons  = Map.fromList builtinConstructors,
                           vals  = Map.fromList vals
                        }
  where
    refVal :: (Var, Val r m) -> m1 (Var, r (Val r m))
    refVal (v , val) =
        do  ref <- newRef val
            return (v, ref)

class (MonadContextException m,
       MonadOpts m,
       MonadRef r m,
       MonadTrace m,
       MonadUnique m)
    => MonadEval r m | m -> r where
    getEvalEnv   :: m (EvalEnv r m)
    putEvalEnv   :: EvalEnv r m -> m ()

    getsEvalEnv :: (EvalEnv r m -> a) -> m a
    getsEvalEnv f = getEvalEnv >>= \s -> return (f s)

    modifyEvalEnv :: (EvalEnv r m -> EvalEnv r m) -> m ()
    modifyEvalEnv f = getEvalEnv >>= \s -> putEvalEnv (f s)

    traceEval :: Doc -> m ()
    traceEval doc =
        do  doTrace <- optVal (isFlagSet Opt_d_dump_eval_trace)
            when doTrace $
                trace "traceEval:" doc

    uniqName :: m Name
    uniqName =
        do  uniq <- newUnique
            let n = "%v" ++ show uniq
            return $ mkName n

    lookupCon :: Con -> m Int
    lookupCon (TupleCon arity) =
        return arity

    lookupCon con =
        do  maybe_arity <- getsEvalEnv (\s -> Map.lookup con (cons s))
            case maybe_arity of
              Just arity  -> return arity
              Nothing     -> throwException $ UnboundCon con

    insertCon :: Con -> Int -> m ()
    insertCon con n =
        modifyEvalEnv $ \s -> s { cons = Map.insert con n (cons s) }

    lookupVar :: Var -> m (Val r m)
    lookupVar v =
        do  maybe_ref <- getsEvalEnv (\s -> Map.lookup v (vals s))
            case maybe_ref of
              Just ref  -> readRef ref
              Nothing   -> throwException $ UnboundVar v

    insertVar :: Var -> Val r m -> m ()
    insertVar v val =
        do  ref <- newRef val
            modifyEvalEnv $ \s -> s {vals = Map.insert v ref (vals s)}

    updateVar :: Var -> Val r m -> m ()
    updateVar v val =
        do  maybe_ref <- getsEvalEnv (\s -> Map.lookup v (vals s))
            case maybe_ref of
              Just ref  -> writeRef ref val
              Nothing   -> throwException $ UnboundVar v

    withEnv :: EvalEnv r m -> m a -> m a
    withEnv env act =
        do  oldEnv <- getEvalEnv
            putEvalEnv env
            v <- act
            putEvalEnv oldEnv
            return v

    pushEnv :: m a -> m a
    pushEnv act =
        do  oldEnv <- getEvalEnv
            v <- act
            putEvalEnv oldEnv
            return v
\end{code}

\subsection{Built-in Constructs}

\begin{code}
unop  ::  (MonadEval r m, Liftable a, Liftable b)
      =>  String -> (a -> b) -> Val r m
unop name f =
    PrimVal name
      (\v1 -> do  n1 <- lowerV v1
                  return $ liftV (f n1)
      )

binop  ::  (MonadEval r m, Liftable a, Liftable b, Liftable c)
       =>  String -> (a -> b -> c) -> Val r m
binop name f =
    PrimVal name
      (\v1 -> return $
              PrimVal (name ++ " " ++ show v1)
              (\v2 -> do  n1 <- lowerV v1
                          n2 <- lowerV v2
                          return $ liftV (f n1 n2)
              ))

builtinVals :: MonadEval r m => [(Var, Val r m)]
builtinVals =
    [(Var $ prelError, PrimVal "error" (\msg -> fail (show msg))),
     (Var $ mkName "negate#", unop "negate#" ((\x -> -x) :: Integer -> Integer)),
     (Var $ mkName "||#", binop "(||#)" (||)),
     (Var $ mkName "&&#", binop "(&&#)" (&&)),
     (Var $ mkName "==#", PrimVal "(==#)"
                   (\v1 -> return $ PrimVal ("(==#) " ++ show v1)
                   (\v2 -> return $ liftV $ v1 == v2))),
     (Var $ mkName "/=#", PrimVal "(/=#)"
                   (\v1 -> return $ PrimVal ("(/=#) " ++ show v1)
                   (\v2 -> return $ liftV $ not $ v1 == v2))),
     (Var $ mkName "<#",  binop "(<#)"  ((<)   :: Integer -> Integer -> Bool)),
     (Var $ mkName "<=#", binop "(<=#)" ((<=)  :: Integer -> Integer -> Bool)),
     (Var $ mkName ">=#", binop "(>=#)" ((>=)  :: Integer -> Integer -> Bool)),
     (Var $ mkName ">#",  binop "(>#)"  ((>)   :: Integer -> Integer -> Bool)),
     (Var $ mkName "+#",  binop "(+#)"  ((+)   :: Integer -> Integer -> Integer)),
     (Var $ mkName "-#",  binop "(-#)"  ((-)   :: Integer -> Integer -> Integer)),
     (Var $ mkName "*#",  binop "(*#)"  ((*)   :: Integer -> Integer -> Integer)),
     (Var $ mkName "/#",  binop "(/#)"  ((div) :: Integer -> Integer -> Integer))]

builtinConstructors :: [(Con, Int)]
builtinConstructors =
    [(Con prelTrue,     0),
     (Con prelFalse,    0),
     (Con builtinNil,   0),
     (Con builtinCons,  2)]
\end{code}

\subsection{Evaluating Declarations}

\begin{code}
evalDecls :: forall r m . MonadEval r m => [Decl] -> m ()
evalDecls decls = do
    mapM_ evalDataDecl decls
    mapM_ evalTypeDecl decls
    mapM_ evalAxiomDecl decls
    mapM_ evalSigDecl decls
    mapM_ evalLetDecl decls
  where
    evalDataDecl :: Decl -> m ()
    evalDataDecl  (DataDecl _ _ condecls _)  = mapM_ evalConDecl condecls
      where
        evalConDecl :: ConDecl -> m ()
        evalConDecl  (ConDecl con ty _ _)        = insertCon con arity
          where
            (_, qty) = unfoldForAll ty
            arity    = length (unfoldFunTy qty) - 1
    evalDataDecl  _                        = return ()

    evalTypeDecl :: Decl -> m ()
    evalTypeDecl _ = return ()

    evalAxiomDecl :: Decl -> m ()
    evalAxiomDecl _ = return ()

    evalSigDecl :: Decl -> m ()
    evalSigDecl _ = return ()

    evalLetDecl :: Decl -> m ()
    evalLetDecl  (LetDecl bg _)  = evalBindingGroup bg
    evalLetDecl  _               = return ()
\end{code}

\subsubsection{Evaluating Bindings}

\begin{code}
evalBindingGroup :: MonadEval r m => Rec Binding -> m ()
evalBindingGroup (NonRec b) = do
    insertVar v (error $ "unevaluated variable " ++ show v)
    evalBinding b
  where
    (v, _) = boundVar b

evalBindingGroup (Rec bs) = do
  forM_ (map boundVar bs) $ \(v, _) ->
      insertVar v (error $ "unevaluated variable " ++ show v)
  mapM_ evalBinding bs

evalBinding :: MonadEval r m => Binding -> m ()
evalBinding (Binding v _ _ e _) = do
    val <- evalExp e
    updateVar v val
\end{code}

\subsection{Evaluating Literals, Expressions and Statements}

\begin{code}
evalLit :: MonadEval r m => Lit -> m (Val r m)
evalLit (IntegerLit n)    = return $ liftV n
evalLit (FloatLit n)      = return $ liftV n
evalLit (CharLit c)       = return $ liftV c
evalLit (StringLit s)     = return $ liftV s
\end{code}

\begin{code}
evalExp :: forall r m . MonadEval r m => Exp -> m (Val r m)

evalExp (LitExp lit _) =
    evalLit lit

evalExp (VarExp v _) =
    lookupVar v

evalExp (ConExp (TupleCon n) _)
    | n == 0     = return $ liftV ()
    | otherwise  = return $ ConVal (TupleCon n) n []

evalExp (ConExp con _) = do
  arity <- lookupCon con
  return $ ConVal con arity []

evalExp (LamExp v _ e _) = do
  env <- getEvalEnv
  return $ LambdaVal env v e

evalExp (AppExp e1 e2 _) = do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case v1 of
      ConVal  _    0  _     ->  throwException $
                                TypeError "constructor already fully applied"
      ConVal  con  n  vals  ->  return $ ConVal con (n-1) (vals ++ [v2])
      LambdaVal env v e     ->  withEnv env $ do
                                    insertVar v v2
                                    evalExp e
      PrimVal _ f           ->  f v2
      _                     ->  throwException $
                                TypeError "cannot apply a non-function"

evalExp (TyLamExp _ _ e _) =
    evalExp e

evalExp (TyAppExp e _ _) =
    evalExp e

evalExp (LetExp bg e _) = pushEnv $ do
    evalBindingGroup bg
    evalExp e

evalExp (CaseExp e v alts _) =
    do  val <- evalExp e
        pushEnv $ do
            insertVar v val
            evalCase val alts
    where
      evalCase :: Val r m -> [Alt] -> m (Val r m)
      evalCase  val  (Alt p e : cases) =
          case patMatch p val of
            Nothing  ->  evalCase val cases
            Just bs  ->  pushEnv $ do
                             mapM_ patBind bs
                             evalExp e

      evalCase  _    [] = throwException FailedMatch

evalExp (CastExp e _ _) =
    evalExp e
\end{code}

\subsection{Pattern Matching}

\begin{code}
patMatch  ::  forall r m . MonadEval r m
          =>  Pat
          ->  Val r m
          ->  Maybe [(WildVar, Val r m)]

patMatch  (LitPat l _)                  val =
    litPatMatch l val
  where
    litPatMatch :: Lit -> Val r m -> Maybe [(WildVar, Val r m)]
    litPatMatch  (IntegerLit i1)  (IntegerVal i2)  | i1 == i2  =  Just []
    litPatMatch  (FloatLit f1)    (FloatVal f2)    | f1 == f2  =  Just []
    litPatMatch  (CharLit c1)     (CharVal c2)     | c1 == c2  =  Just []
    litPatMatch  (StringLit _)    _                            =  error "cannot match on string literal pattern"
    litPatMatch  _                _                            =  Nothing

patMatch  (VarPat (wv, _) _)              val =
    Just [(wv, val)]

patMatch  (ConPat (TupleCon _) _ bs _)    (ConVal (TupleCon _) 0 vals)
    | length bs == length vals  = Just $ vs `zip` vals
    | otherwise                 = Nothing
  where
    (vs, _) = unzip bs

patMatch  (ConPat (TupleCon _) _ _ _)     _ =
    Nothing

patMatch  (ConPat con _ bs _)             (ConVal con' n vals)
    | con == con' && n == 0 && length bs == length vals =
        Just $ vs `zip` vals
  where
    (vs, _) = unzip bs

patMatch  (ConPat _ _ _ _)                _ =
    Nothing
\end{code}

\begin{code}
patBind  ::  MonadEval r m
         =>  (WildVar, Val r m)
         ->  m ()
patBind  (WildVar, _)       = return ()
patBind  (TameVar v,  val)  = insertVar v val
\end{code}
