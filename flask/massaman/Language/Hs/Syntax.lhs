%if False
\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
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
-- Module      :  Language.Hs.Syntax
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.Hs.Syntax (
    TyVar(..),
    TyCon(..),
    Var(..),
    Con(..),
    ExplicitForAll(..),
    Type(..),
    Pred(..),
    Context,
    Module(..),
    Body(..),
    Export(..),
    ImportDecl(..),
    Import(..),
    CName(..),
    ConDecl(..),
    DataOrNewType(..),
    Fixity(..),
    Decl(..),
    Rhs(..),
    Binding(..),
    Lit(..),
    Exp(..),
    Qual(..),
    Alt(..),
    Pat(..),
    Stmt(..),
    fixityDeclarations,
    sigDeclarations,
    typeDeclarations,
    valueDeclarations,
    destructConExp,
    isTyVar,
    destructForAll,
    unfoldAppTy,
    builtinConFixities,
    builtinVarFixities,
    defaultFixity,
    consFixity,
    negateFixity,
    fixityToAssoc,
    unfoldAppExp,
    unfoldFunTy,
    destructFunTy,
  ) where

import Data.Generics (Data,
                      Typeable)
import Data.List (foldl',
                  nub)
import Data.Maybe (fromMaybe,
                   mapMaybe)
import Data.Set ((\\),
                 union)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Loc
import Data.Name
import Text.PrettyPrint.Mainland
\end{code}
%endif

\section{Abstract Syntax}

\subsection{Bindings}

\begin{code}
data TyVar = TyVar Name
  deriving (Eq, Ord, Data, Typeable)

data TyCon  =  TyCon Name
            |  TupleTyCon Int
  deriving (Eq, Ord, Data, Typeable)

data Var  =  Var Name
          |  AntiVar String
          |  AntiVarId String
  deriving (Eq, Ord, Data, Typeable)

data Con  =  Con Name
          |  TupleCon Int
  deriving (Eq, Ord, Data, Typeable)
\end{code}

\begin{code}
instance Located TyVar where
    getLoc (TyVar n) = getLoc n

instance Located TyCon where
    getLoc (TyCon n)       = getLoc n
    getLoc (TupleTyCon _)  = internalLoc

instance Located Var where
    getLoc (Var n) = getLoc n
    getLoc _       = internalLoc

instance Located Con where
    getLoc (Con n)       = getLoc n
    getLoc (TupleCon _)  = internalLoc
\end{code}

\begin{code}
instance NameBinding TyVar where
    bindingName  (TyVar n)  = n
    bindingLoc   (TyVar n)  = nameLoc n

instance NameBinding TyCon where
    bindingName  (TyCon n)       = n
    bindingName  (TupleTyCon _)  = error "bad binding"
    bindingLoc   (TyCon n)       = nameLoc n
    bindingLoc   (TupleTyCon _)  = error "bad binding location"

instance NameBinding Var where
    bindingName  (Var n)        = n
    bindingName  (AntiVar _)    = error "cannot find binding name for anti-quoted variable"
    bindingName  (AntiVarId _)  = error "cannot find binding name for anti-quoted variable"
    bindingLoc   (Var n)        = nameLoc n
    bindingLoc   (AntiVar _)    = error "cannot find binding location for anti-quoted variable"
    bindingLoc   (AntiVarId _)  = error "cannot find binding location for anti-quoted variable"

instance NameBinding Con where
    bindingName  (Con n)       = n
    bindingName  (TupleCon _)  = error "bad binding"
    bindingLoc   (Con n)       = nameLoc n
    bindingLoc   (TupleCon _)  = error "bad binding location"

\end{code}

\subsection{Types}

\begin{code}
data ExplicitForAll  =  ExplicitForAll
                     |  ImplicitForAll
  deriving (Eq, Ord, Data, Typeable)

data Type  =  TyConTy TyCon
           |  TyVarTy TyVar
           |  AppTy Type Type
           |  ForAll ExplicitForAll [TyVar] Context Type
           |  AntiType String
  deriving (Eq, Ord, Data, Typeable)

data Pred = ClassPred TyCon [Type]
  deriving (Eq, Ord, Data, Typeable)

type Context = [Pred]
\end{code}

\subsection{Modules}

\begin{code}
data Module = Module Name [Export] Body
  deriving (Eq, Ord, Data, Typeable)

data Body = Body [ImportDecl] [Decl]
  deriving (Eq, Ord, Data, Typeable)

data Export  =  VarExport Var
             |  TyConExport TyCon (Maybe [CName])
             |  ModuleExport Name
  deriving (Eq, Ord, Data, Typeable)

data ImportDecl  =  ImportDecl Bool Name (Maybe Name) Bool [Import] SrcLoc
  deriving (Eq, Ord, Data, Typeable)

data Import  =  VarImport Var SrcLoc
             |  TyConImport TyCon (Maybe [CName]) SrcLoc
  deriving (Eq, Ord, Data, Typeable)

data CName  =  VarCName Var
            |  ConCName Con
  deriving (Eq, Ord, Data, Typeable)
\end{code}

\subsection{Declarations}

\begin{code}
data ConDecl  =  ConDecl Con OpFixity [Type] SrcLoc
              |  OpConDecl Type Con OpFixity Type SrcLoc
              |  RecConDecl Con OpFixity [(Var, Type)] SrcLoc
  deriving (Eq, Ord, Data, Typeable)

data DataOrNewType  =  DataType
                    |  NewType
  deriving (Eq, Ord, Data, Typeable)

data Fixity = Infixl | Infixr | Infix
  deriving (Eq, Ord, Data, Typeable)

data Decl  =  TypeDecl TyCon [TyVar] Type SrcLoc
           |  DataDecl DataOrNewType Context TyCon [TyVar] [ConDecl] [TyCon] SrcLoc
           |  ClassDecl Context TyCon [TyVar] [Decl] SrcLoc
           |  InstDecl Context TyCon [Type] [Decl] SrcLoc
           |  DefaultDecl [Type] SrcLoc
           |  ExpBindDecl Exp Rhs SrcLoc
           |  VarBindDecl Var [Pat] Rhs SrcLoc
           |  PatBindDecl Pat Rhs SrcLoc
           |  SigDecl [Var] Type SrcLoc
           |  FixityDecl [Name] Fixity Int SrcLoc
  deriving (Eq, Ord, Data, Typeable)

data Rhs  =  Rhs [(Maybe Exp, Exp)] [Decl]
  deriving (Eq, Ord, Data, Typeable)
\end{code}

We de-sugar all bindings into the following form:

\begin{code}
data Binding  =  FixityBind Name OpFixity
              |  SigBind Var Type OpFixity
              |  VarBind Var (Maybe Type) OpFixity [([Pat], Rhs)]
              |  PatBind Var (Maybe Type) OpFixity Rhs
  deriving (Eq, Ord, Data, Typeable)
\end{code}

\begin{code}
instance NameBinding Binding where
    bindingName  (FixityBind n _)          = n
    bindingName  (SigBind v _ _)           = bindingName v
    bindingName  (VarBind v _ _ _)         = bindingName v
    bindingName  (PatBind v _ _ _)         = bindingName v

    bindingLoc  (FixityBind n _)          = nameLoc n
    bindingLoc  (SigBind v _ _)           = bindingLoc v
    bindingLoc  (VarBind v _ _ _)         = bindingLoc v
    bindingLoc  (PatBind v _ _ _)         = bindingLoc v
\end{code}

\subsection{Expressions}

\begin{code}
data Lit  =  IntegerLit Integer
          |  FloatLit Double
          |  CharLit Char
          |  StringLit String
          |  AntiInt String
          |  AntiFloat String
  deriving (Eq, Ord, Data, Typeable)

data Exp  =  WildPatExp SrcLoc
          |  AsPatExp Var Exp SrcLoc
          |  IrrefutPatExp Exp SrcLoc
          |  LitExp Lit SrcLoc
          |  VarExp Var SrcLoc
          |  ConExp Con SrcLoc
          |  RecConExp Con [(Var, Exp)] SrcLoc
          |  RecUpdateExp Exp [(Var, Exp)] SrcLoc
          |  ListCompExp Exp [Qual] SrcLoc
          |  ArithSeqExp Exp (Maybe Exp) (Maybe Exp) SrcLoc
          |  LamExp Pat Exp SrcLoc
          |  AppExp Exp Exp SrcLoc
          |  OpAppExp Exp Exp OpFixity Exp SrcLoc
          |  NegAppExp Exp SrcLoc
          |  LetExp [Decl] Exp SrcLoc
          |  IfExp Exp Exp Exp SrcLoc
          |  CaseExp Exp [Alt] SrcLoc
          |  DoExp [Stmt] SrcLoc
          |  LSection Exp Exp SrcLoc
          |  RSection Exp Exp SrcLoc
          |  SigExp Exp Type SrcLoc
          |  ParExp Exp SrcLoc
          |  AntiExp String SrcLoc
  deriving (Eq, Ord, Data, Typeable)

data Qual  =  GenQual Pat Exp SrcLoc
           |  LetQual [Decl] SrcLoc
           |  GuardQual Exp SrcLoc
  deriving (Eq, Ord, Data, Typeable)

data Alt  =  Alt Pat [(Maybe Exp, Exp)] [Decl]
  deriving (Eq, Ord, Data, Typeable)
\end{code}

\subsection{Patterns}

\begin{code}
data Pat  =  WildPat SrcLoc
          |  IrrefutPat Pat SrcLoc
          |  LitPat Lit SrcLoc
          |  NPlusKPat Var Integer SrcLoc
          |  ConPat Con [Pat] SrcLoc
          |  OpConPat Pat Con OpFixity Pat SrcLoc
          |  RecConPat Con [(Var, Pat)] SrcLoc
          |  AsPat Var Pat SrcLoc
          |  SigPat Pat Type SrcLoc
  deriving (Eq, Ord, Data, Typeable)
\end{code}

\subsection{Statements}

\begin{code}
data Stmt  =  ExpStmt Exp SrcLoc
           |  PatStmt Pat Exp SrcLoc
           |  LetStmt [Decl] SrcLoc
  deriving (Eq, Ord, Data, Typeable)
\end{code}

\begin{code}
instance Located ImportDecl where
    getLoc (ImportDecl _ _ _ _ _ (SrcLoc loc)) = loc

instance Located Import where
    getLoc (VarImport _ (SrcLoc loc)) = loc
    getLoc (TyConImport _ _ (SrcLoc loc)) = loc

instance Located ConDecl where
    getLoc (ConDecl _ _ _ (SrcLoc loc)) = loc
    getLoc (OpConDecl _ _ _ _ (SrcLoc loc)) = loc
    getLoc (RecConDecl _ _ _ (SrcLoc loc)) = loc

instance Located Decl where
    getLoc (TypeDecl _ _ _ (SrcLoc loc)) = loc
    getLoc (DataDecl _ _ _ _ _ _ (SrcLoc loc)) = loc
    getLoc (ClassDecl _ _ _ _ (SrcLoc loc)) = loc
    getLoc (InstDecl _ _ _ _ (SrcLoc loc)) = loc
    getLoc (DefaultDecl _ (SrcLoc loc)) = loc
    getLoc (ExpBindDecl _ _ (SrcLoc loc)) = loc
    getLoc (VarBindDecl _ _ _ (SrcLoc loc)) = loc
    getLoc (PatBindDecl _ _ (SrcLoc loc)) = loc
    getLoc (SigDecl _ _ (SrcLoc loc)) = loc
    getLoc (FixityDecl _ _ _ (SrcLoc loc)) = loc

instance Located Rhs where
    getLoc (Rhs es [])     = getLoc (map snd es)
    getLoc (Rhs es decls)  = getLoc (map snd es) <--> getLoc decls

instance Located Exp where
    getLoc  (WildPatExp loc)         = toLoc loc
    getLoc  (AsPatExp _ _ loc)       = toLoc loc
    getLoc  (IrrefutPatExp _ loc)    = toLoc loc
    getLoc  (LitExp _ loc)           = toLoc loc
    getLoc  (VarExp _ loc)           = toLoc loc
    getLoc  (ConExp _ loc)           = toLoc loc
    getLoc  (RecConExp _ _ loc)      = toLoc loc
    getLoc  (RecUpdateExp _ _ loc)   = toLoc loc
    getLoc  (ListCompExp _ _ loc)    = toLoc loc
    getLoc  (ArithSeqExp _ _ _ loc)  = toLoc loc
    getLoc  (LamExp _ _ loc)         = toLoc loc
    getLoc  (AppExp _ _ loc)         = toLoc loc
    getLoc  (OpAppExp _ _ _ _ loc)   = toLoc loc
    getLoc  (NegAppExp _ loc)        = toLoc loc
    getLoc  (LetExp _ _ loc)         = toLoc loc
    getLoc  (IfExp _ _ _ loc)        = toLoc loc
    getLoc  (CaseExp _ _ loc)        = toLoc loc
    getLoc  (DoExp _ loc)            = toLoc loc
    getLoc  (LSection _ _ loc)       = toLoc loc
    getLoc  (RSection _ _ loc)       = toLoc loc
    getLoc  (SigExp _ _ loc)         = toLoc loc
    getLoc  (ParExp _ loc)           = toLoc loc
    getLoc  (AntiExp _ loc)          = toLoc loc

instance Located Qual where
    getLoc  (GenQual _ _ loc)  = toLoc loc
    getLoc  (LetQual _ loc)    = toLoc loc
    getLoc  (GuardQual _ loc)  = toLoc loc

instance Located Pat where
    getLoc  (WildPat loc)           = toLoc loc
    getLoc  (IrrefutPat _ loc)      = toLoc loc
    getLoc  (LitPat _ loc)          = toLoc loc
    getLoc  (NPlusKPat _ _ loc)     = toLoc loc
    getLoc  (ConPat _ _ loc)        = toLoc loc
    getLoc  (OpConPat _ _ _ _ loc)  = toLoc loc
    getLoc  (RecConPat _ _ loc)     = toLoc loc
    getLoc  (AsPat _ _ loc)         = toLoc loc
    getLoc  (SigPat _ _ loc)        = toLoc loc

instance Located Stmt where
    getLoc  (ExpStmt _ loc)    = toLoc loc
    getLoc  (PatStmt _ _ loc)  = toLoc loc
    getLoc  (LetStmt _ loc)    = toLoc loc
\end{code}

\subsection{Calculating Free and Bound variables}

\begin{code}
instance HasVars Type TyCon where
    free (TyConTy tycon)      = Set.singleton tycon
    free (TyVarTy _)          = Set.empty
    free (AppTy ty1 ty2)      = free ty1 `union` free ty2
    free (ForAll _ _ ctx ty)  = free ctx `Set.union` free ty
    free (AntiType _)         = error "cannot find free variables of anti-quoted type"

instance HasVars Type TyVar where
    free (TyConTy _)          = Set.empty
    free (TyVarTy tv)         = Set.singleton tv
    free (AppTy ty1 ty2)      = free ty1 `union` free ty2
    free (ForAll _ _ ctx ty)  = free ctx `Set.union` free ty
    free (AntiType _)         = error "cannot find free variables of anti-quoted type"
\end{code}

\begin{code}
instance HasVars Pred TyVar where
    free (ClassPred _ tys) = free tys

instance HasVars Pred TyCon where
    free (ClassPred tycon _) = Set.singleton tycon
\end{code}

\begin{code}
instance HasVars ConDecl TyCon where
    free (ConDecl _ _ tys _)        = free tys
    free (OpConDecl ty1 _ _ ty2 _)  = free ty1 `union` free ty2
    free (RecConDecl _ _ tys _)     = free (map snd tys)

instance HasVars ConDecl TyVar where
    free (ConDecl _ _ tys _)        = free tys
    free (OpConDecl ty1 _ _ ty2 _)  = free ty1 `union` free ty2
    free (RecConDecl _ _ tys _)     = free (map snd tys)

instance HasVars ConDecl Con where
    binders (ConDecl con _ _ _)         = [con]
    binders (OpConDecl _ con _ _ _)     = [con]
    binders (RecConDecl con _ _ _)      = [con]

instance HasVars ConDecl Var where
    binders (ConDecl _ _ _ _)           = []
    binders (OpConDecl _ _ _ _ _)       = []
    binders (RecConDecl _ _ flds _)     = map fst flds
\end{code}

\begin{code}
instance HasVars Decl TyCon where
    free (TypeDecl _ _ ty _)                     =  free ty
    free (DataDecl _ ctx _ _ condecls derive _)  =  free ctx `Set.union`
                                                    free condecls `Set.union`
                                                    Set.fromList derive
    free (ClassDecl ctx _ _ decls _)             =  free ctx `Set.union`
                                                    free decls
    free (InstDecl _ _ _ _ _)                    =  Set.empty
    free (DefaultDecl _ _)                       =  Set.empty
    free (ExpBindDecl _ _ _)                     =  error "free: encountered ExpBindDecl"
    free (VarBindDecl _ _ _ _)                   =  Set.empty
    free (PatBindDecl _ _ _)                     =  Set.empty
    free (SigDecl _ ty _)                        =  free ty
    free (FixityDecl _ _ _ _)                    =  Set.empty

    binders (TypeDecl tycon _ _ _)        =  [tycon]
    binders (DataDecl _ _ tycon _ _ _ _)  =  [tycon]
    binders (ClassDecl _ tycls _ _ _)     =  [tycls]
    binders (InstDecl _ tycls _ _ _)      =  [tycls]
    binders (DefaultDecl _ _)             =  []
    binders (ExpBindDecl _ _ _)           =  error "binders: encountered ExpBindDecl"
    binders (VarBindDecl _ _ _ _)         =  []
    binders (PatBindDecl _ _ _)           =  []
    binders (SigDecl _ _ _)               =  []
    binders (FixityDecl _ _ _ _)          =  []

instance HasVars Decl TyVar where
    free (TypeDecl _ tvs ty _)              =  foldl'  (flip Set.delete)
                                                       (free ty) tvs
    free (DataDecl _ _ _ tvs condecls _ _)  =  foldl'  (flip Set.delete)
                                                       (free condecls) tvs
    free (ClassDecl _ _ tvs decls _)        =  foldl'  (flip Set.delete)
                                                       (free decls) tvs
    free (InstDecl _ _ _ _ _)               =  Set.empty
    free (DefaultDecl _ _)                  =  Set.empty
    free (ExpBindDecl _ _ _)                =  error "free: encountered ExpBindDecl"
    free (VarBindDecl _ _ _ _)              =  Set.empty
    free (PatBindDecl _ _ _)                =  Set.empty
    free (SigDecl _ _ _)                    =  Set.empty
    free (FixityDecl _ _ _ _)               =  Set.empty

instance HasVars Decl Con where
    binders (TypeDecl _ _ _ _)               =  []
    binders (DataDecl _ _ _ _ condecls _ _)  =  binders condecls
    binders (ClassDecl _ _ _ _ _)            =  []
    binders (InstDecl _ _ _ _ _)             =  []
    binders (DefaultDecl _ _)                =  []
    binders (ExpBindDecl _ _ _)              =  error "binders: encountered ExpBindDecl"
    binders (VarBindDecl _ _ _ _)            =  []
    binders (PatBindDecl _ _ _)              =  []
    binders (SigDecl _ _ _)                  =  []
    binders (FixityDecl _ _ _ _)             =  []
\end{code}

\begin{code}
instance HasVars Decl Var where
    free (TypeDecl _ _ _ _)          =  Set.empty
    free (DataDecl _ _ _ _ _ _ _)    =  Set.empty
    free (ClassDecl _ _ _ _ _)       =  Set.empty
    free (InstDecl _ _ _ _ _)        =  Set.empty
    free (DefaultDecl _ _)           =  Set.empty
    free (ExpBindDecl _ _ _)         =  error "free: encountered ExpBindDecl"
    free (VarBindDecl _ pats rhs _)  =  free rhs \\ Set.fromList (binders pats)
    free (PatBindDecl pat rhs _)     =  free rhs \\ Set.fromList (binders pat)
    free (SigDecl vs _ _)            =  Set.fromList vs
    free (FixityDecl _ _ _ _)        =  Set.empty

    binders (TypeDecl _ _ _ _)               =  []
    binders (DataDecl _ _ _ _ condecls _ _)  =  Set.toList $ Set.fromList $
                                                binders condecls
    binders (ClassDecl _ _ _ decls _)        =  binders decls
    binders (InstDecl _ _ _ decls _)         =  binders decls
    binders (DefaultDecl _ _)                =  []
    binders (ExpBindDecl _ _ _)              =  error "binders: encountered ExpBindDecl"
    binders (VarBindDecl v _ _ _)            =  [v]
    binders (PatBindDecl pat _ _)            =  binders pat
    binders (SigDecl _ _ _)                  =  []
    binders (FixityDecl _ _ _ _)             =  []
\end{code}

The definition of @HasVars@ for lists of @Decl@ is different because binders can
be bound in multiple locations (think funtion cases) and still be valid.

\begin{code}
instance HasVars [Decl] Var where
    free     decls  = foldl' Set.union Set.empty $ map free decls
    binders  decls  = nub $ concat $ map binders decls
\end{code}

\begin{code}
instance HasVars Rhs Var where
    free (Rhs alts decls)  = (free alts \\ Set.fromList (binders decls)) `union` free decls

instance HasVars Binding Var where
    free (SigBind _ _ _)           = Set.empty
    free (FixityBind _ _ )         = Set.empty
    free (VarBind _ _ _ alts)      = free alts
    free (PatBind _ _ _ rhs)       = free rhs

    binders (SigBind _ _ _)           = []
    binders (FixityBind _ _)          = []
    binders (VarBind v _ _ _)         = [v]
    binders (PatBind v _ _ _)         = [v]

instance HasVars ([Pat], Rhs) Var where
    free (pats, rhs) = free rhs \\ Set.fromList (binders pats)

instance HasVars Exp Var where
    free (VarExp v _)               = Set.singleton v
    free (ConExp _ _)               = Set.empty
    free (RecConExp _ flds _)       = free (map snd flds)
    free (RecUpdateExp _ flds _)    = free (map snd flds)
    free (ListCompExp e quals _)    = free e `union` free quals
    free (ArithSeqExp fr th to _)   = free fr `union` free th `union` free to
    free (LitExp _ _)               = Set.empty
    free (LamExp pat exp _)         = free exp \\ Set.fromList (binders pat)
    free (LetExp decls exp _)       = (free decls `union` free exp) \\ Set.fromList (binders decls)
    free (IfExp test th el _)       = free test `union` free th `union` free el
    free (CaseExp exp alts _)       = free exp `union` free alts
    free (OpAppExp e1 op _ e2 _)    = free e1 `union` free op `union` free e2
    free (NegAppExp e _)            = free e
    free (AppExp e1 e2 _)           = free e1 `union` free e2
    free (LSection e1 e2 _)         = free e1 `union` free e2
    free (RSection e1 e2 _)         = free e1 `union` free e2
    free (SigExp exp _ _)           = free exp
    free (ParExp exp _)             = free exp

    free (DoExp stmts _) =
        f Set.empty Set.empty stmts
      where
        f  ::  Set.Set Var
           ->  Set.Set Var
           ->  [Stmt]
           ->  Set.Set Var
        f  _    fvs  []              = fvs
        f  bvs  fvs  (stmt : stmts)  = f  (bvs `union` Set.fromList (binders stmt))
                                          (fvs `union` (free stmt \\ bvs)) stmts

    free (WildPatExp _) =
        error "internal error: cannot find free variables of WildPatExp"

    free (AsPatExp _ _ _) =
        error "internal error: cannot find free variables of AsPatExp"

    free (IrrefutPatExp _ _) =
        error "internal error: cannot find free variables of IrrefutPatExp"

    free (AntiExp _ _) =
        error "cannot find free variables of anti-quoted expression"

instance HasVars Qual Var where
    free (GenQual pat exp _)   = free exp \\ Set.fromList (binders pat)
    free (LetQual decls _)     = free decls \\ Set.fromList (binders decls)
    free (GuardQual exp _)     = free exp

instance HasVars Alt Var where
    free (Alt pat guards decls) =
        (free guards `union` free decls) \\ (Set.fromList (binders pat) `union` Set.fromList (binders decls))

instance HasVars Pat Var where
    free _ = Set.empty

    binders (WildPat _)               = []
    binders (IrrefutPat p _)        = binders p
    binders (LitPat _ _)            = []
    binders (NPlusKPat v _ _)       = [v]
    binders (ConPat _ pats _)       = binders pats
    binders (OpConPat p1 _ _ p2 _)  = binders p1 ++ binders p2
    binders (RecConPat _ flds _)    = binders $ map snd flds
    binders (AsPat v _ _)           = [v]
    binders (SigPat pat _ _)        = binders pat

instance HasVars Stmt Var where
    free (ExpStmt e _)         = free e
    free (PatStmt _ e _)       = free e
    free (LetStmt decls _)     = free decls \\ Set.fromList (binders decls)

    binders (ExpStmt _ _)      = []
    binders (PatStmt p _ _)    = binders p
    binders (LetStmt decls _)  = binders decls
\end{code}

\subsection{Performing Substitutions}

\begin{code}
instance CanSubst Type TyVar Type where
    subst  _      _    tau@(TyConTy _)         =  tau
    subst  theta  _    tau@(TyVarTy tv)        =  fromMaybe tau (Map.lookup tv theta)
    subst  theta  phi  (AppTy tau1 tau2)       =  AppTy  (subst theta phi tau1)
                                                         (subst theta phi tau2)
    subst  theta  phi  (ForAll ex tvs ps tau)  =  ForAll ex tvs ps (subst theta phi tau)
    subst  _      _    tau@(AntiType _)        =  tau
\end{code}

\subsection{Utilities}

\begin{code}
fixityDeclarations :: [Decl] -> [(Name, OpFixity)]
fixityDeclarations decls = concatMap go decls
  where
    go :: Decl -> [(Name, OpFixity)]
    go (FixityDecl ns fix prec _)  = [(n, (fixityToAssoc fix, prec)) | n <- ns]
    go _                           = []
\end{code}

\begin{code}
sigDeclarations :: [Decl] -> [(Var, Type)]
sigDeclarations decls = concatMap go decls
  where
    go :: Decl -> [(Var, Type)]
    go (SigDecl vs ty _)  = map (\v -> (v, ty)) vs
    go _                  = []
\end{code}

\begin{code}
typeDeclarations :: [Decl] -> [Decl]
typeDeclarations decls = mapMaybe go decls
  where
    go :: Decl -> Maybe Decl
    go decl@(TypeDecl _ _ _ _)        = Just decl
    go decl@(DataDecl _ _ _ _ _ _ _)  = Just decl
    go decl@(ClassDecl _ _ _ _ _)     = Just decl
    go _                              = Nothing
\end{code}

\begin{code}
valueDeclarations :: [Decl] -> [Decl]
valueDeclarations decls = mapMaybe go decls
  where
    go :: Decl -> Maybe Decl
    go decl@(VarBindDecl _ _ _ _)  = Just decl
    go decl@(PatBindDecl _ _ _)    = Just decl
    go decl@(SigDecl _ _ _)        = Just decl
    go _                           = Nothing
\end{code}

\begin{code}
destructConExp :: Exp -> Maybe Con
destructConExp  (ConExp con _)  =  Just con
destructConExp  _               =  Nothing
\end{code}

\begin{code}
isTyVar :: Type -> Bool
isTyVar  (TyVarTy _)  = True
isTyVar  _            = False
\end{code}

\begin{code}
destructForAll :: Type -> ([TyVar], Context, Type)
destructForAll  (ForAll _ tvs ctx ty)  = (tvs, ctx, ty)
destructForAll  ty                     = ([], [], ty)
\end{code}

\begin{code}
unfoldAppTy :: Type -> [Type]
unfoldAppTy  (AppTy ty1 ty2)  =  unfoldAppTy ty1 ++ [ty2]
unfoldAppTy  ty               =  [ty]
\end{code}

\begin{code}
builtinConFixities :: [(Con, OpFixity)]
builtinConFixities = [(Con builtinCons, consFixity)]

builtinVarFixities :: [(Var, OpFixity)]
builtinVarFixities = []

defaultFixity :: OpFixity
defaultFixity = (RightAssoc, 9)

consFixity :: OpFixity
consFixity = (RightAssoc, 5)

negateFixity :: OpFixity
negateFixity = (LeftAssoc, 6)

fixityToAssoc :: Fixity -> Assoc
fixityToAssoc Infixl  = LeftAssoc
fixityToAssoc Infixr  = RightAssoc
fixityToAssoc Infix   = NonAssoc
\end{code}

\begin{code}
unfoldAppExp :: Exp -> [Exp]
unfoldAppExp  (OpAppExp e1 op _ e2 sloc) =
    unfoldAppExp (AppExp (AppExp op e1 sloc) e2 sloc)

unfoldAppExp  (NegAppExp e sloc) =
    unfoldAppExp (AppExp (VarExp (Var prelNegate) sloc) e sloc)

unfoldAppExp  (AppExp e1 e2 _) =
    (unfoldAppExp e1) ++ [e2]

unfoldAppExp  e =
    [e]
\end{code}

\begin{code}
unfoldFunTy :: Type -> [Type]
unfoldFunTy  (AppTy (AppTy (TyConTy (TyCon tycon)) ty1) ty2)
    | tycon == builtinArrow  = ty1 : unfoldFunTy ty2
unfoldFunTy  ty              = [ty]
\end{code}

\begin{code}
destructFunTy :: Type -> ([Type], Type)
destructFunTy ty =
    case reverse (unfoldFunTy ty) of
      res_ty : arg_tys  -> (reverse arg_tys, res_ty)
      []                -> error "destructFunTy: the impossible happened"
\end{code}
