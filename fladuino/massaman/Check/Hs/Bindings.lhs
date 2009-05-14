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
-- Module      :  Check.Hs.Bindings
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Check.Hs.Bindings where

import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)

import Data.Name
import Language.Hs
import Text.PrettyPrint.Mainland

import DepAnal
\end{code}
%endif

\section{Bindings and Dependency Analysis}

\subsection{Converting declarations to bindings}

Before performing dependency analysis, we need to coalesce all function and
pattern bindings to produce a list of |Binding|'s. By this point all pattern
bindings should be de-sugared into single variable bindings, so their
translation is trivial. For function definitions, we must collect all cases for
a given function and bundle them together.

\begin{code}
bindingsFromDecls  ::  [Decl]
                   ->  [Binding]
bindingsFromDecls decls =
    bindings ++
    [SigBind v ty (Map.findWithDefault defaultFixity n fixities)
         | (v@(Var n), ty) <- Map.toList freeSigs] ++
    [FixityBind n fix
         | (n, fix) <- Map.toList freeFixities]
  where
    sigs :: Map.Map Var Type
    sigs = Map.fromList (concat $ map go decls)
      where
        go :: Decl -> [(Var, Type)]
        go  (SigDecl vs ty _)  = [(v, ty) | v <- vs]
        go  _                  = []

    freeSigs :: Map.Map Var Type
    freeSigs =  foldl'  (flip Map.delete) sigs
                        (binders bindings)

    fixities :: Map.Map Name OpFixity
    fixities = Map.fromList (concat $ map go decls)
      where
        go :: Decl -> [(Name, OpFixity)]
        go  (FixityDecl ns f i _)  = [(n, (fixityToAssoc f, i)) | n <- ns]
        go  _                      = []

    freeFixities :: Map.Map Name OpFixity
    freeFixities =  foldl'  (flip Map.delete) fixities
                            (map bindingName bindings)

    bindings :: [Binding]
    bindings = bindDecls decls

    bindDecls :: [Decl] -> [Binding]
    bindDecls  []                                     = []
    bindDecls  decls@(VarBindDecl v@(Var n) _ _ _ : _)  =
        varBinding : bindDecls rest
      where
        group, rest :: [Decl]
        (group, rest) = span sameBinding decls

        sameBinding :: Decl -> Bool
        sameBinding  (VarBindDecl v' _ _ _)  = v' == v
        sameBinding  _                       = False

        sig :: Maybe Type
        sig = Map.lookup v sigs

        fixity :: OpFixity
        fixity = Map.findWithDefault defaultFixity n fixities

        varBinding :: Binding
        varBinding = VarBind v sig fixity (map varBindArgs group)

        varBindArgs :: Decl -> ([Pat], Rhs)
        varBindArgs  (VarBindDecl _ ps rhs _)  = (ps, rhs)
        varBindArgs  _                         = error "varBindArgs: saw non-VarBindDecl"

    bindDecls  (PatBindDecl (AsPat v@(Var n) (WildPat _) _) rhs _ : decls) =
        PatBind v sig fixity rhs : bindDecls decls
      where
        sig :: Maybe Type
        sig = Map.lookup v sigs

        fixity :: OpFixity
        fixity = Map.findWithDefault defaultFixity n fixities

    bindDecls  (PatBindDecl _ _ _ : _) =
        error "internal error: bound patterns may only contain a single variable"

    bindDecls  (_                    : decls)  =  bindDecls decls
\end{code}

\begin{code}
varBindings :: [Binding] -> [(Var, Binding)]
varBindings = mapMaybe go
  where
    go :: Binding -> Maybe (Var, Binding)
    go  b@(VarBind v _ _ _)  = Just (v, b)
    go  _                    = Nothing
\end{code}

\begin{code}
sigBindings :: [Binding] -> [(Var, Type)]
sigBindings = mapMaybe go
  where
    go :: Binding -> Maybe (Var, Type)
    go  (SigBind v ty _)           = Just (v, ty)
    go  (VarBind v (Just ty) _ _)  = Just (v, ty)
    go  (PatBind v (Just ty) _ _)  = Just (v, ty)
    go  _                          = Nothing
\end{code}

\begin{code}
freeSigBindings :: [Binding] -> [(Var, Type)]
freeSigBindings = mapMaybe go
  where
    go :: Binding -> Maybe (Var, Type)
    go  (SigBind v ty _)           = Just (v, ty)
    go  _                          = Nothing
\end{code}

\subsection{Sorting type declaration groups}

Type declaration groups consist of @type@ declarations, @data@ declarations, and
@class@ declarations. We'd like to have the constituents declarations of each
type declaration dependency group to be partition by the type of the declaration
so we can work with the independently. The function |partitionTypeDecls| does
this for us, taking a list of dependency groups (|[Rec Decl]|) and returning a
list of type declaration dependency groups (|[TypeDef]|).

\begin{code}
data TypeDef  =  TypeDef Decl
              |  RecTypeDef ([Decl], [Decl], [Decl])
\end{code}

\begin{code}
partitionTypeDecls :: [Rec Decl] -> [TypeDef]
partitionTypeDecls [] = []
partitionTypeDecls (NonRec decl : defs) =
    TypeDef decl : partitionTypeDecls defs

partitionTypeDecls (Rec decls : defs) =
    RecTypeDef (partition decls) : partitionTypeDecls defs
  where
    partition :: [Decl] -> ([Decl], [Decl], [Decl])
    partition decls = go decls ([], [], [])
      where
        go  ::  [Decl]
            ->  ([Decl], [Decl], [Decl])
            ->  ([Decl], [Decl], [Decl])
        go [] (typedecls, datadecls, classdecls) =
            (reverse typedecls, reverse datadecls, reverse classdecls)
        go (decl : decls) (typedecls, datadecls, classdecls) =
            go decls $ classify decl decl (typedecls, datadecls, classdecls)

        classify  ::  Decl
                  ->  Decl
                  ->  ([Decl], [Decl], [Decl])
                  ->  ([Decl], [Decl], [Decl])
        classify curdecl (TypeDecl _ _ _ _) (typedecls, datadecls, classdecls) =
            (curdecl : typedecls, datadecls, classdecls)
        classify curdecl (DataDecl _ _ _ _ _ _ _) (typedecls, datadecls, classdecls) =
            (typedecls, curdecl : datadecls, classdecls)
        classify curdecl (ClassDecl _ _ _ _ _) (typedecls, datadecls, classdecls) =
            (typedecls, datadecls, curdecl : classdecls)
        classify _ _ _ = error "classify: saw non-type declaration"
\end{code}

\subsection{Grouping type declarations and bindings by dependencies}

\begin{code}
analyzeTypeDeclDependencies  ::  [Decl]
                             ->  [TypeDef]
analyzeTypeDeclDependencies typedecls =
    partitionTypeDecls $
    defGroups [(b, tycon, tycons) |  b <- typedecls,
                                     let [tycon] = binders b :: [TyCon],
                                     let tycons = Set.toList (free b)]
\end{code}

\begin{code}
analyzeBindingDependencies  ::  [Binding]
                            ->  [Rec Binding]
analyzeBindingDependencies bindings =
    defGroups [(b, v, vs) |  b <- filter isBinding bindings,
                             let [v] = binders b :: [Var],
                             let vs = Set.toList (free b)]
  where
    isBinding :: Binding -> Bool
    isBinding  (VarBind _ _ _ _)  = True
    isBinding  (PatBind _ _ _ _)  = True
    isBinding  _                  = False
\end{code}
