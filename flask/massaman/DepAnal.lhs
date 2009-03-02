%if False
\begin{code}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
-- Module      :  DepAnal
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module DepAnal where

import Data.List (elem)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Digraph
import Data.Name
\end{code}
%endif

\chapter{Dependency Analysis}

The actual dependency analysis involves building a graph representing the
dependencies between values and calculating the strongly connected components of
the graph, as described in~\cite{simonpj87implementation}.

\begin{code}
defGroups  ::  forall a n . (Ord a, Ord n)
           =>  [(a, n, [n])]
           ->  [Rec a]
defGroups deps = sccs
  where
    sccs =  reverse $
            mapMaybe (\c -> sccToRec $ mapMaybe lookupDepend $ Set.toList c) $
            scc  (\u -> Map.findWithDefault [] u ins)
                 (\u -> Map.findWithDefault [] u outs)
                 binders

    binders :: [n]
    binders = [n | (_, n, _) <- deps]

    provides :: a -> n
    provides a = Map.fromList [(a, n) | (a, n, _) <- deps] Map.! a

    requires :: a -> [n]
    requires a = Map.fromList [(a, ns) | (a, _, ns) <- deps] Map.! a

    defs  :: Map.Map n a
    defs = Map.fromList [(n, a) | (a, n, _) <- deps]

    edges :: [(n, n)]
    edges = [(n', n) | (_, n, ns) <- deps, n' <- ns]

    ins   :: Map.Map n [n]
    ins = Map.fromList [(n, ns) |  n <- map snd edges,
                                   let ns = [x | (x, y) <- edges, y == n]]

    outs  :: Map.Map n [n]
    outs = Map.fromList [(n, ns) |  n <- map fst edges,
                                    let ns = [y | (x, y) <- edges, x == n]]

    lookupDepend :: n -> Maybe a
    lookupDepend n = Map.lookup n defs

    sccToRec :: [a] -> Maybe (Rec a)
    sccToRec  []   = Nothing
    sccToRec  [a]  = Just $ if bv `elem` fvs then Rec [a] else NonRec a
      where
        bv :: n
        bv = provides a

        fvs :: [n]
        fvs = requires a

    sccToRec  as   = Just $ Rec as
\end{code}
