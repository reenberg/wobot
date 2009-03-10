%if False
\begin{code}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Copyright (c) 2007-2008
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
-- Module      :  Data.Digraph
-- Copyright   :  (c) Harvard University 2007-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Data.Digraph where

import Data.List (foldl')
import qualified Data.Set as Set
\end{code}
%endif

\begin{code}
depthFirstSearch  ::  forall a . Ord a
                  =>  (a -> [a])
                  ->  (Set.Set a, [a])
                  ->  [a]
                  ->  (Set.Set a, [a])
depthFirstSearch = foldl' . search
  where
    search  ::  (a -> [a])
            ->  (Set.Set a, [a])
            ->  a
            ->  (Set.Set a, [a])
    search relation (visited, sequence) vertex
        | vertex `Set.member` visited  = (visited, sequence)
        | otherwise                    = (visited', vertex : sequence')
      where
        (visited', sequence') =
            depthFirstSearch  relation
                              (Set.insert vertex visited, sequence)
                              (relation vertex)
\end{code}

\begin{code}
spanningSearch  ::  forall a . Ord a
                =>  (a -> [a])
                ->  (Set.Set a, [Set.Set a])
                ->  [a]
                ->  (Set.Set a, [Set.Set a])
spanningSearch relation s as = foldl' (search relation) s as
  where
    search  ::  (a -> [a])
            ->  (Set.Set a, [Set.Set a])
            ->  a
            ->  (Set.Set a, [Set.Set a])
    search relation (visited, setSequence) vertex
        | vertex `Set.member` visited  =  (visited,   setSequence)
        | otherwise                    =  (visited',  setSequence')
      where
        (visited', sequence) =
            depthFirstSearch  relation
                              (Set.insert vertex visited, [])
                              (relation vertex)

        setSequence' = Set.fromList (vertex : sequence) : setSequence
\end{code}

\begin{code}
scc  ::  Ord a
     =>  (a -> [a])
     ->  (a -> [a])
     ->  [a]
     ->  [Set.Set a]
scc ins outs = spanning . depthFirst
  where
    depthFirst  = snd . depthFirstSearch  outs  (Set.empty, [])
    spanning    = snd . spanningSearch    ins   (Set.empty, [])
\end{code}
