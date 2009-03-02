{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverlappingInstances #-}

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
-- Module      :  Data.Loc.Pretty
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Data.Loc.Pretty where

import System.FilePath

import Data.Loc
import Text.PrettyPrint.Mainland

instance Pretty Pos where
    ppr (Pos f l c _) =
        text (takeFileName f) <> text ":" <> ppr l <> text ":" <> ppr c

instance Pretty Loc where
    ppr (Loc p1@(Pos f1 l1 c1 _) p2@(Pos f2 l2 c2 _))
        | f1 == f2   = text (takeFileName f1) <> text ":"
                       <//> pprLineCol l1 c1 l2 c2
        | otherwise  = ppr p1 <> text "-" <> ppr p2
      where
        pprLineCol :: Int -> Int -> Int -> Int -> Doc
        pprLineCol l1 c1 l2 c2
            | l1 == l2 && c1 == c2  =  ppr l1 <//> text ":" <//> ppr c1
            | l1 == l2 && c1 /= c2  =  ppr l1 <//> text ":" <//> ppr c1
                                       <> text "-" <>
                                       ppr c2
            | otherwise             =  ppr l1 <//> text ":" <//> ppr c1
                                       <> text "-" <>
                                       ppr l2 <//> text ":" <//> ppr c2

instance Pretty x => Pretty (L x) where
    pprPrec p (L _ x) = pprPrec p x

instance Show Pos where
    show = pprint

instance Show Loc where
    show = pprint
