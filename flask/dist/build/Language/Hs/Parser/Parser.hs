{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS -w #-}
{-# OPTIONS -fglasgow-exts #-}

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
-- Module      :  Language.Hs.Parser.Parser
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.Hs.Parser.Parser (
    parseModule,
    parseBody,
    parseTopDecls,
    parseDecls,
    parseType,
    parseExp,
    parseStmt
  ) where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.List (foldl1')
import qualified Data.Map as Map

import Compiler.Opt
import Control.Monad.ContextException
import Control.Monad.Exception
import Data.Loc
import Data.Name
import Language.Hs
import Language.Hs.Parser.Exceptions
import Language.Hs.Parser.Layout
import Language.Hs.Parser.Lexer
import Language.Hs.Parser.Monad
import Language.Hs.Parser.Tokens
import Language.Hs.Parser.Utils
import Text.PrettyPrint.Mainland
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
#else
import Array
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

-- parser produced by Happy Version 1.18.2

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = GHC.Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn10 :: (L Exp) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (L Exp)
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (L Name) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (L Name)
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (L Name) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (L Name)
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (L Name) -> (HappyAbsSyn )
happyIn13 x = unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (L Name)
happyOut13 x = unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: (L Name) -> (HappyAbsSyn )
happyIn14 x = unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> (L Name)
happyOut14 x = unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (L Name) -> (HappyAbsSyn )
happyIn15 x = unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (L Name)
happyOut15 x = unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (L Name) -> (HappyAbsSyn )
happyIn16 x = unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (L Name)
happyOut16 x = unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (L Name) -> (HappyAbsSyn )
happyIn17 x = unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (L Name)
happyOut17 x = unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (L Name) -> (HappyAbsSyn )
happyIn18 x = unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (L Name)
happyOut18 x = unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (L Name) -> (HappyAbsSyn )
happyIn19 x = unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (L Name)
happyOut19 x = unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (L Name) -> (HappyAbsSyn )
happyIn20 x = unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (L Name)
happyOut20 x = unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (L Name) -> (HappyAbsSyn )
happyIn21 x = unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (L Name)
happyOut21 x = unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (L Name) -> (HappyAbsSyn )
happyIn22 x = unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (L Name)
happyOut22 x = unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (L Name) -> (HappyAbsSyn )
happyIn23 x = unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (L Name)
happyOut23 x = unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (L Name) -> (HappyAbsSyn )
happyIn24 x = unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (L Name)
happyOut24 x = unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (L Name) -> (HappyAbsSyn )
happyIn25 x = unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (L Name)
happyOut25 x = unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (L Name) -> (HappyAbsSyn )
happyIn26 x = unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (L Name)
happyOut26 x = unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (Module) -> (HappyAbsSyn )
happyIn27 x = unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (Module)
happyOut27 x = unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: ([Export]) -> (HappyAbsSyn )
happyIn28 x = unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> ([Export])
happyOut28 x = unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: ([Export]) -> (HappyAbsSyn )
happyIn29 x = unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> ([Export])
happyOut29 x = unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: ([L Export]) -> (HappyAbsSyn )
happyIn30 x = unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> ([L Export])
happyOut30 x = unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (L Export) -> (HappyAbsSyn )
happyIn31 x = unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (L Export)
happyOut31 x = unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (L ImportDecl) -> (HappyAbsSyn )
happyIn32 x = unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (L ImportDecl)
happyOut32 x = unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: ([L ImportDecl]) -> (HappyAbsSyn )
happyIn33 x = unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> ([L ImportDecl])
happyOut33 x = unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (Bool) -> (HappyAbsSyn )
happyIn34 x = unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (Bool)
happyOut34 x = unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (Maybe Name) -> (HappyAbsSyn )
happyIn35 x = unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (Maybe Name)
happyOut35 x = unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: ((Bool, [L Import])) -> (HappyAbsSyn )
happyIn36 x = unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> ((Bool, [L Import]))
happyOut36 x = unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: ([L Import]) -> (HappyAbsSyn )
happyIn37 x = unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> ([L Import])
happyOut37 x = unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (L Import) -> (HappyAbsSyn )
happyIn38 x = unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (L Import)
happyOut38 x = unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: ([L CName]) -> (HappyAbsSyn )
happyIn39 x = unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> ([L CName])
happyOut39 x = unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (L CName) -> (HappyAbsSyn )
happyIn40 x = unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (L CName)
happyOut40 x = unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (L Body) -> (HappyAbsSyn )
happyIn41 x = unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (L Body)
happyOut41 x = unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: ([L Decl]) -> (HappyAbsSyn )
happyIn42 x = unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> ([L Decl])
happyOut42 x = unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (L Decl) -> (HappyAbsSyn )
happyIn43 x = unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (L Decl)
happyOut43 x = unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: (L [Pred]) -> (HappyAbsSyn )
happyIn44 x = unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> (L [Pred])
happyOut44 x = unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (L ([Pred], Type)) -> (HappyAbsSyn )
happyIn45 x = unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> (L ([Pred], Type))
happyOut45 x = unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: ([L Decl]) -> (HappyAbsSyn )
happyIn46 x = unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> ([L Decl])
happyOut46 x = unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: ([L Decl]) -> (HappyAbsSyn )
happyIn47 x = unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> ([L Decl])
happyOut47 x = unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (L Decl) -> (HappyAbsSyn )
happyIn48 x = unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (L Decl)
happyOut48 x = unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (L Decl) -> (HappyAbsSyn )
happyIn49 x = unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (L Decl)
happyOut49 x = unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: ([L Name]) -> (HappyAbsSyn )
happyIn50 x = unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> ([L Name])
happyOut50 x = unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: ([L Var]) -> (HappyAbsSyn )
happyIn51 x = unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> ([L Var])
happyOut51 x = unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: (L Fixity) -> (HappyAbsSyn )
happyIn52 x = unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> (L Fixity)
happyOut52 x = unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: (L Type) -> (HappyAbsSyn )
happyIn53 x = unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> (L Type)
happyOut53 x = unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: ([L Type]) -> (HappyAbsSyn )
happyIn54 x = unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> ([L Type])
happyOut54 x = unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyIn55 :: ([L Type]) -> (HappyAbsSyn )
happyIn55 x = unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> ([L Type])
happyOut55 x = unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: ([L Type]) -> (HappyAbsSyn )
happyIn56 x = unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> ([L Type])
happyOut56 x = unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyIn57 :: (L Type) -> (HappyAbsSyn )
happyIn57 x = unsafeCoerce# x
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> (L Type)
happyOut57 x = unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyIn58 :: (L Type) -> (HappyAbsSyn )
happyIn58 x = unsafeCoerce# x
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> (L Type)
happyOut58 x = unsafeCoerce# x
{-# INLINE happyOut58 #-}
happyIn59 :: (L TyCon) -> (HappyAbsSyn )
happyIn59 x = unsafeCoerce# x
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> (L TyCon)
happyOut59 x = unsafeCoerce# x
{-# INLINE happyOut59 #-}
happyIn60 :: (Int) -> (HappyAbsSyn )
happyIn60 x = unsafeCoerce# x
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> (Int)
happyOut60 x = unsafeCoerce# x
{-# INLINE happyOut60 #-}
happyIn61 :: ([L ConDecl]) -> (HappyAbsSyn )
happyIn61 x = unsafeCoerce# x
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> ([L ConDecl])
happyOut61 x = unsafeCoerce# x
{-# INLINE happyOut61 #-}
happyIn62 :: (L ConDecl) -> (HappyAbsSyn )
happyIn62 x = unsafeCoerce# x
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> (L ConDecl)
happyOut62 x = unsafeCoerce# x
{-# INLINE happyOut62 #-}
happyIn63 :: (L ConDecl) -> (HappyAbsSyn )
happyIn63 x = unsafeCoerce# x
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> (L ConDecl)
happyOut63 x = unsafeCoerce# x
{-# INLINE happyOut63 #-}
happyIn64 :: ([(Var, Type)]) -> (HappyAbsSyn )
happyIn64 x = unsafeCoerce# x
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> ([(Var, Type)])
happyOut64 x = unsafeCoerce# x
{-# INLINE happyOut64 #-}
happyIn65 :: ([(Var, Type)]) -> (HappyAbsSyn )
happyIn65 x = unsafeCoerce# x
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> ([(Var, Type)])
happyOut65 x = unsafeCoerce# x
{-# INLINE happyOut65 #-}
happyIn66 :: ([L TyCon]) -> (HappyAbsSyn )
happyIn66 x = unsafeCoerce# x
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> ([L TyCon])
happyOut66 x = unsafeCoerce# x
{-# INLINE happyOut66 #-}
happyIn67 :: ([L TyCon]) -> (HappyAbsSyn )
happyIn67 x = unsafeCoerce# x
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> ([L TyCon])
happyOut67 x = unsafeCoerce# x
{-# INLINE happyOut67 #-}
happyIn68 :: (L Rhs) -> (HappyAbsSyn )
happyIn68 x = unsafeCoerce# x
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> (L Rhs)
happyOut68 x = unsafeCoerce# x
{-# INLINE happyOut68 #-}
happyIn69 :: ([L Decl]) -> (HappyAbsSyn )
happyIn69 x = unsafeCoerce# x
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> ([L Decl])
happyOut69 x = unsafeCoerce# x
{-# INLINE happyOut69 #-}
happyIn70 :: ([L (Maybe Exp, Exp)]) -> (HappyAbsSyn )
happyIn70 x = unsafeCoerce# x
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> ([L (Maybe Exp, Exp)])
happyOut70 x = unsafeCoerce# x
{-# INLINE happyOut70 #-}
happyIn71 :: (L (Maybe Exp, Exp)) -> (HappyAbsSyn )
happyIn71 x = unsafeCoerce# x
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> (L (Maybe Exp, Exp))
happyOut71 x = unsafeCoerce# x
{-# INLINE happyOut71 #-}
happyIn72 :: (L Exp) -> (HappyAbsSyn )
happyIn72 x = unsafeCoerce# x
{-# INLINE happyIn72 #-}
happyOut72 :: (HappyAbsSyn ) -> (L Exp)
happyOut72 x = unsafeCoerce# x
{-# INLINE happyOut72 #-}
happyIn73 :: (L Exp) -> (HappyAbsSyn )
happyIn73 x = unsafeCoerce# x
{-# INLINE happyIn73 #-}
happyOut73 :: (HappyAbsSyn ) -> (L Exp)
happyOut73 x = unsafeCoerce# x
{-# INLINE happyOut73 #-}
happyIn74 :: (L Exp) -> (HappyAbsSyn )
happyIn74 x = unsafeCoerce# x
{-# INLINE happyIn74 #-}
happyOut74 :: (HappyAbsSyn ) -> (L Exp)
happyOut74 x = unsafeCoerce# x
{-# INLINE happyOut74 #-}
happyIn75 :: (L Exp) -> (HappyAbsSyn )
happyIn75 x = unsafeCoerce# x
{-# INLINE happyIn75 #-}
happyOut75 :: (HappyAbsSyn ) -> (L Exp)
happyOut75 x = unsafeCoerce# x
{-# INLINE happyOut75 #-}
happyIn76 :: (L Exp) -> (HappyAbsSyn )
happyIn76 x = unsafeCoerce# x
{-# INLINE happyIn76 #-}
happyOut76 :: (HappyAbsSyn ) -> (L Exp)
happyOut76 x = unsafeCoerce# x
{-# INLINE happyOut76 #-}
happyIn77 :: (L Exp) -> (HappyAbsSyn )
happyIn77 x = unsafeCoerce# x
{-# INLINE happyIn77 #-}
happyOut77 :: (HappyAbsSyn ) -> (L Exp)
happyOut77 x = unsafeCoerce# x
{-# INLINE happyOut77 #-}
happyIn78 :: (L Exp) -> (HappyAbsSyn )
happyIn78 x = unsafeCoerce# x
{-# INLINE happyIn78 #-}
happyOut78 :: (HappyAbsSyn ) -> (L Exp)
happyOut78 x = unsafeCoerce# x
{-# INLINE happyOut78 #-}
happyIn79 :: ([L Exp]) -> (HappyAbsSyn )
happyIn79 x = unsafeCoerce# x
{-# INLINE happyIn79 #-}
happyOut79 :: (HappyAbsSyn ) -> ([L Exp])
happyOut79 x = unsafeCoerce# x
{-# INLINE happyOut79 #-}
happyIn80 :: (L Exp) -> (HappyAbsSyn )
happyIn80 x = unsafeCoerce# x
{-# INLINE happyIn80 #-}
happyOut80 :: (HappyAbsSyn ) -> (L Exp)
happyOut80 x = unsafeCoerce# x
{-# INLINE happyOut80 #-}
happyIn81 :: (L Exp) -> (HappyAbsSyn )
happyIn81 x = unsafeCoerce# x
{-# INLINE happyIn81 #-}
happyOut81 :: (HappyAbsSyn ) -> (L Exp)
happyOut81 x = unsafeCoerce# x
{-# INLINE happyOut81 #-}
happyIn82 :: (L Exp) -> (HappyAbsSyn )
happyIn82 x = unsafeCoerce# x
{-# INLINE happyIn82 #-}
happyOut82 :: (HappyAbsSyn ) -> (L Exp)
happyOut82 x = unsafeCoerce# x
{-# INLINE happyOut82 #-}
happyIn83 :: (L Exp) -> (HappyAbsSyn )
happyIn83 x = unsafeCoerce# x
{-# INLINE happyIn83 #-}
happyOut83 :: (HappyAbsSyn ) -> (L Exp)
happyOut83 x = unsafeCoerce# x
{-# INLINE happyOut83 #-}
happyIn84 :: ([L Exp]) -> (HappyAbsSyn )
happyIn84 x = unsafeCoerce# x
{-# INLINE happyIn84 #-}
happyOut84 :: (HappyAbsSyn ) -> ([L Exp])
happyOut84 x = unsafeCoerce# x
{-# INLINE happyOut84 #-}
happyIn85 :: ([L Exp]) -> (HappyAbsSyn )
happyIn85 x = unsafeCoerce# x
{-# INLINE happyIn85 #-}
happyOut85 :: (HappyAbsSyn ) -> ([L Exp])
happyOut85 x = unsafeCoerce# x
{-# INLINE happyOut85 #-}
happyIn86 :: ([L Exp]) -> (HappyAbsSyn )
happyIn86 x = unsafeCoerce# x
{-# INLINE happyIn86 #-}
happyOut86 :: (HappyAbsSyn ) -> ([L Exp])
happyOut86 x = unsafeCoerce# x
{-# INLINE happyOut86 #-}
happyIn87 :: ((Var, Exp)) -> (HappyAbsSyn )
happyIn87 x = unsafeCoerce# x
{-# INLINE happyIn87 #-}
happyOut87 :: (HappyAbsSyn ) -> ((Var, Exp))
happyOut87 x = unsafeCoerce# x
{-# INLINE happyOut87 #-}
happyIn88 :: ([(Var, Exp)]) -> (HappyAbsSyn )
happyIn88 x = unsafeCoerce# x
{-# INLINE happyIn88 #-}
happyOut88 :: (HappyAbsSyn ) -> ([(Var, Exp)])
happyOut88 x = unsafeCoerce# x
{-# INLINE happyOut88 #-}
happyIn89 :: (L Pat) -> (HappyAbsSyn )
happyIn89 x = unsafeCoerce# x
{-# INLINE happyIn89 #-}
happyOut89 :: (HappyAbsSyn ) -> (L Pat)
happyOut89 x = unsafeCoerce# x
{-# INLINE happyOut89 #-}
happyIn90 :: (L Pat) -> (HappyAbsSyn )
happyIn90 x = unsafeCoerce# x
{-# INLINE happyIn90 #-}
happyOut90 :: (HappyAbsSyn ) -> (L Pat)
happyOut90 x = unsafeCoerce# x
{-# INLINE happyOut90 #-}
happyIn91 :: ([L Pat]) -> (HappyAbsSyn )
happyIn91 x = unsafeCoerce# x
{-# INLINE happyIn91 #-}
happyOut91 :: (HappyAbsSyn ) -> ([L Pat])
happyOut91 x = unsafeCoerce# x
{-# INLINE happyOut91 #-}
happyIn92 :: (L Qual) -> (HappyAbsSyn )
happyIn92 x = unsafeCoerce# x
{-# INLINE happyIn92 #-}
happyOut92 :: (HappyAbsSyn ) -> (L Qual)
happyOut92 x = unsafeCoerce# x
{-# INLINE happyOut92 #-}
happyIn93 :: ([L Qual]) -> (HappyAbsSyn )
happyIn93 x = unsafeCoerce# x
{-# INLINE happyIn93 #-}
happyOut93 :: (HappyAbsSyn ) -> ([L Qual])
happyOut93 x = unsafeCoerce# x
{-# INLINE happyOut93 #-}
happyIn94 :: (L Alt) -> (HappyAbsSyn )
happyIn94 x = unsafeCoerce# x
{-# INLINE happyIn94 #-}
happyOut94 :: (HappyAbsSyn ) -> (L Alt)
happyOut94 x = unsafeCoerce# x
{-# INLINE happyOut94 #-}
happyIn95 :: ([L Alt]) -> (HappyAbsSyn )
happyIn95 x = unsafeCoerce# x
{-# INLINE happyIn95 #-}
happyOut95 :: (HappyAbsSyn ) -> ([L Alt])
happyOut95 x = unsafeCoerce# x
{-# INLINE happyOut95 #-}
happyIn96 :: (L [(Maybe Exp, Exp)]) -> (HappyAbsSyn )
happyIn96 x = unsafeCoerce# x
{-# INLINE happyIn96 #-}
happyOut96 :: (HappyAbsSyn ) -> (L [(Maybe Exp, Exp)])
happyOut96 x = unsafeCoerce# x
{-# INLINE happyOut96 #-}
happyIn97 :: (L Stmt) -> (HappyAbsSyn )
happyIn97 x = unsafeCoerce# x
{-# INLINE happyIn97 #-}
happyOut97 :: (HappyAbsSyn ) -> (L Stmt)
happyOut97 x = unsafeCoerce# x
{-# INLINE happyOut97 #-}
happyIn98 :: ([L Stmt]) -> (HappyAbsSyn )
happyIn98 x = unsafeCoerce# x
{-# INLINE happyIn98 #-}
happyOut98 :: (HappyAbsSyn ) -> ([L Stmt])
happyOut98 x = unsafeCoerce# x
{-# INLINE happyOut98 #-}
happyIn99 :: (L Con) -> (HappyAbsSyn )
happyIn99 x = unsafeCoerce# x
{-# INLINE happyIn99 #-}
happyOut99 :: (HappyAbsSyn ) -> (L Con)
happyOut99 x = unsafeCoerce# x
{-# INLINE happyOut99 #-}
happyIn100 :: (L Var) -> (HappyAbsSyn )
happyIn100 x = unsafeCoerce# x
{-# INLINE happyIn100 #-}
happyOut100 :: (HappyAbsSyn ) -> (L Var)
happyOut100 x = unsafeCoerce# x
{-# INLINE happyOut100 #-}
happyIn101 :: (L Var) -> (HappyAbsSyn )
happyIn101 x = unsafeCoerce# x
{-# INLINE happyIn101 #-}
happyOut101 :: (HappyAbsSyn ) -> (L Var)
happyOut101 x = unsafeCoerce# x
{-# INLINE happyOut101 #-}
happyIn102 :: (L Con) -> (HappyAbsSyn )
happyIn102 x = unsafeCoerce# x
{-# INLINE happyIn102 #-}
happyOut102 :: (HappyAbsSyn ) -> (L Con)
happyOut102 x = unsafeCoerce# x
{-# INLINE happyOut102 #-}
happyIn103 :: (L Con) -> (HappyAbsSyn )
happyIn103 x = unsafeCoerce# x
{-# INLINE happyIn103 #-}
happyOut103 :: (HappyAbsSyn ) -> (L Con)
happyOut103 x = unsafeCoerce# x
{-# INLINE happyOut103 #-}
happyIn104 :: (L Var) -> (HappyAbsSyn )
happyIn104 x = unsafeCoerce# x
{-# INLINE happyIn104 #-}
happyOut104 :: (HappyAbsSyn ) -> (L Var)
happyOut104 x = unsafeCoerce# x
{-# INLINE happyOut104 #-}
happyIn105 :: (L Var) -> (HappyAbsSyn )
happyIn105 x = unsafeCoerce# x
{-# INLINE happyIn105 #-}
happyOut105 :: (HappyAbsSyn ) -> (L Var)
happyOut105 x = unsafeCoerce# x
{-# INLINE happyOut105 #-}
happyIn106 :: (L Var) -> (HappyAbsSyn )
happyIn106 x = unsafeCoerce# x
{-# INLINE happyIn106 #-}
happyOut106 :: (HappyAbsSyn ) -> (L Var)
happyOut106 x = unsafeCoerce# x
{-# INLINE happyOut106 #-}
happyIn107 :: (L Con) -> (HappyAbsSyn )
happyIn107 x = unsafeCoerce# x
{-# INLINE happyIn107 #-}
happyOut107 :: (HappyAbsSyn ) -> (L Con)
happyOut107 x = unsafeCoerce# x
{-# INLINE happyOut107 #-}
happyIn108 :: (L Con) -> (HappyAbsSyn )
happyIn108 x = unsafeCoerce# x
{-# INLINE happyIn108 #-}
happyOut108 :: (HappyAbsSyn ) -> (L Con)
happyOut108 x = unsafeCoerce# x
{-# INLINE happyOut108 #-}
happyIn109 :: (L Name) -> (HappyAbsSyn )
happyIn109 x = unsafeCoerce# x
{-# INLINE happyIn109 #-}
happyOut109 :: (HappyAbsSyn ) -> (L Name)
happyOut109 x = unsafeCoerce# x
{-# INLINE happyOut109 #-}
happyIn110 :: (L Exp) -> (HappyAbsSyn )
happyIn110 x = unsafeCoerce# x
{-# INLINE happyIn110 #-}
happyOut110 :: (HappyAbsSyn ) -> (L Exp)
happyOut110 x = unsafeCoerce# x
{-# INLINE happyOut110 #-}
happyIn111 :: (L Exp) -> (HappyAbsSyn )
happyIn111 x = unsafeCoerce# x
{-# INLINE happyIn111 #-}
happyOut111 :: (HappyAbsSyn ) -> (L Exp)
happyOut111 x = unsafeCoerce# x
{-# INLINE happyOut111 #-}
happyIn112 :: (L Con) -> (HappyAbsSyn )
happyIn112 x = unsafeCoerce# x
{-# INLINE happyIn112 #-}
happyOut112 :: (HappyAbsSyn ) -> (L Con)
happyOut112 x = unsafeCoerce# x
{-# INLINE happyOut112 #-}
happyInTok :: ((L Token)) -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> ((L Token))
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x04\x00\x30\x03\x87\x09\x2f\x03\x40\x0b\x07\x09\xc6\x08\x66\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xed\x06\x00\x00\x00\x00\x00\x00\x56\x0b\x00\x00\x2b\x03\x00\x00\x36\x03\x21\x03\x00\x00\x33\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x85\x08\x26\x03\x85\x08\x25\x03\x00\x00\x00\x00\x00\x00\x00\x00\x56\x0b\x56\x0b\x56\x0b\xfb\x05\x3c\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x03\x2e\x07\x23\x03\x00\x00\x00\x00\x00\x00\x00\x00\x15\x03\x9e\x0b\x00\x00\x00\x00\xec\x09\xac\x09\x00\x00\x15\x03\x83\x0a\xdd\xff\x00\x00\x00\x00\x00\x00\x5b\x00\xa1\x04\xac\x06\x80\x00\x40\x0b\x40\x0b\x22\x03\x00\x00\x00\x00\x00\x00\x40\x0b\x40\x0b\x40\x0b\x14\x03\x48\x09\x14\x03\x00\x00\x4c\x03\x11\x03\x00\x00\x00\x00\xa0\x01\x5e\x01\x24\x03\x00\x00\x1e\x03\x13\x03\x19\x03\x00\x00\x99\x0b\x20\x03\x40\x0b\x0a\x00\x20\x03\x56\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x37\x00\x00\x00\x00\x00\x00\x00\x01\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x85\x08\x85\x08\x00\x00\x00\x00\x1b\x03\x00\x00\x00\x00\x08\x03\x00\x00\x00\x00\x00\x00\x47\x01\xf5\x02\x40\x0b\x2a\x01\x87\x09\x40\x01\x00\x00\xfe\x02\x00\x00\x01\x03\xf4\x02\xff\x02\xf7\x02\x00\x00\xf2\x02\x00\x00\x40\x0b\x10\x03\x44\x0a\x40\x0b\x0d\x01\xed\x02\xef\x02\x00\x00\xea\x02\xe8\x02\x00\x00\xc3\x02\x2e\x07\xc9\x02\xbf\x02\x00\x00\x00\x00\x44\x0a\xbd\x02\xbb\x02\x56\x0b\x00\x00\xb9\x02\x1b\x03\x00\x00\x00\x00\x00\x00\x00\x00\x17\x0b\xd6\x02\xce\x02\x44\x08\xcd\x02\x03\x08\xe7\x05\x00\x00\x00\x00\x25\x01\xab\x02\xa6\x01\x00\x00\x86\x02\x00\x00\x34\x01\x03\x08\x03\x08\x00\x00\x03\x08\x88\x02\x80\x02\x83\x02\x75\x02\x00\x00\x00\x00\x74\x02\x6f\x07\x00\x00\x03\x08\xc6\x09\x00\x00\x00\x00\x03\x08\x00\x00\x03\x08\xc2\x07\x81\x07\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x02\x00\x00\x00\x00\x40\x0b\x00\x00\x00\x00\x00\x00\x05\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x9c\x00\x00\x00\x66\x02\x64\x02\x4b\x02\x47\x01\x45\x02\x53\x02\x59\x02\x00\x00\x00\x00\x2f\x02\x00\x00\x00\x00\x00\x00\x1b\x00\xa8\x0a\x41\x02\x22\x02\x00\x00\x00\x00\x1e\x00\x6a\x0a\x6a\x0a\x4c\x02\x00\x00\x00\x00\x00\x00\x00\x00\x34\x02\x87\x09\x00\x00\x2a\x02\x00\x00\x70\x06\x00\x00\xf8\x01\x8a\x02\x00\x00\x00\x00\x21\x02\xed\x01\xb7\x00\x00\x00\xfb\x01\x00\x00\x00\x00\x00\x00\x0b\x02\x4a\x0b\x0f\x02\x00\x00\xd7\x01\x77\x0b\xf6\xff\x00\x00\xca\x01\xaf\x05\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x81\x07\x00\x00\x00\x00\x00\x00\x00\x00\xc3\x01\x00\x00\xda\x01\x00\x00\xc0\x01\x00\x00\xbe\x01\x00\x00\xac\x01\xa3\x01\x00\x00\x00\x00\x00\x00\x9f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xba\x01\x00\x00\x40\x07\x00\x00\x01\x0b\x97\x01\xff\x06\xe7\x05\x00\x00\x00\x00\x00\x00\x6f\x07\xd4\x00\x00\x00\x59\x00\x00\x00\xff\x06\xb3\x01\xbe\x06\x7d\x06\x7d\x06\x00\x00\x00\x00\x00\x00\x87\x02\x00\x00\x8c\x01\xe7\x05\x00\x00\x2a\x0a\x9a\x05\xb7\x01\x00\x00\xe7\x05\x00\x00\xf4\xff\x96\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x06\x22\x06\x28\x02\x00\x00\x00\x00\x00\x00\x4d\x01\x7f\x00\x00\x00\x00\x00\x00\x00\x4c\x01\x63\x0b\xef\xff\x00\x00\xb6\x00\x00\x00\xaa\x0b\x00\x00\xff\xff\x00\x00\x00\x00\x51\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\x0a\x00\x00\x36\x00\x01\x0b\x7d\x06\x7b\x01\xc0\x0c\x00\x00\x01\x0b\x00\x00\x00\x00\x00\x00\xe7\x05\x00\x00\x00\x00\x9a\x05\x49\x01\xe4\x01\x00\x00\x00\x00\x63\x0b\x00\x00\x00\x00\x63\x06\x00\x00\xd9\x01\x00\x00\x63\x0b\xee\x05\x00\x00\x00\x00\xb8\x05\x7d\x06\x00\x00\x00\x00\x7d\x06\xb9\x01\x46\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x07\x00\x4f\x01\x71\x01\x53\x01\xbb\x0c\x2b\x04\x64\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x69\x05\x00\x00\x00\x00\x00\x00\x13\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x04\x00\x00\x00\x04\x48\x01\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x05\xf0\x04\xeb\x04\x01\x00\x44\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x69\x05\x3b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x07\x00\x00\x00\x00\x34\x0c\xb2\x0c\x00\x00\x00\x00\xcf\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x98\x05\x97\x00\x00\x00\x2d\x0c\x26\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x0c\xf3\x0b\x96\x0c\x00\x00\x52\x00\x00\x00\x00\x00\x54\x01\xc7\x01\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x07\x0f\x01\xe2\x0b\x04\x01\xf7\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9a\x00\x00\x00\x00\x00\x00\x00\xe6\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdf\x03\xb7\x03\x00\x00\x00\x00\x69\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7d\x05\xa9\x00\xe9\x0b\x11\x00\xc2\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\x00\x00\x00\x89\x0c\x00\x00\xc5\x04\x80\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x05\x00\x00\x00\x00\x00\x00\x00\x00\x74\x04\x00\x00\x00\x00\xeb\x04\x00\x00\xec\x00\xfe\x01\x00\x00\x00\x00\x00\x00\x00\x00\x33\x04\x00\x00\x00\x00\x9d\x00\x00\x00\xac\x03\x4e\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbc\x01\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x03\x81\x03\x00\x00\x58\x03\x00\x00\x00\x00\x00\x00\xdd\x00\x00\x00\x00\x00\x00\x00\x68\x02\x00\x00\x38\x03\xc5\x04\x00\x00\x00\x00\x2d\x03\x00\x00\x0b\x03\x7d\x00\xc2\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x0c\x00\x00\x00\x00\x00\x00\xcf\x02\x00\x00\x00\x00\x00\x00\x00\x00\x8a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x84\x01\x00\x00\x00\x00\xe0\x00\x00\x00\x00\x00\xdf\x00\x00\x00\x00\x00\x00\x00\x16\x01\x4e\x05\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x72\x0c\x65\x0c\xf6\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe5\x00\x10\x01\x00\x00\x00\x00\x00\x00\x31\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xeb\x00\xcf\x00\x00\x00\x00\x00\xb9\x00\x00\x00\x00\x00\x00\x00\xa1\x00\x6d\x06\xab\x00\x00\x00\x00\x00\x05\x00\x93\x00\x00\x00\x00\x00\xd3\x0b\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\xa2\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x69\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe6\x00\x00\x00\x54\x04\x00\x00\x97\x02\xee\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x5e\x00\x00\x00\x00\x00\x00\x00\x6e\x02\x00\x00\xc6\x00\x4e\x02\x43\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x02\x00\x00\x5f\x05\xa8\x05\x82\x00\x00\x00\x15\x01\x00\x00\x5d\x00\x53\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb7\x02\x60\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa2\x01\x00\x00\x00\x00\x00\x00\x00\x00\x93\x05\x00\x00\x00\x00\x00\x00\x00\x00\xdb\x01\x00\x00\x00\x00\x00\x00\x00\x00\xaf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x04\x00\x00\xed\xff\x9d\x04\x20\x02\xdf\xff\x2c\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\xa5\x04\x00\x00\x00\x00\x41\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x82\x05\x00\x00\x00\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x97\x05\xff\x00\x00\x00\x00\x00\x2c\x00\xfa\x01\x00\x00\x00\x00\xef\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf8\xff\x45\xff\xee\xff\x12\xff\xeb\xff\x0b\xff\x1d\xff\x60\xff\x5e\xff\x5d\xff\x5b\xff\x59\xff\x50\xff\x4f\xff\x4e\xff\x4a\xff\x48\xff\x00\x00\x00\x00\x46\xff\x47\xff\x15\xff\xf7\xff\xf6\xff\xf5\xff\xf2\xff\xed\xff\xec\xff\xea\xff\x00\x00\x00\x00\x00\x00\x00\x00\x3f\xff\xf1\xff\xef\xff\xf0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\xff\xf4\xff\xf3\xff\x0f\xff\x5f\xff\x00\x00\x5d\xff\x00\x00\xe9\xff\xe5\xff\x84\xff\x80\xff\x00\x00\x90\xff\x88\xff\x85\xff\x00\x00\x00\x00\x86\xff\x00\x00\x9f\xff\x00\x00\xaf\xff\xa5\xff\x9c\xff\x00\x00\x00\x00\x00\x00\x47\xff\x00\x00\x00\x00\x00\x00\x93\xff\x92\xff\x91\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd7\xff\x00\x00\xd6\xff\xe6\xff\xc9\xff\x00\x00\x00\x00\xc7\xff\xca\xff\x00\x00\x00\x00\x00\x00\xa3\xff\x90\xff\x66\xff\x8e\xff\x6e\xff\x66\xff\x00\x00\xdf\xff\xe3\xff\x05\xff\xda\xff\xf8\xfe\x9b\xff\x66\xff\x64\xff\xfb\xfe\xfa\xfe\x00\x00\x01\xff\xe0\xff\xde\xff\xdb\xff\xd9\xff\xf9\xfe\x00\x00\x00\x00\xe2\xff\xe1\xff\x00\x00\x09\xff\x03\xff\x99\xff\xff\xfe\xfe\xfe\x97\xff\x00\x00\x00\x00\x00\x00\x00\x00\xae\xff\x00\x00\x9e\xff\x00\x00\x7e\xff\x8b\xff\x00\x00\x00\x00\x00\x00\x7f\xff\x7b\xff\x87\xff\x00\x00\x00\x00\x00\x00\x00\x00\x3e\xff\x00\x00\x3d\xff\x17\xff\x00\x00\xdd\xff\x07\xff\x35\xff\x5d\xff\x00\x00\x00\x00\xfd\xfe\xfc\xfe\x00\x00\x01\xff\xdc\xff\xe2\xff\x18\xff\x7b\xff\x00\x00\x51\xff\x4c\xff\x2d\xff\x2c\xff\x00\x00\x1b\xff\x00\x00\x00\x00\x00\x00\x00\x00\x31\xff\x4d\xff\x30\xff\x00\x00\x00\x00\x00\x00\x1c\xff\x00\x00\x1a\xff\x00\x00\x00\x00\x00\x00\x2b\xff\x00\x00\x00\x00\x00\x00\x00\x00\x7b\xff\x10\xff\x0a\xff\x00\x00\x5d\xff\x43\xff\x00\x00\x00\x00\x44\xff\x11\xff\x00\x00\x42\xff\x3c\xff\x27\xff\x00\x00\x61\xff\x5c\xff\x5a\xff\x8f\xff\x00\x00\x7d\xff\x83\xff\x00\x00\x81\xff\x82\xff\xa0\xff\x00\x00\xa1\xff\xad\xff\x14\xff\x94\xff\x00\x00\x9a\xff\x00\x00\x00\x00\x98\xff\x00\x00\x00\x00\x00\x00\x66\xff\x67\xff\x63\xff\x00\x00\x4b\xff\xa8\xff\xab\xff\x00\x00\x00\x00\x8d\xff\x00\x00\x8c\xff\xa7\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc6\xff\xb0\xff\xb1\xff\xb2\xff\x00\x00\x00\x00\xb3\xff\x00\x00\xd5\xff\xd3\xff\xe8\xff\x00\x00\x00\x00\xd2\xff\xcf\xff\x00\x00\x00\x00\x00\x00\xc8\xff\xc5\xff\xac\xff\xa2\xff\x0d\xff\x6e\xff\x00\x00\x00\x00\xa6\xff\xeb\xff\x77\xff\x6e\xff\x79\xff\x00\x00\x00\x00\xe4\xff\x6d\xff\x00\x00\x65\xff\x68\xff\x00\x00\x04\xff\x96\xff\x02\xff\x08\xff\x00\x00\x9d\xff\x89\xff\x7c\xff\x36\xff\x28\xff\x00\x00\x26\xff\x38\xff\x00\x00\x3a\xff\x37\xff\x41\xff\x33\xff\x40\xff\x7a\xff\x16\xff\x00\xff\x06\xff\x58\xff\x57\xff\x00\x00\x52\xff\x00\x00\x53\xff\x00\x00\x00\x00\x00\x00\x00\x00\x49\xff\x2f\xff\x32\xff\x2e\xff\x00\x00\x22\xff\x00\x00\x19\xff\x00\x00\x29\xff\x00\x00\x00\x00\x3b\xff\x13\xff\x62\xff\x6a\xff\x00\x00\x6c\xff\x00\x00\x71\xff\xaa\xff\x00\x00\x00\x00\x00\x00\x74\xff\x71\xff\xa9\xff\x00\x00\x00\x00\xb4\xff\xb5\xff\xd8\xff\xcc\xff\xd4\xff\xd0\xff\xba\xff\x00\x00\xb9\xff\xb7\xff\xb6\xff\x00\x00\x00\x00\xd1\xff\xc4\xff\xcb\xff\x00\x00\xc1\xff\x00\x00\x70\xff\x00\x00\x95\xff\x76\xff\x78\xff\x00\x00\x0c\xff\x6b\xff\x00\x00\x39\xff\x2a\xff\x25\xff\x56\xff\x54\xff\x00\x00\x55\xff\x66\xff\x00\x00\x00\x00\x66\xff\x00\x00\x23\xff\x00\x00\x21\xff\x20\xff\x69\xff\x00\x00\x75\xff\x73\xff\x00\x00\x00\x00\x00\x00\xc0\xff\xbd\xff\xc1\xff\xce\xff\xcd\xff\x00\x00\xb8\xff\x00\x00\xc3\xff\xbe\xff\xba\xff\x72\xff\x6f\xff\x00\x00\x00\x00\x24\xff\x1f\xff\x00\x00\x00\x00\x00\x00\xbf\xff\xc2\xff\xbc\xff\xbb\xff\x1e\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x01\x00\x11\x00\x03\x00\x04\x00\x05\x00\x03\x00\x0b\x00\x0c\x00\x09\x00\x0e\x00\x0f\x00\x10\x00\x01\x00\x36\x00\x0f\x00\x03\x00\x04\x00\x28\x00\x11\x00\x25\x00\x3b\x00\x11\x00\x0a\x00\x07\x00\x08\x00\x42\x00\x1c\x00\x2b\x00\x07\x00\x08\x00\x35\x00\x07\x00\x1f\x00\x33\x00\x3b\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x29\x00\x35\x00\x30\x00\x31\x00\x0b\x00\x0c\x00\x0d\x00\x3b\x00\x0f\x00\x10\x00\x35\x00\x3a\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x34\x00\x39\x00\x4b\x00\x4c\x00\x33\x00\x01\x00\x02\x00\x33\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x21\x00\x21\x00\x00\x00\x59\x00\x08\x00\x5b\x00\x01\x00\x5d\x00\x03\x00\x5f\x00\x2b\x00\x2b\x00\x62\x00\x5c\x00\x64\x00\x61\x00\x66\x00\x16\x00\x17\x00\x41\x00\x5a\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x20\x00\x21\x00\x46\x00\x47\x00\x48\x00\x1a\x00\x26\x00\x27\x00\x1e\x00\x29\x00\x2a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x59\x00\x28\x00\x5b\x00\x03\x00\x5d\x00\x59\x00\x09\x00\x5b\x00\x0b\x00\x5d\x00\x60\x00\x24\x00\x62\x00\x36\x00\x35\x00\x65\x00\x66\x00\x41\x00\x3b\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x4d\x00\x4e\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x0b\x00\x0c\x00\x0d\x00\x09\x00\x0f\x00\x10\x00\x28\x00\x5b\x00\x01\x00\x59\x00\x03\x00\x5b\x00\x2e\x00\x5d\x00\x31\x00\x32\x00\x03\x00\x04\x00\x56\x00\x35\x00\x5a\x00\x00\x00\x5c\x00\x0a\x00\x0f\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x38\x00\x4f\x00\x31\x00\x32\x00\x52\x00\x53\x00\x3a\x00\x19\x00\x3c\x00\x3d\x00\x3b\x00\x59\x00\x3d\x00\x5b\x00\x38\x00\x5d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x35\x00\x4f\x00\x36\x00\x1f\x00\x01\x00\x02\x00\x3b\x00\x3b\x00\x08\x00\x57\x00\x58\x00\x59\x00\x60\x00\x5b\x00\x62\x00\x5d\x00\x16\x00\x65\x00\x66\x00\x08\x00\x2b\x00\x01\x00\x2d\x00\x03\x00\x24\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x32\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x4f\x00\x01\x00\x02\x00\x52\x00\x03\x00\x04\x00\x3b\x00\x1d\x00\x1e\x00\x32\x00\x59\x00\x0a\x00\x5b\x00\x32\x00\x5d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x05\x00\x20\x00\x21\x00\x3b\x00\x26\x00\x00\x00\x4f\x00\x26\x00\x27\x00\x2b\x00\x29\x00\x2a\x00\x4d\x00\x38\x00\x57\x00\x29\x00\x59\x00\x00\x00\x5b\x00\x35\x00\x5d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x5b\x00\x3b\x00\x36\x00\x37\x00\x23\x00\x24\x00\x25\x00\x09\x00\x41\x00\x0b\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x5a\x00\x35\x00\x5c\x00\x08\x00\x33\x00\x00\x00\x24\x00\x3b\x00\x01\x00\x18\x00\x03\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x59\x00\x36\x00\x5b\x00\x24\x00\x5d\x00\x1f\x00\x3b\x00\x5b\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x36\x00\x24\x00\x31\x00\x32\x00\x34\x00\x3b\x00\x33\x00\x1d\x00\x1e\x00\x33\x00\x39\x00\x34\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x0b\x00\x0c\x00\x20\x00\x21\x00\x0f\x00\x36\x00\x0b\x00\x0c\x00\x26\x00\x27\x00\x3b\x00\x29\x00\x2a\x00\x21\x00\x59\x00\x08\x00\x5b\x00\x00\x00\x5d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x0b\x00\x0c\x00\x09\x00\x0a\x00\x0f\x00\x41\x00\x4f\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x5a\x00\x57\x00\x5c\x00\x59\x00\x07\x00\x5b\x00\x34\x00\x5d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x0b\x00\x0c\x00\x16\x00\x59\x00\x34\x00\x5b\x00\x13\x00\x5d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x35\x00\x35\x00\x36\x00\x31\x00\x32\x00\x12\x00\x13\x00\x3b\x00\x01\x00\x3a\x00\x03\x00\x04\x00\x05\x00\x35\x00\x5e\x00\x21\x00\x09\x00\x61\x00\x26\x00\x63\x00\x26\x00\x27\x00\x2c\x00\x29\x00\x2a\x00\x34\x00\x35\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x25\x00\x26\x00\x27\x00\x34\x00\x29\x00\x2a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x01\x00\x02\x00\x03\x00\x04\x00\x41\x00\x3a\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x30\x00\x31\x00\x34\x00\x35\x00\x35\x00\x41\x00\x3a\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x34\x00\x35\x00\x0b\x00\x59\x00\x11\x00\x5b\x00\x23\x00\x5d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x01\x00\x02\x00\x3a\x00\x59\x00\x08\x00\x5b\x00\x33\x00\x5d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x59\x00\x15\x00\x5b\x00\x21\x00\x5d\x00\x29\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x59\x00\x08\x00\x5b\x00\x34\x00\x5d\x00\x07\x00\x08\x00\x36\x00\x37\x00\x34\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x3a\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x0b\x00\x0c\x00\x0d\x00\x35\x00\x0f\x00\x10\x00\x59\x00\x21\x00\x5b\x00\x29\x00\x5d\x00\x39\x00\x5b\x00\x35\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x59\x00\x39\x00\x5b\x00\x39\x00\x5d\x00\x34\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x59\x00\x34\x00\x5b\x00\x35\x00\x5d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x34\x00\x01\x00\x39\x00\x03\x00\x34\x00\x35\x00\x06\x00\x34\x00\x35\x00\x3a\x00\x39\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x59\x00\x60\x00\x5b\x00\x62\x00\x5d\x00\x15\x00\x65\x00\x66\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x29\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x1e\x00\x16\x00\x1f\x00\x35\x00\x34\x00\x59\x00\x34\x00\x5b\x00\x34\x00\x5d\x00\x26\x00\x27\x00\x34\x00\x29\x00\x2a\x00\x05\x00\x59\x00\x07\x00\x5b\x00\x35\x00\x5d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x41\x00\x5a\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x23\x00\x24\x00\x25\x00\x59\x00\x34\x00\x5b\x00\x34\x00\x5d\x00\x05\x00\x06\x00\x07\x00\x08\x00\x35\x00\x38\x00\x16\x00\x35\x00\x59\x00\x35\x00\x5b\x00\x34\x00\x5d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x01\x00\x34\x00\x03\x00\x34\x00\x38\x00\x06\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x35\x00\x23\x00\x24\x00\x25\x00\x21\x00\x29\x00\x30\x00\x33\x00\x14\x00\x15\x00\x29\x00\x24\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x08\x00\x33\x00\x42\x00\x42\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x3a\x00\x42\x00\x3a\x00\x3a\x00\x2e\x00\x2c\x00\x42\x00\x59\x00\x3a\x00\x5b\x00\x01\x00\x5d\x00\x3a\x00\x3a\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\x5a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\xff\xff\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x46\x00\x47\x00\x48\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\x50\x00\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\xff\xff\xff\xff\xff\xff\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x41\x00\xff\xff\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x01\x00\x4f\x00\xff\xff\xff\xff\x01\x00\x02\x00\x54\x00\x55\x00\x09\x00\xff\xff\x0b\x00\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x41\x00\xff\xff\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x4f\x00\xff\xff\x59\x00\x29\x00\x5b\x00\x54\x00\x5d\x00\x31\x00\x32\x00\xff\xff\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\x39\x00\x36\x00\xff\xff\xff\xff\x41\x00\xff\xff\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x5b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\xff\xff\xff\xff\x46\x00\x47\x00\x48\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\x59\x00\xff\xff\x5b\x00\xff\xff\x5d\x00\xff\xff\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x46\x00\x47\x00\x48\x00\x09\x00\xff\xff\x46\x00\x47\x00\x48\x00\xff\xff\xff\xff\x50\x00\x51\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\x59\x00\x09\x00\x5b\x00\xff\xff\x5d\x00\x59\x00\x60\x00\x5b\x00\x62\x00\x5d\x00\xff\xff\x65\x00\x66\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x0f\x00\x10\x00\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\xff\xff\x33\x00\x34\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\x0b\x00\x0c\x00\xff\xff\x09\x00\x0f\x00\xff\xff\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x34\x00\x01\x00\x02\x00\x03\x00\x04\x00\x01\x00\x02\x00\x03\x00\x04\x00\x09\x00\x1b\x00\x1c\x00\x05\x00\x09\x00\x07\x00\x08\x00\x0b\x00\x0c\x00\x28\x00\xff\xff\x0f\x00\xff\xff\x01\x00\x5c\x00\x03\x00\x04\x00\x05\x00\x1b\x00\x1c\x00\xff\xff\x09\x00\xff\xff\x1c\x00\x05\x00\xff\xff\x07\x00\x08\x00\xff\xff\xff\xff\x0b\x00\x5c\x00\xff\xff\x23\x00\x24\x00\x25\x00\x28\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\xff\xff\xff\xff\xff\xff\xff\xff\x60\x00\xff\xff\x62\x00\xff\xff\x33\x00\x65\x00\x66\x00\xff\xff\x37\x00\x23\x00\x24\x00\x25\x00\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x40\x00\x5e\x00\x2d\x00\x5b\x00\x61\x00\x27\x00\x63\x00\xff\xff\x33\x00\x34\x00\x35\x00\x2d\x00\x37\x00\xff\xff\xff\xff\x31\x00\x32\x00\xff\xff\x05\x00\x06\x00\x5b\x00\x40\x00\xff\xff\x39\x00\x5b\x00\x05\x00\xff\xff\x07\x00\x5e\x00\xff\xff\xff\xff\x61\x00\xff\xff\x63\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x23\x00\x24\x00\x25\x00\x12\x00\xff\xff\x14\x00\xff\xff\x23\x00\x24\x00\x25\x00\x26\x00\xff\xff\x1b\x00\xff\xff\xff\xff\xff\xff\x33\x00\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\x33\x00\x27\x00\x3c\x00\xff\xff\x2a\x00\x3f\x00\x05\x00\xff\xff\x07\x00\x2f\x00\xff\xff\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\xff\xff\x37\x00\xff\xff\x39\x00\xff\xff\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xff\xff\x41\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x23\x00\x24\x00\x25\x00\x26\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\xff\xff\xff\xff\xff\xff\xff\xff\x33\x00\xff\xff\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\x05\x00\xff\xff\x07\x00\x2f\x00\xff\xff\x31\x00\x01\x00\x33\x00\x03\x00\x04\x00\x05\x00\x37\x00\x38\x00\x05\x00\x09\x00\x07\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xff\xff\x41\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x23\x00\x24\x00\x25\x00\xff\xff\x0d\x00\xff\xff\x1c\x00\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\xff\xff\x23\x00\x24\x00\x25\x00\x33\x00\xff\xff\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\x30\x00\x31\x00\x22\x00\x23\x00\x24\x00\x25\x00\x33\x00\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xff\xff\x41\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\x27\x00\xff\xff\x29\x00\xff\xff\x2b\x00\xff\xff\x1b\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\x39\x00\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xff\xff\x41\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\x27\x00\x28\x00\xff\xff\xff\xff\xff\xff\x2c\x00\x1b\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\x39\x00\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xff\xff\x41\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\x27\x00\x28\x00\xff\xff\xff\xff\xff\xff\xff\xff\x1b\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\x39\x00\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xff\xff\x41\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\x27\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1b\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\x39\x00\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x31\x00\x01\x00\x33\x00\x03\x00\x04\x00\x05\x00\x37\x00\xff\xff\xff\xff\x09\x00\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xff\xff\x41\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\x30\x00\x31\x00\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xff\xff\x41\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xff\xff\x41\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xff\xff\x41\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xff\xff\x41\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xff\xff\x41\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xff\xff\x41\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\xff\xff\x12\x00\xff\xff\xff\xff\x15\x00\xff\xff\x17\x00\x18\x00\x19\x00\x1a\x00\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\x35\x00\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\xff\xff\x12\x00\xff\xff\xff\xff\xff\xff\xff\xff\x17\x00\x18\x00\x19\x00\x1a\x00\xff\xff\xff\xff\x1d\x00\xff\xff\xff\xff\x20\x00\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x05\x00\xff\xff\x07\x00\x08\x00\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x23\x00\x24\x00\x25\x00\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\xff\xff\xff\xff\xff\xff\xff\xff\x33\x00\xff\xff\x1b\x00\xff\xff\x37\x00\x38\x00\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\x40\x00\xff\xff\xff\xff\xff\xff\x2a\x00\x05\x00\xff\xff\x07\x00\x08\x00\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\x34\x00\xff\xff\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\x23\x00\x24\x00\x25\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\x2d\x00\xff\xff\xff\xff\x17\x00\x18\x00\x19\x00\x33\x00\x34\x00\x35\x00\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\x40\x00\xff\xff\xff\xff\x05\x00\xff\xff\x07\x00\x08\x00\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x23\x00\x24\x00\x25\x00\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\xff\xff\xff\xff\xff\xff\xff\xff\x33\x00\xff\xff\x1b\x00\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\x40\x00\xff\xff\xff\xff\xff\xff\x2a\x00\x05\x00\xff\xff\x07\x00\x08\x00\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\x23\x00\x24\x00\x25\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\xff\xff\xff\xff\xff\xff\x17\x00\x18\x00\x19\x00\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\x40\x00\xff\xff\xff\xff\x05\x00\xff\xff\x07\x00\x08\x00\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x23\x00\x24\x00\x25\x00\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\x40\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\x36\x00\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\xff\xff\xff\xff\xff\xff\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x31\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\x22\x00\x23\x00\x24\x00\x25\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\xff\xff\xff\xff\xff\xff\x2d\x00\x05\x00\x2f\x00\x07\x00\x08\x00\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\x05\x00\xff\xff\x07\x00\x08\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x23\x00\x24\x00\x25\x00\xff\xff\xff\xff\x05\x00\x06\x00\x07\x00\x08\x00\xff\xff\x23\x00\x24\x00\x25\x00\xff\xff\xff\xff\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\x22\x00\x23\x00\x24\x00\x25\x00\x05\x00\x33\x00\x07\x00\x08\x00\x40\x00\x37\x00\x0b\x00\xff\xff\x3a\x00\x2f\x00\x23\x00\x24\x00\x25\x00\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\xff\xff\xff\xff\xff\xff\xff\xff\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x33\x00\xff\xff\xff\xff\xff\xff\x23\x00\x24\x00\x25\x00\xff\xff\x05\x00\x3c\x00\x07\x00\x08\x00\x3f\x00\x05\x00\xff\xff\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\x05\x00\x39\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x23\x00\x24\x00\x25\x00\xff\xff\xff\xff\x23\x00\x24\x00\x25\x00\xff\xff\xff\xff\x2d\x00\xff\xff\xff\xff\x30\x00\xff\xff\x2d\x00\x33\x00\x23\x00\x24\x00\x25\x00\x37\x00\x33\x00\xff\xff\xff\xff\x01\x00\x37\x00\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\x09\x00\x33\x00\xff\xff\xff\xff\xff\xff\x37\x00\x0f\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\x01\x00\x09\x00\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\x09\x00\xff\xff\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\x09\x00\xff\xff\x2b\x00\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x22\x00\x23\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x2b\x00\x22\x00\x23\x00\xff\xff\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x01\x00\x2b\x00\x03\x00\x04\x00\x05\x00\x2f\x00\x30\x00\x31\x00\x09\x00\xff\xff\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\x01\x00\x09\x00\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\x01\x00\x09\x00\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\x09\x00\x22\x00\x23\x00\xff\xff\xff\xff\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x2b\x00\x22\x00\x23\x00\x09\x00\x2f\x00\x30\x00\x31\x00\xff\xff\x22\x00\x23\x00\x2b\x00\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x2b\x00\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x2b\x00\xff\xff\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\xff\xff\x2b\x00\xff\xff\x09\x00\xff\xff\x2f\x00\x30\x00\x31\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\x01\x00\x09\x00\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\x01\x00\x09\x00\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\x09\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\xff\xff\x2b\x00\xff\xff\x09\x00\xff\xff\x2f\x00\x30\x00\x31\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\xff\xff\x2b\x00\xff\xff\x09\x00\xff\xff\x2f\x00\x30\x00\x31\x00\x2b\x00\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x2b\x00\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\xff\xff\x01\x00\x2b\x00\x03\x00\x04\x00\x05\x00\x2f\x00\x30\x00\x31\x00\x09\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x2b\x00\xff\xff\xff\xff\x09\x00\x2f\x00\x30\x00\x31\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2b\x00\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\xff\xff\xff\xff\x2b\x00\x27\x00\xff\xff\xff\xff\x2f\x00\x30\x00\x31\x00\x2d\x00\xff\xff\xff\xff\xff\xff\x31\x00\x32\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x3a\x00\xfc\x00\x0c\x00\x3b\x00\x3c\x00\x1b\x01\xa2\x00\xa3\x00\x3d\x00\xa4\x00\x71\x00\x72\x00\xea\x00\x8f\x00\x85\x00\x0c\x00\x26\x01\x9b\x01\x5a\x00\x7b\x01\xac\x01\xfc\x00\x5c\x01\x24\x00\x25\x00\xff\xff\x5d\x00\x63\x01\x24\x00\x25\x00\x8e\x00\x24\x00\x5b\x00\x7c\x01\x92\x01\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xfd\x00\x98\x01\x99\x00\x41\x00\x6e\x00\x6f\x00\x70\x00\x99\x01\x71\x00\x72\x00\x1c\x01\x5a\x00\xa5\x00\x0f\x00\x10\x00\xa6\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x5f\x01\x5d\x01\xa7\x00\xa8\x00\x29\x01\x0a\x00\x0b\x00\x1f\x01\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xf8\x00\xf8\x00\x8b\x01\x1b\x00\x78\x01\x1c\x00\xea\x00\x1d\x00\x1b\x01\xa9\x00\x94\x01\x81\x00\xaa\x00\x1d\x01\xab\x00\x63\x01\xac\x00\x5f\x00\x60\x00\xaa\x01\xeb\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x61\x00\x48\x00\xf8\x00\x17\x00\x18\x00\x79\x01\x49\x00\x4a\x00\xa3\x01\x4b\x00\x4c\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1b\x00\x8d\x00\x1c\x00\xef\x00\x1d\x00\x1b\x00\x7b\x00\x1c\x00\x7d\x00\x1d\x00\x76\x00\x56\x01\x77\x00\x8c\x01\x8e\x00\x78\x00\x79\x00\x4d\x00\x8d\x01\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xbe\x00\xbf\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x6e\x00\x6f\x00\x70\x00\x7b\x00\x71\x00\x72\x00\x95\xff\xc0\x00\xee\x00\x1b\x00\xef\x00\x4e\x00\x6e\x00\x1d\x00\x82\x00\x83\x00\x0c\x00\x26\x01\x8d\x01\x95\xff\x73\x01\x6b\x01\x74\x01\x96\x01\x5f\x01\x35\x01\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x61\x01\x36\x01\x82\x00\x83\x00\x37\x01\x38\x01\x73\x00\x68\x01\x74\x00\x75\x00\xf5\x00\x1b\x00\xf6\x00\x1c\x00\x67\x01\x1d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x98\x01\x19\x00\x8f\x00\x6c\x01\x0a\x00\x0b\x00\x9a\x01\x6c\x01\x6d\x01\xc4\x00\xc5\x00\x1b\x00\x76\x00\x1c\x00\x77\x00\x1d\x00\x17\x01\x78\x00\x79\x00\x18\x01\x8f\x01\xea\x00\x90\x01\x1b\x01\x29\x01\x35\x01\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x3f\x01\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x36\x01\x0a\x00\x0b\x00\x88\x01\x0c\x00\x26\x01\x2a\x01\xaf\x01\x72\x01\xcc\x00\x1b\x00\x27\x01\x1c\x00\xe0\x00\x1d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x22\x00\x16\x01\x48\x00\xf9\x00\xda\x00\x47\x01\x19\x00\x49\x00\x4a\x00\xdb\x00\x4b\x00\x4c\x00\x4e\x01\xfa\x00\x54\x01\x7c\x01\x1b\x00\xe7\x00\x1c\x00\xdc\x00\x1d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xc0\x00\x00\x01\x7d\x01\x7e\x01\x2b\x00\x2c\x00\x2d\x00\x7b\x00\x4d\x00\x7d\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x73\x01\x4d\x01\x74\x01\x5d\x00\xed\x00\x07\x01\x9b\x00\x4e\x01\xea\x00\x04\x01\x1b\x01\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1b\x00\x48\x01\x4e\x00\xb7\x00\x1d\x00\x58\x00\x49\x01\x7f\x01\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xe8\x00\x45\x00\x82\x00\x83\x00\xb4\x01\xe9\x00\xa8\x01\x71\x01\x72\x01\xa0\x01\x8c\x00\xa1\x01\x9e\x00\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x9f\x00\xa0\x00\x84\x00\x6f\x00\x47\x00\x48\x00\x85\x00\x8f\x00\x30\x01\x6f\x00\x49\x00\x4a\x00\x08\x01\x4b\x00\x4c\x00\xf8\x00\x1b\x00\x5f\x00\x1c\x00\x09\x01\x1d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x30\x01\x6f\x00\x7b\x00\x4b\x01\x5f\x01\x4d\x00\x19\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x73\x01\x1a\x00\x74\x01\x1b\x00\x24\x00\x1c\x00\x84\x01\x1d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xa2\x00\x6f\x00\xc8\x00\x1b\x00\xcf\x00\x4e\x00\x56\x01\x1d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x34\xff\x0a\x01\x0b\x01\x82\x00\x83\x00\x0c\x01\x0d\x01\x0c\x01\x3a\x00\x47\x00\x0c\x00\x3b\x00\x3c\x00\x58\x01\x87\x00\xe9\x00\x3d\x00\x88\x00\x5a\x01\x2d\x01\x49\x00\x4a\x00\x59\x01\x4b\x00\x4c\x00\xb5\x01\xa3\x01\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x8f\x00\x90\x00\x4a\x00\x5b\x01\x4b\x00\x4c\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0a\x00\xca\x00\x0c\x00\xcb\x00\x4d\x00\x61\x01\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x99\x00\x41\x00\xb3\x01\xa7\x01\x8a\xff\x4d\x00\x0d\xff\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xa6\x01\xa7\x01\x7d\x00\x1b\x00\xfc\x00\x4e\x00\x6a\x01\x1d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0a\x00\x0b\x00\x5a\x00\x1b\x00\x5f\x00\x4e\x00\x71\x01\x1d\x00\xb5\x01\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xad\x01\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1b\x00\x63\x00\x1c\x00\x16\x01\x1d\x00\x7c\x01\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1b\x00\x5f\x00\x1c\x00\x20\x01\x1d\x00\x24\x00\x25\x00\x7d\x01\x82\x01\xa2\x01\xa3\x01\x90\x01\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x47\x00\x0a\x00\xf2\x00\x0c\x00\xcb\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x6e\x00\x6f\x00\x70\x00\x8b\xff\x71\x00\x72\x00\x1b\x00\xf8\x00\x1c\x00\x2c\x01\x1d\x00\x2d\x01\x7f\x01\xf2\x00\x86\x01\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x87\x01\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1b\x00\x2f\x01\x1c\x00\x30\x01\x1d\x00\x34\x01\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1b\x00\x3f\x01\x1c\x00\xce\x00\x1d\x00\x89\x01\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x41\x01\xea\x00\x42\x01\x0f\x01\x85\x01\x86\x01\x10\x01\x6f\x01\x70\x01\x4a\x01\x43\x01\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1b\x00\x76\x00\x1c\x00\x77\x00\x1d\x00\x77\x01\x9c\x00\x79\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x4c\x01\x4f\x01\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x5b\x01\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xc4\x00\xc8\x00\xc7\x00\xce\x00\xcf\x00\x1b\x00\xd0\x00\x1c\x00\xd3\x00\x1d\x00\x31\x01\x4a\x00\xd6\x00\x4b\x00\x4c\x00\x22\x00\x1b\x00\x24\x00\x1c\x00\xd4\x00\x1d\x00\x34\x01\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x4d\x00\x13\x01\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x2b\x00\x2c\x00\x2d\x00\x1b\x00\xe3\xff\x1c\x00\xd7\x00\x1d\x00\x22\x00\x23\x00\x24\x00\x25\x00\xd8\x00\xd9\x00\xc8\x00\xce\x00\x1b\x00\xe4\x00\x4e\x00\xe2\x00\x1d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xea\x00\xe3\x00\x0f\x01\xe5\x00\xe6\x00\x10\x01\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xf2\x00\x2b\x00\x2c\x00\x2d\x00\xf8\x00\x02\x01\x03\x01\x0f\x01\x11\x01\x12\x01\x04\x01\x06\x01\x3a\x01\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x5f\x00\x6b\x00\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x47\x00\xff\xff\x47\x00\xba\x00\x6e\x00\xbc\x00\xff\xff\x1b\x00\xbd\x00\x1c\x00\x09\x00\x1d\x00\x47\x00\x5a\x00\x3b\x01\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x3d\x01\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x13\x01\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x43\x01\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x44\x01\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x45\x01\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\xc2\x00\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xf3\x00\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf4\x00\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\xb8\x00\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xba\x00\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x37\x00\x0f\x00\x10\x00\x38\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xb4\x00\x17\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\xc8\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x01\x00\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x8b\x00\x51\x01\x00\x00\x00\x00\x0a\x00\x0b\x00\x52\x01\x53\x01\x7b\x00\x00\x00\x7d\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\xd0\x00\x10\x00\xd1\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x50\x01\x00\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x51\x01\x00\x00\x1b\x00\x7c\x01\x1c\x00\x94\x01\x1d\x00\x82\x00\x83\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x8c\x00\xa9\x01\x00\x00\x00\x00\x91\x01\x00\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\x00\xde\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x6e\x00\x6f\x00\x70\x00\x00\x00\x71\x00\x72\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xde\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xb2\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x00\x00\xb3\x00\x17\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x3a\x00\x00\x00\x20\x01\x3b\x00\x3c\x00\xb4\x00\x17\x00\x18\x00\x3d\x00\x00\x00\xbd\x00\x17\x00\x18\x00\x00\x00\x00\x00\xb5\x00\xb6\x00\x3a\x00\x00\x00\x20\x01\x3b\x00\x3c\x00\x00\x00\x00\x00\x1b\x00\x3d\x00\x1c\x00\x00\x00\x1d\x00\x1b\x00\x76\x00\x1c\x00\x77\x00\x1d\x00\x00\x00\xd4\x00\x79\x00\x6e\x00\x6f\x00\x70\x00\x00\x00\x71\x00\x72\x00\x00\x00\x00\x00\x00\x00\x21\x01\x40\x00\x41\x00\x00\x00\x22\x01\x23\x01\x0a\x00\x0b\x00\x0c\x00\x3b\x00\x00\x00\x84\x00\x6f\x00\x00\x00\x9b\x01\x85\x00\x00\x00\x21\x01\x40\x00\x41\x00\x00\x00\x00\x00\x81\x01\x0a\x00\x0b\x00\x0c\x00\x3b\x00\x0a\x00\x0b\x00\x0c\x00\x3b\x00\x9b\x01\xa4\x01\x9d\x01\x22\x00\x9b\x01\x24\x00\x25\x00\x84\x00\x6f\x00\xf0\x00\x00\x00\x85\x00\x00\x00\x3a\x00\x24\x01\x0c\x00\x3b\x00\x3c\x00\x9c\x01\x9d\x01\x00\x00\x3d\x00\x00\x00\xb1\x01\x22\x00\x00\x00\x24\x00\x25\x00\x00\x00\x00\x00\x7d\x00\x24\x01\x00\x00\x2b\x00\x2c\x00\x2d\x00\x86\x00\x7b\x00\x7c\x00\x7d\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x76\x00\x00\x00\x77\x00\x00\x00\x43\x00\x9c\x00\x79\x00\x00\x00\x44\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x80\x01\x40\x00\x41\x00\x45\x00\x87\x00\x97\x00\x9e\x01\x88\x00\x7f\x00\x89\x00\x00\x00\x43\x00\x98\x00\x99\x00\xaf\x01\x44\x00\x00\x00\x00\x00\x82\x00\x83\x00\x00\x00\x22\x00\x23\x00\x9e\x01\x45\x00\x00\x00\x84\x00\x9e\x01\x22\x00\x00\x00\x24\x00\x87\x00\x00\x00\x00\x00\x88\x00\x00\x00\x89\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x7b\x00\xae\x00\x7d\x00\x7e\x00\x26\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\x27\x00\x00\x00\x28\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\xb1\x01\x00\x00\x3a\x00\x00\x00\x00\x00\x00\x00\xc2\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x77\x01\x7f\x00\x33\x00\x00\x00\x2e\x00\x36\x00\x22\x00\x00\x00\x24\x00\x2f\x00\x00\x00\xaf\x00\x83\x00\x31\x00\xb0\x00\xb1\x00\x00\x00\x32\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x37\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x2b\x00\x2c\x00\x2d\x00\x76\x01\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x77\x01\x00\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x22\x00\x00\x00\x24\x00\x2f\x00\x00\x00\x30\x00\x3a\x00\x31\x00\x0c\x00\x3b\x00\x3c\x00\x32\x00\xa2\x00\x22\x00\x3d\x00\x24\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x37\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x26\x00\x00\x00\x15\x01\x00\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\x77\x01\x00\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x65\x01\x41\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\xed\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x7b\x00\x7c\x00\x7d\x00\x7e\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x37\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x28\x00\x7f\x00\x00\x00\x80\x00\x00\x00\x81\x00\x00\x00\x3a\x01\x00\x00\x00\x00\x00\x00\x82\x00\x83\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x84\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x7b\x00\x7c\x00\x7d\x00\x7e\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x37\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x28\x00\x7f\x00\x9e\x00\x00\x00\x00\x00\x00\x00\x2e\xff\x3a\x00\x00\x00\x00\x00\x00\x00\x82\x00\x83\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x84\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x7b\x00\x7c\x00\x7d\x00\x7e\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x37\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x28\x00\x7f\x00\x9e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\x82\x00\x83\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x84\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x7b\x00\x7c\x00\x7d\x00\x7e\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x37\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x28\x00\x7f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x82\x00\x83\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x84\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x3a\x00\x31\x00\x0c\x00\x3b\x00\x3c\x00\x32\x00\x00\x00\x00\x00\x3d\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x37\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x99\x00\x41\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x37\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x37\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x37\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x37\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x37\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x37\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x50\x00\x51\x00\x52\x00\x00\x00\x27\x00\x00\x00\x00\x00\x63\x00\x00\x00\x53\x00\x54\x00\x55\x00\x56\x00\x00\x00\x00\x00\x57\x00\x00\x00\x00\x00\x58\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x64\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x50\x00\x51\x00\x52\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x53\x00\x54\x00\x55\x00\x56\x00\x00\x00\x00\x00\x57\x00\x00\x00\x00\x00\x58\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x00\x00\x00\x24\x00\x25\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x43\x00\x00\x00\x3a\x00\x00\x00\x44\x00\x93\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x45\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x22\x00\x00\x00\x24\x00\x25\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x3d\x01\x00\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x97\x00\x00\x00\x00\x00\x53\x00\x54\x00\x55\x00\x43\x00\x98\x00\x99\x00\x00\x00\x44\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x45\x00\x00\x00\x00\x00\x22\x00\x00\x00\x24\x00\x25\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x01\x00\x00\x3a\x00\x00\x00\x44\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x45\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x22\x00\x00\x00\x24\x00\x25\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x53\x00\x54\x00\x55\x00\x43\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x45\x00\x00\x00\x00\x00\x22\x00\x00\x00\x24\x00\x25\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x01\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x96\x01\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x33\x00\x34\x00\x35\x00\x36\x00\x00\x00\x00\x00\x00\x00\xca\x00\x22\x00\x2f\x00\x24\x00\x25\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x22\x00\x00\x00\x24\x00\x25\x00\x33\x00\x34\x00\x35\x00\x36\x00\x09\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x44\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x22\x00\x43\x00\x24\x00\x25\x00\x45\x00\x44\x00\x7d\x00\x00\x00\x67\x01\x2f\x00\x2b\x00\x2c\x00\x2d\x00\x31\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x34\x00\x35\x00\x36\x00\xc2\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x22\x00\x33\x00\x24\x00\x25\x00\x36\x00\x22\x00\x00\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x44\x00\x22\x00\x65\x01\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\x00\x00\x00\x00\x9b\x00\x00\x00\x00\x00\xa4\xff\x00\x00\x9b\x00\x43\x00\x2b\x00\x2c\x00\x2d\x00\x44\x00\x43\x00\x00\x00\x00\x00\x3a\x00\x44\x00\x0c\x00\x3b\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x43\x00\x00\x00\x00\x00\x00\x00\x44\x00\x5f\x01\x3a\x00\x00\x00\x0c\x00\x3b\x00\x3c\x00\x00\x00\x00\x00\x3a\x00\x3d\x00\x0c\x00\x3b\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x00\x00\x3a\x00\x00\x00\x0c\x00\x3b\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x00\x00\x93\x00\x00\x00\x94\x00\x95\x00\x3f\x00\x40\x00\x41\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x65\x00\xed\x00\xfd\x00\xfe\x00\x94\x00\xff\x00\x3f\x00\x40\x00\x41\x00\x67\x00\x65\x00\x66\x00\x00\x00\x68\x00\x40\x00\x41\x00\x00\x00\x00\x00\x3a\x00\x67\x00\x0c\x00\x3b\x00\x3c\x00\x68\x00\x40\x00\x41\x00\x3d\x00\x00\x00\x3a\x00\x00\x00\x0c\x00\x3b\x00\x3c\x00\x00\x00\x00\x00\x3a\x00\x3d\x00\x0c\x00\x3b\x00\x3c\x00\x00\x00\x00\x00\x3a\x00\x3d\x00\x0c\x00\x3b\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x65\x00\x69\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x0c\x00\x3b\x00\x3c\x00\x67\x00\x65\x00\x6b\x00\x3d\x00\x68\x00\x40\x00\x41\x00\x00\x00\x65\x00\x6c\x00\x67\x00\x00\x00\x00\x00\x00\x00\x68\x00\x40\x00\x41\x00\x67\x00\x00\x00\x00\x00\x00\x00\x68\x00\x40\x00\x41\x00\x93\x00\x00\x00\x94\x00\x95\x00\x3f\x00\x40\x00\x41\x00\x3a\x00\x00\x00\x0c\x00\x3b\x00\x3c\x00\x00\x00\xa8\x01\x00\x00\x3d\x00\x00\x00\x3f\x00\x40\x00\x41\x00\x3a\x00\x00\x00\x0c\x00\x3b\x00\x3c\x00\x00\x00\x00\x00\x3a\x00\x3d\x00\x0c\x00\x3b\x00\x3c\x00\x00\x00\x00\x00\x3a\x00\x3d\x00\x0c\x00\x3b\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x3a\x00\x00\x00\x0c\x00\x3b\x00\x3c\x00\x00\x00\x19\x01\x00\x00\x3d\x00\x00\x00\x3f\x00\x40\x00\x41\x00\x3a\x00\x00\x00\x0c\x00\x3b\x00\x3c\x00\x00\x00\x1a\x01\x00\x00\x3d\x00\x00\x00\x3f\x00\x40\x00\x41\x00\x32\x01\x00\x00\x00\x00\x00\x00\x3f\x00\x40\x00\x41\x00\xdc\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x40\x00\x41\x00\x00\x00\x3a\x00\xdf\x00\x0c\x00\x3b\x00\x3c\x00\x3f\x00\x40\x00\x41\x00\x3d\x00\x3a\x00\x00\x00\x0c\x00\x3b\x00\x3c\x00\x64\x00\x00\x00\x00\x00\x3d\x00\x3f\x00\x40\x00\x41\x00\x00\x00\x7b\x00\x7c\x00\x7d\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x91\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x40\x00\x41\x00\x00\x00\x00\x00\x3e\x00\x7f\x00\x00\x00\x00\x00\x3f\x00\x40\x00\x41\x00\xac\x01\x00\x00\x00\x00\x00\x00\x82\x00\x83\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (7, 263) [
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136),
	(137 , happyReduce_137),
	(138 , happyReduce_138),
	(139 , happyReduce_139),
	(140 , happyReduce_140),
	(141 , happyReduce_141),
	(142 , happyReduce_142),
	(143 , happyReduce_143),
	(144 , happyReduce_144),
	(145 , happyReduce_145),
	(146 , happyReduce_146),
	(147 , happyReduce_147),
	(148 , happyReduce_148),
	(149 , happyReduce_149),
	(150 , happyReduce_150),
	(151 , happyReduce_151),
	(152 , happyReduce_152),
	(153 , happyReduce_153),
	(154 , happyReduce_154),
	(155 , happyReduce_155),
	(156 , happyReduce_156),
	(157 , happyReduce_157),
	(158 , happyReduce_158),
	(159 , happyReduce_159),
	(160 , happyReduce_160),
	(161 , happyReduce_161),
	(162 , happyReduce_162),
	(163 , happyReduce_163),
	(164 , happyReduce_164),
	(165 , happyReduce_165),
	(166 , happyReduce_166),
	(167 , happyReduce_167),
	(168 , happyReduce_168),
	(169 , happyReduce_169),
	(170 , happyReduce_170),
	(171 , happyReduce_171),
	(172 , happyReduce_172),
	(173 , happyReduce_173),
	(174 , happyReduce_174),
	(175 , happyReduce_175),
	(176 , happyReduce_176),
	(177 , happyReduce_177),
	(178 , happyReduce_178),
	(179 , happyReduce_179),
	(180 , happyReduce_180),
	(181 , happyReduce_181),
	(182 , happyReduce_182),
	(183 , happyReduce_183),
	(184 , happyReduce_184),
	(185 , happyReduce_185),
	(186 , happyReduce_186),
	(187 , happyReduce_187),
	(188 , happyReduce_188),
	(189 , happyReduce_189),
	(190 , happyReduce_190),
	(191 , happyReduce_191),
	(192 , happyReduce_192),
	(193 , happyReduce_193),
	(194 , happyReduce_194),
	(195 , happyReduce_195),
	(196 , happyReduce_196),
	(197 , happyReduce_197),
	(198 , happyReduce_198),
	(199 , happyReduce_199),
	(200 , happyReduce_200),
	(201 , happyReduce_201),
	(202 , happyReduce_202),
	(203 , happyReduce_203),
	(204 , happyReduce_204),
	(205 , happyReduce_205),
	(206 , happyReduce_206),
	(207 , happyReduce_207),
	(208 , happyReduce_208),
	(209 , happyReduce_209),
	(210 , happyReduce_210),
	(211 , happyReduce_211),
	(212 , happyReduce_212),
	(213 , happyReduce_213),
	(214 , happyReduce_214),
	(215 , happyReduce_215),
	(216 , happyReduce_216),
	(217 , happyReduce_217),
	(218 , happyReduce_218),
	(219 , happyReduce_219),
	(220 , happyReduce_220),
	(221 , happyReduce_221),
	(222 , happyReduce_222),
	(223 , happyReduce_223),
	(224 , happyReduce_224),
	(225 , happyReduce_225),
	(226 , happyReduce_226),
	(227 , happyReduce_227),
	(228 , happyReduce_228),
	(229 , happyReduce_229),
	(230 , happyReduce_230),
	(231 , happyReduce_231),
	(232 , happyReduce_232),
	(233 , happyReduce_233),
	(234 , happyReduce_234),
	(235 , happyReduce_235),
	(236 , happyReduce_236),
	(237 , happyReduce_237),
	(238 , happyReduce_238),
	(239 , happyReduce_239),
	(240 , happyReduce_240),
	(241 , happyReduce_241),
	(242 , happyReduce_242),
	(243 , happyReduce_243),
	(244 , happyReduce_244),
	(245 , happyReduce_245),
	(246 , happyReduce_246),
	(247 , happyReduce_247),
	(248 , happyReduce_248),
	(249 , happyReduce_249),
	(250 , happyReduce_250),
	(251 , happyReduce_251),
	(252 , happyReduce_252),
	(253 , happyReduce_253),
	(254 , happyReduce_254),
	(255 , happyReduce_255),
	(256 , happyReduce_256),
	(257 , happyReduce_257),
	(258 , happyReduce_258),
	(259 , happyReduce_259),
	(260 , happyReduce_260),
	(261 , happyReduce_261),
	(262 , happyReduce_262),
	(263 , happyReduce_263)
	]

happy_n_terms = 67 :: Int
happy_n_nonterms = 103 :: Int

happyReduce_7 = happySpecReduce_1  0# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (locate (getLoc happy_var_1) $ LitExp $ IntegerLit (getINTEGER happy_var_1)
	)}

happyReduce_8 = happySpecReduce_1  0# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (locate (getLoc happy_var_1) $ LitExp $ FloatLit (getFLOAT happy_var_1)
	)}

happyReduce_9 = happySpecReduce_1  0# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (locate (getLoc happy_var_1) $ LitExp $ CharLit (getCHAR happy_var_1)
	)}

happyReduce_10 = happySpecReduce_1  0# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (locate (getLoc happy_var_1) $ LitExp $ StringLit (getSTRING happy_var_1)
	)}

happyReduce_11 = happySpecReduce_1  0# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (locate (getLoc happy_var_1) $ LitExp $ AntiInt (getANTI_INT happy_var_1)
	)}

happyReduce_12 = happySpecReduce_1  0# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (locate (getLoc happy_var_1) $ LitExp $ AntiFloat (getANTI_FLOAT happy_var_1)
	)}

happyReduce_13 = happySpecReduce_1  1# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (locname (getLoc happy_var_1) (getVARID happy_var_1)
	)}

happyReduce_14 = happySpecReduce_1  1# happyReduction_14
happyReduction_14 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (locname (getLoc happy_var_1) "as"
	)}

happyReduce_15 = happySpecReduce_1  1# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (locname (getLoc happy_var_1) "hiding"
	)}

happyReduce_16 = happySpecReduce_1  1# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (locname (getLoc happy_var_1) "qualified"
	)}

happyReduce_17 = happySpecReduce_1  2# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (happy_var_1
	)}

happyReduce_18 = happySpecReduce_1  2# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (locqname (getLoc happy_var_1) (getQVARID happy_var_1)
	)}

happyReduce_19 = happySpecReduce_1  3# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (locname (getLoc happy_var_1) (getCONID happy_var_1)
	)}

happyReduce_20 = happySpecReduce_1  4# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 (happy_var_1
	)}

happyReduce_21 = happySpecReduce_1  4# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn14
		 (locqname (getLoc happy_var_1) (getQCONID happy_var_1)
	)}

happyReduce_22 = happySpecReduce_1  5# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (happy_var_1
	)}

happyReduce_23 = happySpecReduce_1  6# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (happy_var_1
	)}

happyReduce_24 = happySpecReduce_1  7# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (happy_var_1
	)}

happyReduce_25 = happySpecReduce_1  8# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn18
		 (locname (getLoc happy_var_1) (getCONID happy_var_1)
	)}

happyReduce_26 = happySpecReduce_1  9# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (happy_var_1
	)}

happyReduce_27 = happySpecReduce_1  10# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (happy_var_1
	)}

happyReduce_28 = happySpecReduce_1  11# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (happy_var_1
	)}

happyReduce_29 = happySpecReduce_1  11# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (locname (getLoc happy_var_1) "-"
	)}

happyReduce_30 = happySpecReduce_1  11# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn21
		 (locname (getLoc happy_var_1) "!"
	)}

happyReduce_31 = happySpecReduce_1  12# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (locname (getLoc happy_var_1) (getVARSYM happy_var_1)
	)}

happyReduce_32 = happySpecReduce_1  13# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (happy_var_1
	)}

happyReduce_33 = happySpecReduce_1  13# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (locqname (getLoc happy_var_1) (getQVARSYM happy_var_1)
	)}

happyReduce_34 = happySpecReduce_1  14# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (happy_var_1
	)}

happyReduce_35 = happySpecReduce_1  14# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (locqname (getLoc happy_var_1) (getQVARSYM happy_var_1)
	)}

happyReduce_36 = happySpecReduce_1  15# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (locname (getLoc happy_var_1) (getCONSYM happy_var_1)
	)}

happyReduce_37 = happySpecReduce_1  16# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (happy_var_1
	)}

happyReduce_38 = happySpecReduce_1  16# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (locqname (getLoc happy_var_1) (getQCONSYM happy_var_1)
	)}

happyReduce_39 = happyReduce 5# 17# happyReduction_39
happyReduction_39 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_2 of { happy_var_2 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	case happyOut41 happy_x_5 of { happy_var_5 -> 
	happyIn27
		 (Module (unLoc happy_var_2) happy_var_3 (unLoc happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_40 = happySpecReduce_1  17# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (Module (name (getLoc happy_var_1) "Main") [] (unLoc happy_var_1)
	)}

happyReduce_41 = happySpecReduce_0  18# happyReduction_41
happyReduction_41  =  happyIn28
		 ([]
	)

happyReduce_42 = happySpecReduce_1  18# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (happy_var_1
	)}

happyReduce_43 = happySpecReduce_3  19# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn29
		 (reverse $ map unLoc happy_var_2
	)}

happyReduce_44 = happySpecReduce_0  20# happyReduction_44
happyReduction_44  =  happyIn30
		 ([]
	)

happyReduce_45 = happySpecReduce_1  20# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ([happy_var_1]
	)}

happyReduce_46 = happySpecReduce_3  20# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn30
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_47 = happySpecReduce_2  20# happyReduction_47
happyReduction_47 happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (happy_var_1
	)}

happyReduce_48 = happySpecReduce_1  21# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut100 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (L (getLoc happy_var_1) $ VarExport (unLoc happy_var_1)
	)}

happyReduce_49 = happyReduce 4# 21# happyReduction_49
happyReduction_49 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn31
		 (let loc = getLoc happy_var_1 <--> getLoc happy_var_4
          in
            L loc $ TyConExport (TyCon $ unLoc happy_var_1) Nothing
	) `HappyStk` happyRest}}

happyReduce_50 = happyReduce 4# 21# happyReduction_50
happyReduction_50 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn31
		 (let loc = getLoc happy_var_1 <--> getLoc happy_var_4
          in
            L loc $
            TyConExport (TyCon $ unLoc happy_var_1) (Just (reverse $ map unLoc happy_var_3))
	) `HappyStk` happyRest}}}

happyReduce_51 = happySpecReduce_2  21# happyReduction_51
happyReduction_51 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (L (getLoc happy_var_1) $ ModuleExport (unLoc happy_var_2)
	)}}

happyReduce_52 = happyReduce 5# 22# happyReduction_52
happyReduction_52 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_2 of { happy_var_2 -> 
	case happyOut18 happy_x_3 of { happy_var_3 -> 
	case happyOut35 happy_x_4 of { happy_var_4 -> 
	case happyOut36 happy_x_5 of { happy_var_5 -> 
	happyIn32
		 (let loc = combineLocs (getLoc happy_var_1 : map getLoc (snd happy_var_5))
          in
            locate loc $
            ImportDecl happy_var_2 (unLoc happy_var_3) happy_var_4 (fst happy_var_5) (map unLoc (snd happy_var_5))
	) `HappyStk` happyRest}}}}}

happyReduce_53 = happySpecReduce_1  23# happyReduction_53
happyReduction_53 happy_x_1
	 =  happyIn33
		 ([]
	)

happyReduce_54 = happySpecReduce_1  23# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 ([happy_var_1]
	)}

happyReduce_55 = happySpecReduce_3  23# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_56 = happySpecReduce_0  24# happyReduction_56
happyReduction_56  =  happyIn34
		 (False
	)

happyReduce_57 = happySpecReduce_1  24# happyReduction_57
happyReduction_57 happy_x_1
	 =  happyIn34
		 (True
	)

happyReduce_58 = happySpecReduce_0  25# happyReduction_58
happyReduction_58  =  happyIn35
		 (Nothing
	)

happyReduce_59 = happySpecReduce_2  25# happyReduction_59
happyReduction_59 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn35
		 (Just (unLoc happy_var_2)
	)}

happyReduce_60 = happySpecReduce_3  26# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_2 of { happy_var_2 -> 
	happyIn36
		 ((False, reverse happy_var_2)
	)}

happyReduce_61 = happyReduce 4# 26# happyReduction_61
happyReduction_61 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 ((True, reverse happy_var_3)
	) `HappyStk` happyRest}

happyReduce_62 = happySpecReduce_0  27# happyReduction_62
happyReduction_62  =  happyIn37
		 ([]
	)

happyReduce_63 = happySpecReduce_1  27# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 ([happy_var_1]
	)}

happyReduce_64 = happySpecReduce_3  27# happyReduction_64
happyReduction_64 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_65 = happySpecReduce_2  27# happyReduction_65
happyReduction_65 happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (happy_var_1
	)}

happyReduce_66 = happySpecReduce_1  28# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut101 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (locate (getLoc happy_var_1) $ VarImport (unLoc happy_var_1)
	)}

happyReduce_67 = happyReduce 4# 28# happyReduction_67
happyReduction_67 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn38
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
         in
           locate loc $ TyConImport (TyCon $ unLoc happy_var_1) Nothing
	) `HappyStk` happyRest}}

happyReduce_68 = happyReduce 4# 28# happyReduction_68
happyReduction_68 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut39 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn38
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
         in
           locate loc $
           TyConImport (TyCon $ unLoc happy_var_1) (Just (reverse $ map unLoc happy_var_3))
	) `HappyStk` happyRest}}}

happyReduce_69 = happySpecReduce_0  29# happyReduction_69
happyReduction_69  =  happyIn39
		 ([]
	)

happyReduce_70 = happySpecReduce_1  29# happyReduction_70
happyReduction_70 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 ([happy_var_1]
	)}

happyReduce_71 = happySpecReduce_3  29# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	case happyOut40 happy_x_3 of { happy_var_3 -> 
	happyIn39
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_72 = happySpecReduce_1  30# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOut100 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (L (getLoc happy_var_1) $ VarCName (unLoc happy_var_1)
	)}

happyReduce_73 = happySpecReduce_1  30# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOut102 happy_x_1 of { happy_var_1 -> 
	happyIn40
		 (L (getLoc happy_var_1) $ ConCName (unLoc happy_var_1)
	)}

happyReduce_74 = happyReduce 5# 31# happyReduction_74
happyReduction_74 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	case happyOut42 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn41
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_5]
          in
            L loc $
            Body (reverse $ map unLoc happy_var_2) (reverse $ map unLoc happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_75 = happyMonadReduce 5# 31# happyReduction_75
happyReduction_75 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	case happyOut42 happy_x_4 of { happy_var_4 -> 
	(  do  {  lookForRightBrace
               ;  let loc = combineLocs (getLoc happy_var_1 : map getLoc happy_var_4)
               ;  return $ L loc $
                  Body (reverse $ map unLoc happy_var_2) (reverse $ map unLoc happy_var_4)
               })}}}
	) (\r -> happyReturn (happyIn41 r))

happyReduce_76 = happySpecReduce_3  31# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn41
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
          in
            L loc $
            Body (reverse $ map unLoc happy_var_2) []
	)}}}

happyReduce_77 = happyMonadReduce 3# 31# happyReduction_77
happyReduction_77 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut33 happy_x_2 of { happy_var_2 -> 
	(  do  {  lookForRightBrace
               ;  let loc = combineLocs (getLoc happy_var_1 : map getLoc happy_var_2)
               ;  return $ L loc $
                  Body (reverse $ map unLoc happy_var_2) []
               })}}
	) (\r -> happyReturn (happyIn41 r))

happyReduce_78 = happySpecReduce_3  31# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn41
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
          in
            L loc $
            Body [] (reverse $ map unLoc happy_var_2)
	)}}}

happyReduce_79 = happyMonadReduce 3# 31# happyReduction_79
happyReduction_79 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut42 happy_x_2 of { happy_var_2 -> 
	(  do  {  lookForRightBrace
               ;  let loc = combineLocs (getLoc happy_var_1 : map getLoc happy_var_2)
               ;  return $ L loc $
                  Body [] (reverse $ map unLoc happy_var_2)
               })}}
	) (\r -> happyReturn (happyIn41 r))

happyReduce_80 = happySpecReduce_1  32# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 ([happy_var_1]
	)}

happyReduce_81 = happySpecReduce_2  32# happyReduction_81
happyReduction_81 happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (happy_var_1
	)}

happyReduce_82 = happySpecReduce_3  32# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn42
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_83 = happyMonadReduce 4# 33# happyReduction_83
happyReduction_83 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_2 of { happy_var_2 -> 
	case happyOut53 happy_x_4 of { happy_var_4 -> 
	( do  {  let loc          =   combineLocs [getLoc happy_var_1, getLoc happy_var_4]
              ;  (tycon, tyvars)  <-  checkData (unLoc happy_var_2)
              ;  return $ locate loc $
                 TypeDecl tycon tyvars (unLoc happy_var_4)
              })}}}
	) (\r -> happyReturn (happyIn43 r))

happyReduce_84 = happyMonadReduce 3# 33# happyReduction_84
happyReduction_84 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_2 of { happy_var_2 -> 
	case happyOut66 happy_x_3 of { happy_var_3 -> 
	( do  {  let loc          =   combineLocs (getLoc happy_var_1 : map getLoc happy_var_3)
              ;  let (ctx, ty)    =   unLoc happy_var_2
              ;  (tycon, tyvars)  <-  checkData ty
              ;  let derives      =   map unLoc happy_var_3
              ;  return $ locate loc $
                 DataDecl DataType ctx tycon tyvars [] derives
              })}}}
	) (\r -> happyReturn (happyIn43 r))

happyReduce_85 = happyMonadReduce 5# 33# happyReduction_85
happyReduction_85 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_2 of { happy_var_2 -> 
	case happyOut61 happy_x_4 of { happy_var_4 -> 
	case happyOut66 happy_x_5 of { happy_var_5 -> 
	( do  {  let loc          =   combineLocs (getLoc happy_var_1 : map getLoc happy_var_5)
              ;  let (ctx, ty)    =   unLoc happy_var_2
              ;  (tycon, tyvars)  <-  checkData ty
              ;  let constrs      =   reverse $ map unLoc happy_var_4
              ;  let derives      =   map unLoc happy_var_5
              ;  return $ locate loc $
                 DataDecl DataType ctx tycon tyvars constrs derives
              })}}}}
	) (\r -> happyReturn (happyIn43 r))

happyReduce_86 = happyMonadReduce 5# 33# happyReduction_86
happyReduction_86 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_2 of { happy_var_2 -> 
	case happyOut63 happy_x_4 of { happy_var_4 -> 
	case happyOut66 happy_x_5 of { happy_var_5 -> 
	( do  {  let loc          =   combineLocs (getLoc happy_var_1 : map getLoc happy_var_5)
              ;  let (ctx, ty)    =   unLoc happy_var_2
              ;  (tycon, tyvars)  <-  checkData ty
              ;  let derives      =   map unLoc happy_var_5
              ;  return $ locate loc $
                 DataDecl NewType ctx tycon tyvars [unLoc happy_var_4] derives
              })}}}}
	) (\r -> happyReturn (happyIn43 r))

happyReduce_87 = happyMonadReduce 3# 33# happyReduction_87
happyReduction_87 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_2 of { happy_var_2 -> 
	case happyOut69 happy_x_3 of { happy_var_3 -> 
	( do  {  let loc          =   combineLocs (getLoc happy_var_1 : map getLoc happy_var_3)
              ;  let (ctx, ty)    =   unLoc happy_var_2
              ;  (tycls, tyvars)  <-  checkClass ty
              ;  let decls        =   map unLoc happy_var_3
              ;  return $ locate loc $
                 ClassDecl ctx tycls tyvars decls
              })}}}
	) (\r -> happyReturn (happyIn43 r))

happyReduce_88 = happyMonadReduce 3# 33# happyReduction_88
happyReduction_88 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_2 of { happy_var_2 -> 
	case happyOut69 happy_x_3 of { happy_var_3 -> 
	( do  {  let loc        =   combineLocs (getLoc happy_var_1 : map getLoc happy_var_3)
              ;  let (ctx, ty)  =   unLoc happy_var_2
              ;  (tycls, tys)   <-  checkInstance ty
              ;  let decls      =   map unLoc happy_var_3
              ;  return $ locate loc $
                 InstDecl ctx tycls tys decls
              })}}}
	) (\r -> happyReturn (happyIn43 r))

happyReduce_89 = happyReduce 4# 33# happyReduction_89
happyReduction_89 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn43
		 (let  {  loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
              }
         in
           locate loc $ DefaultDecl (map unLoc happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_90 = happySpecReduce_1  33# happyReduction_90
happyReduction_90 happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (happy_var_1
	)}

happyReduce_91 = happyMonadReduce 1# 34# happyReduction_91
happyReduction_91 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut57 happy_x_1 of { happy_var_1 -> 
	( do  {  let loc = getLoc happy_var_1
             ;  ctx <- checkContext (unLoc happy_var_1)
             ;  return $ L loc ctx
             })}
	) (\r -> happyReturn (happyIn44 r))

happyReduce_92 = happySpecReduce_1  35# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (L (getLoc happy_var_1) ([], unLoc happy_var_1)
	)}

happyReduce_93 = happyMonadReduce 3# 35# happyReduction_93
happyReduction_93 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	( do  {  let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
              ;  return $ L loc (unLoc happy_var_1, unLoc happy_var_3)
              })}}
	) (\r -> happyReturn (happyIn45 r))

happyReduce_94 = happySpecReduce_3  36# happyReduction_94
happyReduction_94 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_2 of { happy_var_2 -> 
	happyIn46
		 (reverse happy_var_2
	)}

happyReduce_95 = happyMonadReduce 3# 36# happyReduction_95
happyReduction_95 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut47 happy_x_2 of { happy_var_2 -> 
	( do  {  lookForRightBrace
              ;  return $ reverse happy_var_2
              })}
	) (\r -> happyReturn (happyIn46 r))

happyReduce_96 = happySpecReduce_0  37# happyReduction_96
happyReduction_96  =  happyIn47
		 ([]
	)

happyReduce_97 = happySpecReduce_1  37# happyReduction_97
happyReduction_97 happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 ([happy_var_1]
	)}

happyReduce_98 = happySpecReduce_3  37# happyReduction_98
happyReduction_98 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	case happyOut48 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_99 = happySpecReduce_1  38# happyReduction_99
happyReduction_99 happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn48
		 (happy_var_1
	)}

happyReduce_100 = happySpecReduce_2  38# happyReduction_100
happyReduction_100 happy_x_2
	happy_x_1
	 =  case happyOut75 happy_x_1 of { happy_var_1 -> 
	case happyOut68 happy_x_2 of { happy_var_2 -> 
	happyIn48
		 (locate (combineLocs [getLoc happy_var_1, getLoc happy_var_2]) $
          ExpBindDecl (unLoc happy_var_1) (unLoc happy_var_2)
	)}}

happyReduce_101 = happyMonadReduce 3# 39# happyReduction_101
happyReduction_101 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	( do  {  let loc         =  combineLocs (getLoc happy_var_3 : map getLoc happy_var_1)
             ;  let vars        =  reverse $ map unLoc happy_var_1
             ;  let (preds, ty)  =  unLoc happy_var_3
             ;  return $ locate loc $
                    SigDecl vars (ForAll ImplicitForAll [] preds ty)
             })}}
	) (\r -> happyReturn (happyIn49 r))

happyReduce_102 = happyMonadReduce 2# 39# happyReduction_102
happyReduction_102 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_2 of { happy_var_2 -> 
	( do  {  let loc = combineLocs $ getLoc happy_var_1 : map getLoc happy_var_2
             ;  let fixity = unLoc happy_var_1
             ;  let prec = fromInteger 9
             ;  let ops = map unLoc happy_var_2
             ;  return $ locate loc $ FixityDecl ops fixity prec
             })}}
	) (\r -> happyReturn (happyIn49 r))

happyReduce_103 = happyMonadReduce 3# 39# happyReduction_103
happyReduction_103 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	( do  {  let loc     = combineLocs (getLoc happy_var_1 : map getLoc happy_var_3)
             ;  let fixity  = unLoc happy_var_1
             ;  let prec    = fromInteger $ getINTEGER happy_var_2
             ;  let ops     = map unLoc happy_var_3
             ;  return $ locate loc $ FixityDecl ops fixity prec
             })}}}
	) (\r -> happyReturn (happyIn49 r))

happyReduce_104 = happySpecReduce_1  40# happyReduction_104
happyReduction_104 happy_x_1
	 =  case happyOut109 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 ([happy_var_1]
	)}

happyReduce_105 = happySpecReduce_3  40# happyReduction_105
happyReduction_105 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut109 happy_x_3 of { happy_var_3 -> 
	happyIn50
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_106 = happySpecReduce_1  41# happyReduction_106
happyReduction_106 happy_x_1
	 =  case happyOut101 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 ([happy_var_1]
	)}

happyReduce_107 = happySpecReduce_3  41# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut100 happy_x_3 of { happy_var_3 -> 
	happyIn51
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_108 = happySpecReduce_1  42# happyReduction_108
happyReduction_108 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (L (getLoc happy_var_1) Infix
	)}

happyReduce_109 = happySpecReduce_1  42# happyReduction_109
happyReduction_109 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (L (getLoc happy_var_1) Infixl
	)}

happyReduce_110 = happySpecReduce_1  42# happyReduction_110
happyReduction_110 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (L (getLoc happy_var_1) Infixr
	)}

happyReduce_111 = happySpecReduce_1  43# happyReduction_111
happyReduction_111 happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 (happy_var_1
	)}

happyReduce_112 = happySpecReduce_3  43# happyReduction_112
happyReduction_112 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn53
		 (let loc = getLoc happy_var_1 <--> getLoc happy_var_3
        in
          L loc $
          AppTy  (AppTy  (TyConTy (TyCon builtinArrow))
                         (unLoc happy_var_1))
                 (unLoc happy_var_3)
	)}}

happyReduce_113 = happySpecReduce_0  44# happyReduction_113
happyReduction_113  =  happyIn54
		 ([]
	)

happyReduce_114 = happySpecReduce_1  44# happyReduction_114
happyReduction_114 happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	happyIn54
		 ([happy_var_1]
	)}

happyReduce_115 = happySpecReduce_1  44# happyReduction_115
happyReduction_115 happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	happyIn54
		 (reverse happy_var_1
	)}

happyReduce_116 = happySpecReduce_1  45# happyReduction_116
happyReduction_116 happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	happyIn55
		 ([happy_var_1]
	)}

happyReduce_117 = happySpecReduce_3  45# happyReduction_117
happyReduction_117 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn55
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_118 = happySpecReduce_3  46# happyReduction_118
happyReduction_118 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn56
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_119 = happySpecReduce_1  47# happyReduction_119
happyReduction_119 happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (happy_var_1
	)}

happyReduce_120 = happySpecReduce_2  47# happyReduction_120
happyReduction_120 happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn57
		 (let loc = getLoc happy_var_1 <--> getLoc happy_var_2
        in
          L loc $ AppTy (unLoc happy_var_1) (unLoc happy_var_2)
	)}}

happyReduce_121 = happySpecReduce_1  47# happyReduction_121
happyReduction_121 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (L (getLoc happy_var_1) $ AntiType (getANTI_TYPE happy_var_1)
	)}

happyReduce_122 = happySpecReduce_1  48# happyReduction_122
happyReduction_122 happy_x_1
	 =  case happyOut59 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 (L (getLoc happy_var_1) $ TyConTy (unLoc happy_var_1)
	)}

happyReduce_123 = happySpecReduce_1  48# happyReduction_123
happyReduction_123 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 (L (getLoc happy_var_1) $ TyVarTy (TyVar (unLoc happy_var_1))
	)}

happyReduce_124 = happySpecReduce_3  48# happyReduction_124
happyReduction_124 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn58
		 (let  {  loc  = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
              ;  tys  = reverse (map unLoc happy_var_2)
              }
         in
           L loc $
           foldl AppTy (TyConTy (TupleTyCon (length tys))) tys
	)}}}

happyReduce_125 = happySpecReduce_3  48# happyReduction_125
happyReduction_125 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn58
		 (let loc = getLoc happy_var_1 <--> getLoc happy_var_3
         in
           L loc $
           AppTy (TyConTy (TyCon builtinNil)) (unLoc happy_var_2)
	)}}}

happyReduce_126 = happySpecReduce_3  48# happyReduction_126
happyReduction_126 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_2 of { happy_var_2 -> 
	happyIn58
		 (happy_var_2
	)}

happyReduce_127 = happySpecReduce_1  49# happyReduction_127
happyReduction_127 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn59
		 (L (getLoc happy_var_1) $
         TyCon (unLoc happy_var_1)
	)}

happyReduce_128 = happySpecReduce_2  49# happyReduction_128
happyReduction_128 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn59
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
        in
          L loc $
          TupleTyCon 0
	)}}

happyReduce_129 = happySpecReduce_2  49# happyReduction_129
happyReduction_129 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn59
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
        in
          L loc $
          TyCon builtinNil
	)}}

happyReduce_130 = happySpecReduce_3  49# happyReduction_130
happyReduction_130 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn59
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
        in
          L loc $
          TyCon (name loc "->")
	)}}

happyReduce_131 = happyReduce 4# 49# happyReduction_131
happyReduction_131 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut60 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn59
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
        in
          L loc $
          TupleTyCon (2 + happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_132 = happySpecReduce_0  50# happyReduction_132
happyReduction_132  =  happyIn60
		 (0
	)

happyReduce_133 = happySpecReduce_2  50# happyReduction_133
happyReduction_133 happy_x_2
	happy_x_1
	 =  case happyOut60 happy_x_2 of { happy_var_2 -> 
	happyIn60
		 (1 + happy_var_2
	)}

happyReduce_134 = happySpecReduce_1  51# happyReduction_134
happyReduction_134 happy_x_1
	 =  case happyOut62 happy_x_1 of { happy_var_1 -> 
	happyIn61
		 ([happy_var_1]
	)}

happyReduce_135 = happySpecReduce_3  51# happyReduction_135
happyReduction_135 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut61 happy_x_1 of { happy_var_1 -> 
	case happyOut62 happy_x_3 of { happy_var_3 -> 
	happyIn61
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_136 = happyMonadReduce 1# 52# happyReduction_136
happyReduction_136 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut57 happy_x_1 of { happy_var_1 -> 
	( do  {  let loc = getLoc happy_var_1
              ;  let (ty : tys) = unfoldAppTy (unLoc happy_var_1)
              ;  case ty : tys of
                   {  TyConTy (TyCon tycon) : tys ->
                          return $ locate loc $
                          ConDecl (Con tycon) defaultFixity tys
                   ;  _ ->
                       throwException BadPat
                   }
              })}
	) (\r -> happyReturn (happyIn62 r))

happyReduce_137 = happySpecReduce_3  52# happyReduction_137
happyReduction_137 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_1 of { happy_var_1 -> 
	case happyOut107 happy_x_2 of { happy_var_2 -> 
	case happyOut57 happy_x_3 of { happy_var_3 -> 
	happyIn62
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
         in
           locate loc $
           OpConDecl (unLoc happy_var_1) (unLoc happy_var_2) defaultFixity (unLoc happy_var_3)
	)}}}

happyReduce_138 = happyReduce 4# 52# happyReduction_138
happyReduction_138 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut102 happy_x_1 of { happy_var_1 -> 
	case happyOut65 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn62
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
         in
           locate loc $
           RecConDecl (unLoc happy_var_1) defaultFixity (reverse happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_139 = happySpecReduce_2  53# happyReduction_139
happyReduction_139 happy_x_2
	happy_x_1
	 =  case happyOut102 happy_x_1 of { happy_var_1 -> 
	case happyOut58 happy_x_2 of { happy_var_2 -> 
	happyIn63
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
         in
           locate loc $
           ConDecl (unLoc happy_var_1) defaultFixity [unLoc happy_var_2]
	)}}

happyReduce_140 = happyReduce 4# 53# happyReduction_140
happyReduction_140 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut102 happy_x_1 of { happy_var_1 -> 
	case happyOut65 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn63
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
         in
           locate loc $
           RecConDecl (unLoc happy_var_1) defaultFixity (reverse happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_141 = happySpecReduce_3  54# happyReduction_141
happyReduction_141 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn64
		 (let  { vars  = map unLoc happy_var_1
              ; ty    = unLoc happy_var_3
              }
         in
           map (\v -> (v, ty)) vars
	)}}

happyReduce_142 = happySpecReduce_0  55# happyReduction_142
happyReduction_142  =  happyIn65
		 ([]
	)

happyReduce_143 = happySpecReduce_1  55# happyReduction_143
happyReduction_143 happy_x_1
	 =  case happyOut64 happy_x_1 of { happy_var_1 -> 
	happyIn65
		 (happy_var_1
	)}

happyReduce_144 = happySpecReduce_3  55# happyReduction_144
happyReduction_144 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut65 happy_x_1 of { happy_var_1 -> 
	case happyOut64 happy_x_3 of { happy_var_3 -> 
	happyIn65
		 (happy_var_3 ++ happy_var_1
	)}}

happyReduce_145 = happySpecReduce_0  56# happyReduction_145
happyReduction_145  =  happyIn66
		 ([]
	)

happyReduce_146 = happySpecReduce_2  56# happyReduction_146
happyReduction_146 happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_2 of { happy_var_2 -> 
	happyIn66
		 ([L (getLoc happy_var_2) $ TyCon (unLoc happy_var_2)]
	)}

happyReduce_147 = happySpecReduce_3  56# happyReduction_147
happyReduction_147 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn66
		 ([]
	)

happyReduce_148 = happyReduce 4# 56# happyReduction_148
happyReduction_148 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut67 happy_x_3 of { happy_var_3 -> 
	happyIn66
		 (reverse happy_var_3
	) `HappyStk` happyRest}

happyReduce_149 = happySpecReduce_1  57# happyReduction_149
happyReduction_149 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn67
		 ([L (getLoc happy_var_1) $ TyCon (unLoc happy_var_1)]
	)}

happyReduce_150 = happySpecReduce_3  57# happyReduction_150
happyReduction_150 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut67 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_3 of { happy_var_3 -> 
	happyIn67
		 ((L (getLoc happy_var_3) $ TyCon (unLoc happy_var_3)) : happy_var_1
	)}}

happyReduce_151 = happySpecReduce_3  58# happyReduction_151
happyReduction_151 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_2 of { happy_var_2 -> 
	case happyOut69 happy_x_3 of { happy_var_3 -> 
	happyIn68
		 (let loc = combineLocs $ getLoc happy_var_1 : getLoc happy_var_2 : map getLoc happy_var_3
        in
          L loc $
          Rhs [(Nothing, unLoc happy_var_2)] (map unLoc happy_var_3)
	)}}}

happyReduce_152 = happySpecReduce_2  58# happyReduction_152
happyReduction_152 happy_x_2
	happy_x_1
	 =  case happyOut70 happy_x_1 of { happy_var_1 -> 
	case happyOut69 happy_x_2 of { happy_var_2 -> 
	happyIn68
		 (let loc = combineLocs $ map getLoc happy_var_1 ++ map getLoc happy_var_2
        in
          L loc $
          Rhs (map unLoc $ reverse happy_var_1) (map unLoc happy_var_2)
	)}}

happyReduce_153 = happySpecReduce_0  59# happyReduction_153
happyReduction_153  =  happyIn69
		 ([]
	)

happyReduce_154 = happySpecReduce_2  59# happyReduction_154
happyReduction_154 happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_2 of { happy_var_2 -> 
	happyIn69
		 (happy_var_2
	)}

happyReduce_155 = happySpecReduce_1  60# happyReduction_155
happyReduction_155 happy_x_1
	 =  case happyOut71 happy_x_1 of { happy_var_1 -> 
	happyIn70
		 ([happy_var_1]
	)}

happyReduce_156 = happySpecReduce_2  60# happyReduction_156
happyReduction_156 happy_x_2
	happy_x_1
	 =  case happyOut70 happy_x_1 of { happy_var_1 -> 
	case happyOut71 happy_x_2 of { happy_var_2 -> 
	happyIn70
		 (happy_var_2 : happy_var_1
	)}}

happyReduce_157 = happyMonadReduce 4# 61# happyReduction_157
happyReduction_157 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_2 of { happy_var_2 -> 
	case happyOut72 happy_x_4 of { happy_var_4 -> 
	( do  {  let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
             ;  return $ L loc (Just (unLoc happy_var_2), unLoc happy_var_4)
             })}}}
	) (\r -> happyReturn (happyIn71 r))

happyReduce_158 = happySpecReduce_3  62# happyReduction_158
happyReduction_158 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut75 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn72
		 (locate (combineLocs [getLoc happy_var_1, getLoc happy_var_3]) $
          SigExp (unLoc happy_var_1) (unLoc happy_var_3)
	)}}

happyReduce_159 = happySpecReduce_1  62# happyReduction_159
happyReduction_159 happy_x_1
	 =  case happyOut73 happy_x_1 of { happy_var_1 -> 
	happyIn72
		 (happy_var_1
	)}

happyReduce_160 = happySpecReduce_1  62# happyReduction_160
happyReduction_160 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn72
		 (locate (getLoc happy_var_1) $ AntiExp (getANTI_EXP happy_var_1)
	)}

happyReduce_161 = happySpecReduce_1  63# happyReduction_161
happyReduction_161 happy_x_1
	 =  case happyOut74 happy_x_1 of { happy_var_1 -> 
	happyIn73
		 (happy_var_1
	)}

happyReduce_162 = happySpecReduce_1  63# happyReduction_162
happyReduction_162 happy_x_1
	 =  case happyOut75 happy_x_1 of { happy_var_1 -> 
	happyIn73
		 (happy_var_1
	)}

happyReduce_163 = happyMonadReduce 3# 64# happyReduction_163
happyReduction_163 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut75 happy_x_1 of { happy_var_1 -> 
	case happyOut111 happy_x_2 of { happy_var_2 -> 
	case happyOut76 happy_x_3 of { happy_var_3 -> 
	( do  {  let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
              ;  return $ L loc $
                 opappE (unLoc happy_var_1) (unLoc happy_var_2) defaultFixity (unLoc happy_var_3)
              })}}}
	) (\r -> happyReturn (happyIn74 r))

happyReduce_164 = happySpecReduce_1  64# happyReduction_164
happyReduction_164 happy_x_1
	 =  case happyOut76 happy_x_1 of { happy_var_1 -> 
	happyIn74
		 (happy_var_1
	)}

happyReduce_165 = happyMonadReduce 3# 65# happyReduction_165
happyReduction_165 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut75 happy_x_1 of { happy_var_1 -> 
	case happyOut111 happy_x_2 of { happy_var_2 -> 
	case happyOut77 happy_x_3 of { happy_var_3 -> 
	( do  {  let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
              ;  return $ L loc $
                 opappE (unLoc happy_var_1) (unLoc happy_var_2) defaultFixity (unLoc happy_var_3)
              })}}}
	) (\r -> happyReturn (happyIn75 r))

happyReduce_166 = happySpecReduce_1  65# happyReduction_166
happyReduction_166 happy_x_1
	 =  case happyOut77 happy_x_1 of { happy_var_1 -> 
	happyIn75
		 (happy_var_1
	)}

happyReduce_167 = happyReduce 4# 66# happyReduction_167
happyReduction_167 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut91 happy_x_2 of { happy_var_2 -> 
	case happyOut72 happy_x_4 of { happy_var_4 -> 
	happyIn76
		 (let  {  loc  = getLoc happy_var_1 <--> getLoc happy_var_4
              ;  ps   = reverse (map unLoc happy_var_2)
              }
         in
         L loc $
         foldr lamE (unLoc happy_var_4) ps
	) `HappyStk` happyRest}}}

happyReduce_168 = happyReduce 4# 66# happyReduction_168
happyReduction_168 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut46 happy_x_2 of { happy_var_2 -> 
	case happyOut72 happy_x_4 of { happy_var_4 -> 
	happyIn76
		 (let loc = getLoc happy_var_1 <--> getLoc happy_var_4
        in
          locate loc $
          LetExp (map unLoc happy_var_2) (unLoc happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_169 = happyReduce 6# 66# happyReduction_169
happyReduction_169 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_2 of { happy_var_2 -> 
	case happyOut72 happy_x_4 of { happy_var_4 -> 
	case happyOut72 happy_x_6 of { happy_var_6 -> 
	happyIn76
		 (let loc = getLoc happy_var_1 <--> getLoc happy_var_6
        in
         locate loc $
         IfExp (unLoc happy_var_2) (unLoc happy_var_4) (unLoc happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_170 = happyReduce 6# 67# happyReduction_170
happyReduction_170 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_2 of { happy_var_2 -> 
	case happyOut95 happy_x_5 of { happy_var_5 -> 
	case happyOutTok happy_x_6 of { happy_var_6 -> 
	happyIn77
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_6]
        in
          locate loc $
          CaseExp (unLoc happy_var_2) (reverse $ map unLoc happy_var_5)
	) `HappyStk` happyRest}}}}

happyReduce_171 = happyMonadReduce 6# 67# happyReduction_171
happyReduction_171 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_2 of { happy_var_2 -> 
	case happyOut95 happy_x_5 of { happy_var_5 -> 
	case happyOutTok happy_x_6 of { happy_var_6 -> 
	( do  {  lookForRightBrace
             ;  let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_6]
             ;  return $ locate loc $
                CaseExp (unLoc happy_var_2) (reverse $ map unLoc happy_var_5)
             })}}}}
	) (\r -> happyReturn (happyIn77 r))

happyReduce_172 = happyMonadReduce 4# 67# happyReduction_172
happyReduction_172 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut98 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	( do  {  lookForRightBrace
             ;  let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
             ;  return $ locate loc $
                DoExp (reverse $ map unLoc happy_var_3)
             })}}}
	) (\r -> happyReturn (happyIn77 r))

happyReduce_173 = happyMonadReduce 4# 67# happyReduction_173
happyReduction_173 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut98 happy_x_3 of { happy_var_3 -> 
	( do  {  lookForRightBrace
             ;  let loc = combineLocs (getLoc happy_var_1 : map getLoc happy_var_3)
             ;  return $ locate loc $
                DoExp (reverse $ map unLoc happy_var_3)
             })}}
	) (\r -> happyReturn (happyIn77 r))

happyReduce_174 = happySpecReduce_2  67# happyReduction_174
happyReduction_174 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut78 happy_x_2 of { happy_var_2 -> 
	happyIn77
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
        in
          locate loc $
          NegAppExp (unLoc happy_var_2)
	)}}

happyReduce_175 = happySpecReduce_1  67# happyReduction_175
happyReduction_175 happy_x_1
	 =  case happyOut78 happy_x_1 of { happy_var_1 -> 
	happyIn77
		 (happy_var_1
	)}

happyReduce_176 = happySpecReduce_1  68# happyReduction_176
happyReduction_176 happy_x_1
	 =  case happyOut79 happy_x_1 of { happy_var_1 -> 
	happyIn78
		 (case happy_var_1 of
           {  [e]  ->  e
           ;  _    ->  let loc = combineLocs (map getLoc happy_var_1)
                       in
                         L loc $
                         foldl1' appE (reverse $ map unLoc happy_var_1)
           }
	)}

happyReduce_177 = happySpecReduce_1  69# happyReduction_177
happyReduction_177 happy_x_1
	 =  case happyOut80 happy_x_1 of { happy_var_1 -> 
	happyIn79
		 ([happy_var_1]
	)}

happyReduce_178 = happySpecReduce_2  69# happyReduction_178
happyReduction_178 happy_x_2
	happy_x_1
	 =  case happyOut79 happy_x_1 of { happy_var_1 -> 
	case happyOut80 happy_x_2 of { happy_var_2 -> 
	happyIn79
		 (happy_var_2 : happy_var_1
	)}}

happyReduce_179 = happySpecReduce_2  70# happyReduction_179
happyReduction_179 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut80 happy_x_2 of { happy_var_2 -> 
	happyIn80
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
          in
            locate loc $ IrrefutPatExp (unLoc happy_var_2)
	)}}

happyReduce_180 = happySpecReduce_3  70# happyReduction_180
happyReduction_180 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut101 happy_x_1 of { happy_var_1 -> 
	case happyOut80 happy_x_3 of { happy_var_3 -> 
	happyIn80
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
          in
            locate loc $ AsPatExp (unLoc happy_var_1) (unLoc happy_var_3)
	)}}

happyReduce_181 = happySpecReduce_1  70# happyReduction_181
happyReduction_181 happy_x_1
	 =  case happyOut81 happy_x_1 of { happy_var_1 -> 
	happyIn80
		 (happy_var_1
	)}

happyReduce_182 = happyReduce 4# 71# happyReduction_182
happyReduction_182 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut81 happy_x_1 of { happy_var_1 -> 
	case happyOut88 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn81
		 (let  {  loc     = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
               ;  exp     = unLoc happy_var_1
               ;  fields  = reverse happy_var_3
               }
          in
            locate loc $
            case destructConExp exp of
              {  Just con  -> RecConExp con fields
              ;  Nothing   -> RecUpdateExp exp fields
              }
	) `HappyStk` happyRest}}}

happyReduce_183 = happySpecReduce_1  71# happyReduction_183
happyReduction_183 happy_x_1
	 =  case happyOut82 happy_x_1 of { happy_var_1 -> 
	happyIn81
		 (happy_var_1
	)}

happyReduce_184 = happySpecReduce_1  72# happyReduction_184
happyReduction_184 happy_x_1
	 =  case happyOut101 happy_x_1 of { happy_var_1 -> 
	happyIn82
		 (locate (getLoc happy_var_1) $ VarExp (unLoc happy_var_1)
	)}

happyReduce_185 = happySpecReduce_1  72# happyReduction_185
happyReduction_185 happy_x_1
	 =  case happyOut99 happy_x_1 of { happy_var_1 -> 
	happyIn82
		 (locate (getLoc happy_var_1) $ ConExp (unLoc happy_var_1)
	)}

happyReduce_186 = happySpecReduce_1  72# happyReduction_186
happyReduction_186 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn82
		 (happy_var_1
	)}

happyReduce_187 = happySpecReduce_3  72# happyReduction_187
happyReduction_187 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn82
		 (let loc = getLoc happy_var_1 <--> getLoc happy_var_3
          in
            locate loc $ ParExp (unLoc happy_var_2)
	)}}}

happyReduce_188 = happySpecReduce_3  72# happyReduction_188
happyReduction_188 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut86 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn82
		 (let  {  loc   = getLoc happy_var_1 <--> getLoc happy_var_3
               ;  exps  = reverse (map unLoc happy_var_2)
               ;  conExp  = ConExp (TupleCon (length exps)) (fromLoc loc)
               }
          in
            L loc $
            foldl appE conExp exps
	)}}}

happyReduce_189 = happySpecReduce_3  72# happyReduction_189
happyReduction_189 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut83 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn82
		 (let  { loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3] }
          in
            L loc (unLoc happy_var_2)
	)}}}

happyReduce_190 = happyReduce 4# 72# happyReduction_190
happyReduction_190 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut75 happy_x_2 of { happy_var_2 -> 
	case happyOut111 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn82
		 (locate (combineLocs [getLoc happy_var_1, getLoc happy_var_4]) $
          LSection (unLoc happy_var_2) (unLoc happy_var_3)
	) `HappyStk` happyRest}}}}

happyReduce_191 = happyReduce 4# 72# happyReduction_191
happyReduction_191 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut110 happy_x_2 of { happy_var_2 -> 
	case happyOut73 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn82
		 (locate (combineLocs [getLoc happy_var_1, getLoc happy_var_4]) $
          RSection (unLoc happy_var_2) (unLoc happy_var_3)
	) `HappyStk` happyRest}}}}

happyReduce_192 = happySpecReduce_1  72# happyReduction_192
happyReduction_192 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn82
		 (locate (getLoc happy_var_1) WildPatExp
	)}

happyReduce_193 = happySpecReduce_1  73# happyReduction_193
happyReduction_193 happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	happyIn83
		 (let  {  loc     = getLoc happy_var_1
               ;  exps    = [unLoc happy_var_1]
               ;  nil     = ConExp (Con builtinNil) (fromLoc loc)
               ;  cons    = ConExp (Con builtinCons) (fromLoc loc)
               }
          in
            L loc $
            foldr (\hd tl -> opappE hd cons consFixity tl) nil exps
	)}

happyReduce_194 = happySpecReduce_1  73# happyReduction_194
happyReduction_194 happy_x_1
	 =  case happyOut84 happy_x_1 of { happy_var_1 -> 
	happyIn83
		 (let  {  loc     = combineLocs (map getLoc happy_var_1)
               ;  exps    = reverse (map unLoc happy_var_1)
               ;  nil     = ConExp (Con builtinNil) (fromLoc loc)
               ;  cons    = ConExp (Con builtinCons) (fromLoc loc)
               }
          in
            L loc $
            foldr (\hd tl -> opappE hd cons consFixity tl) nil exps
	)}

happyReduce_195 = happyMonadReduce 2# 73# happyReduction_195
happyReduction_195 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut72 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	(  do  {  let loc    = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
               ;  fromExp    <- checkExp (unLoc happy_var_1)
               ;  return $ locate loc $
                  ArithSeqExp fromExp Nothing Nothing
               })}}
	) (\r -> happyReturn (happyIn83 r))

happyReduce_196 = happyMonadReduce 4# 73# happyReduction_196
happyReduction_196 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut72 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	(  do  {  let loc    = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
               ;  fromExp    <- checkExp (unLoc happy_var_1)
               ;  thenExp    <- checkExp (unLoc happy_var_3)
               ;  return $ locate loc $
                  ArithSeqExp fromExp (Just thenExp) Nothing
               })}}}
	) (\r -> happyReturn (happyIn83 r))

happyReduce_197 = happyMonadReduce 3# 73# happyReduction_197
happyReduction_197 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut72 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	(  do  {  let loc    = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
               ;  fromExp    <- checkExp (unLoc happy_var_1)
               ;  toExp      <- checkExp (unLoc happy_var_3)
               ;  return $ locate loc $
                  ArithSeqExp fromExp Nothing (Just toExp)
               })}}
	) (\r -> happyReturn (happyIn83 r))

happyReduce_198 = happyMonadReduce 5# 73# happyReduction_198
happyReduction_198 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut72 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	case happyOut72 happy_x_5 of { happy_var_5 -> 
	(  do  {  let loc    = combineLocs [getLoc happy_var_1, getLoc happy_var_5]
               ;  fromExp    <- checkExp (unLoc happy_var_1)
               ;  thenExp    <- checkExp (unLoc happy_var_3)
               ;  toExp      <- checkExp (unLoc happy_var_5)
               ;  return $ locate loc $
                  ArithSeqExp fromExp (Just thenExp) (Just toExp)
               })}}}
	) (\r -> happyReturn (happyIn83 r))

happyReduce_199 = happyMonadReduce 3# 73# happyReduction_199
happyReduction_199 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut72 happy_x_1 of { happy_var_1 -> 
	case happyOut93 happy_x_3 of { happy_var_3 -> 
	(  do  {  let loc    = combineLocs (getLoc happy_var_1 : map getLoc happy_var_3)
               ;  exp        <- checkExp (unLoc happy_var_1)
               ;  let quals  = reverse $ map unLoc happy_var_3
               ;  return $ locate loc $
                  ListCompExp exp quals
               })}}
	) (\r -> happyReturn (happyIn83 r))

happyReduce_200 = happySpecReduce_3  74# happyReduction_200
happyReduction_200 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut84 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	happyIn84
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_201 = happySpecReduce_3  74# happyReduction_201
happyReduction_201 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	happyIn84
		 ([happy_var_3, happy_var_1]
	)}}

happyReduce_202 = happySpecReduce_1  75# happyReduction_202
happyReduction_202 happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	happyIn85
		 ([happy_var_1]
	)}

happyReduce_203 = happySpecReduce_3  75# happyReduction_203
happyReduction_203 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut85 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	happyIn85
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_204 = happySpecReduce_3  76# happyReduction_204
happyReduction_204 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut85 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	happyIn86
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_205 = happySpecReduce_3  77# happyReduction_205
happyReduction_205 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut101 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	happyIn87
		 ((unLoc happy_var_1, unLoc happy_var_3)
	)}}

happyReduce_206 = happySpecReduce_0  78# happyReduction_206
happyReduction_206  =  happyIn88
		 ([]
	)

happyReduce_207 = happySpecReduce_1  78# happyReduction_207
happyReduction_207 happy_x_1
	 =  case happyOut87 happy_x_1 of { happy_var_1 -> 
	happyIn88
		 ([happy_var_1]
	)}

happyReduce_208 = happySpecReduce_3  78# happyReduction_208
happyReduction_208 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut88 happy_x_1 of { happy_var_1 -> 
	case happyOut87 happy_x_3 of { happy_var_3 -> 
	happyIn88
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_209 = happyMonadReduce 1# 79# happyReduction_209
happyReduction_209 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut75 happy_x_1 of { happy_var_1 -> 
	( do  {  pat  <- checkPat (unLoc happy_var_1)
              ;  return $ L (getLoc happy_var_1) pat
              })}
	) (\r -> happyReturn (happyIn89 r))

happyReduce_210 = happyMonadReduce 1# 80# happyReduction_210
happyReduction_210 (happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut80 happy_x_1 of { happy_var_1 -> 
	( do  {  pat  <- checkPat (unLoc happy_var_1)
              ;  return $ L (getLoc happy_var_1) pat
              })}
	) (\r -> happyReturn (happyIn90 r))

happyReduce_211 = happySpecReduce_1  81# happyReduction_211
happyReduction_211 happy_x_1
	 =  case happyOut90 happy_x_1 of { happy_var_1 -> 
	happyIn91
		 ([happy_var_1]
	)}

happyReduce_212 = happySpecReduce_2  81# happyReduction_212
happyReduction_212 happy_x_2
	happy_x_1
	 =  case happyOut91 happy_x_1 of { happy_var_1 -> 
	case happyOut90 happy_x_2 of { happy_var_2 -> 
	happyIn91
		 (happy_var_2 : happy_var_1
	)}}

happyReduce_213 = happyMonadReduce 3# 82# happyReduction_213
happyReduction_213 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut89 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	( do  {  let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
              ;  let pat = unLoc happy_var_1
              ;  let exp = unLoc happy_var_3
              ;  return $ locate loc $
                 GenQual pat exp
              })}}
	) (\r -> happyReturn (happyIn92 r))

happyReduce_214 = happySpecReduce_2  82# happyReduction_214
happyReduction_214 happy_x_2
	happy_x_1
	 =  happyIn92
		 (error "cannot fully handle statements"
	)

happyReduce_215 = happySpecReduce_1  82# happyReduction_215
happyReduction_215 happy_x_1
	 =  happyIn92
		 (error "cannot fully handle statements"
	)

happyReduce_216 = happySpecReduce_0  83# happyReduction_216
happyReduction_216  =  happyIn93
		 ([]
	)

happyReduce_217 = happySpecReduce_1  83# happyReduction_217
happyReduction_217 happy_x_1
	 =  case happyOut92 happy_x_1 of { happy_var_1 -> 
	happyIn93
		 ([ happy_var_1]
	)}

happyReduce_218 = happySpecReduce_3  83# happyReduction_218
happyReduction_218 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut93 happy_x_1 of { happy_var_1 -> 
	case happyOut92 happy_x_3 of { happy_var_3 -> 
	happyIn93
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_219 = happyMonadReduce 4# 84# happyReduction_219
happyReduction_219 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut89 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	case happyOut69 happy_x_4 of { happy_var_4 -> 
	( do  {  let loc = combineLocs $ getLoc happy_var_1 : getLoc happy_var_3 : map getLoc happy_var_4
             ;  let pat = unLoc happy_var_1
             ;  let exp = unLoc happy_var_3
             ;  return $ L loc $
                Alt pat [(Nothing, exp)] (map unLoc happy_var_4)
             })}}}
	) (\r -> happyReturn (happyIn94 r))

happyReduce_220 = happyMonadReduce 3# 84# happyReduction_220
happyReduction_220 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut89 happy_x_1 of { happy_var_1 -> 
	case happyOut96 happy_x_2 of { happy_var_2 -> 
	case happyOut69 happy_x_3 of { happy_var_3 -> 
	( do  {  let loc = combineLocs $ getLoc happy_var_1 : getLoc happy_var_2 : map getLoc happy_var_3
             ;  let pat = unLoc happy_var_1
             ;  return $ L loc $
                Alt pat (unLoc happy_var_2) (map unLoc happy_var_3)
             })}}}
	) (\r -> happyReturn (happyIn94 r))

happyReduce_221 = happySpecReduce_1  85# happyReduction_221
happyReduction_221 happy_x_1
	 =  case happyOut94 happy_x_1 of { happy_var_1 -> 
	happyIn95
		 ([happy_var_1]
	)}

happyReduce_222 = happySpecReduce_3  85# happyReduction_222
happyReduction_222 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut95 happy_x_1 of { happy_var_1 -> 
	case happyOut94 happy_x_3 of { happy_var_3 -> 
	happyIn95
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_223 = happySpecReduce_3  85# happyReduction_223
happyReduction_223 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut95 happy_x_1 of { happy_var_1 -> 
	happyIn95
		 (happy_var_1
	)}

happyReduce_224 = happyMonadReduce 4# 86# happyReduction_224
happyReduction_224 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut75 happy_x_2 of { happy_var_2 -> 
	case happyOut72 happy_x_4 of { happy_var_4 -> 
	( do  {  let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
             ;  exp <- checkExp (unLoc happy_var_2)
             ;  return $ L loc $
                [(Just exp, unLoc happy_var_4)]
             })}}}
	) (\r -> happyReturn (happyIn96 r))

happyReduce_225 = happyMonadReduce 5# 86# happyReduction_225
happyReduction_225 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut96 happy_x_1 of { happy_var_1 -> 
	case happyOut75 happy_x_3 of { happy_var_3 -> 
	case happyOut72 happy_x_5 of { happy_var_5 -> 
	( do  {  let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_5]
             ;  exp <- checkExp (unLoc happy_var_3)
             ;  return $ L loc $
                (Just exp, unLoc happy_var_5) : unLoc happy_var_1
             })}}}
	) (\r -> happyReturn (happyIn96 r))

happyReduce_226 = happySpecReduce_1  87# happyReduction_226
happyReduction_226 happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	happyIn97
		 (locate (getLoc happy_var_1) $ ExpStmt (unLoc happy_var_1)
	)}

happyReduce_227 = happyMonadReduce 3# 87# happyReduction_227
happyReduction_227 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest) tk
	 = happyThen (case happyOut89 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	( do  {  let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
             ;  return $ locate loc $
                (PatStmt (unLoc happy_var_1) (unLoc happy_var_3))
             })}}
	) (\r -> happyReturn (happyIn97 r))

happyReduce_228 = happySpecReduce_2  87# happyReduction_228
happyReduction_228 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut46 happy_x_2 of { happy_var_2 -> 
	happyIn97
		 (let loc = combineLocs $ getLoc happy_var_1 : map getLoc happy_var_2
        in
          locate loc $ LetStmt (map unLoc happy_var_2)
	)}}

happyReduce_229 = happySpecReduce_1  88# happyReduction_229
happyReduction_229 happy_x_1
	 =  case happyOut97 happy_x_1 of { happy_var_1 -> 
	happyIn98
		 ([happy_var_1]
	)}

happyReduce_230 = happySpecReduce_3  88# happyReduction_230
happyReduction_230 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut98 happy_x_1 of { happy_var_1 -> 
	case happyOut97 happy_x_3 of { happy_var_3 -> 
	happyIn98
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_231 = happySpecReduce_2  89# happyReduction_231
happyReduction_231 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn99
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
        in
          L loc $ TupleCon 0
	)}}

happyReduce_232 = happySpecReduce_2  89# happyReduction_232
happyReduction_232 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn99
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
        in
          L loc $ Con builtinNil
	)}}

happyReduce_233 = happyReduce 4# 89# happyReduction_233
happyReduction_233 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut60 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn99
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
        in
          L loc $ TupleCon (2 + happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_234 = happySpecReduce_1  89# happyReduction_234
happyReduction_234 happy_x_1
	 =  case happyOut103 happy_x_1 of { happy_var_1 -> 
	happyIn99
		 (happy_var_1
	)}

happyReduce_235 = happySpecReduce_1  90# happyReduction_235
happyReduction_235 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn100
		 (L (getLoc happy_var_1) $ Var $ unLoc happy_var_1
	)}

happyReduce_236 = happySpecReduce_3  90# happyReduction_236
happyReduction_236 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn100
		 (L (getLoc happy_var_2) $ Var $ unLoc happy_var_2
	)}

happyReduce_237 = happySpecReduce_1  91# happyReduction_237
happyReduction_237 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn101
		 (L (getLoc happy_var_1) $ Var $ unLoc happy_var_1
	)}

happyReduce_238 = happySpecReduce_3  91# happyReduction_238
happyReduction_238 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn101
		 (L (getLoc happy_var_2) $ Var $ unLoc happy_var_2
	)}

happyReduce_239 = happySpecReduce_3  91# happyReduction_239
happyReduction_239 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn101
		 (L (getLoc happy_var_2) $ Var $ qname (getLoc happy_var_2) (getQVARSYM happy_var_2)
	)}

happyReduce_240 = happySpecReduce_1  91# happyReduction_240
happyReduction_240 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn101
		 (L (getLoc happy_var_1) $ AntiVar (getANTI_VAR happy_var_1)
	)}

happyReduce_241 = happySpecReduce_1  91# happyReduction_241
happyReduction_241 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn101
		 (L (getLoc happy_var_1) $ AntiVarId (getANTI_ID happy_var_1)
	)}

happyReduce_242 = happySpecReduce_1  92# happyReduction_242
happyReduction_242 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn102
		 (L (getLoc happy_var_1) $ Con $ unLoc happy_var_1
	)}

happyReduce_243 = happySpecReduce_3  92# happyReduction_243
happyReduction_243 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn102
		 (L (getLoc happy_var_2) $ Con $ unLoc happy_var_2
	)}

happyReduce_244 = happySpecReduce_1  93# happyReduction_244
happyReduction_244 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn103
		 (L (getLoc happy_var_1) $ Con $ unLoc happy_var_1
	)}

happyReduce_245 = happySpecReduce_3  93# happyReduction_245
happyReduction_245 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut112 happy_x_2 of { happy_var_2 -> 
	happyIn103
		 (happy_var_2
	)}

happyReduce_246 = happySpecReduce_1  94# happyReduction_246
happyReduction_246 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn104
		 (L (getLoc happy_var_1) $ Var $ unLoc happy_var_1
	)}

happyReduce_247 = happySpecReduce_3  94# happyReduction_247
happyReduction_247 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_2 of { happy_var_2 -> 
	happyIn104
		 (L (getLoc happy_var_1) $ Var $ unLoc happy_var_2
	)}}

happyReduce_248 = happySpecReduce_1  95# happyReduction_248
happyReduction_248 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn105
		 (L (getLoc happy_var_1) $ Var $ unLoc happy_var_1
	)}

happyReduce_249 = happySpecReduce_3  95# happyReduction_249
happyReduction_249 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn105
		 (L (getLoc happy_var_1) $ Var $ unLoc happy_var_2
	)}}

happyReduce_250 = happySpecReduce_1  96# happyReduction_250
happyReduction_250 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn106
		 (L (getLoc happy_var_1) $ Var $ unLoc happy_var_1
	)}

happyReduce_251 = happySpecReduce_3  96# happyReduction_251
happyReduction_251 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn106
		 (L (getLoc happy_var_1) $ Var $ unLoc happy_var_2
	)}}

happyReduce_252 = happySpecReduce_1  97# happyReduction_252
happyReduction_252 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn107
		 (L (getLoc happy_var_1) $ Con $ unLoc happy_var_1
	)}

happyReduce_253 = happySpecReduce_3  97# happyReduction_253
happyReduction_253 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn107
		 (L (getLoc happy_var_1) $ Con $ unLoc happy_var_2
	)}}

happyReduce_254 = happySpecReduce_1  98# happyReduction_254
happyReduction_254 happy_x_1
	 =  case happyOut112 happy_x_1 of { happy_var_1 -> 
	happyIn108
		 (happy_var_1
	)}

happyReduce_255 = happySpecReduce_3  98# happyReduction_255
happyReduction_255 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn108
		 (L (getLoc happy_var_1) $ Con $ unLoc happy_var_2
	)}}

happyReduce_256 = happySpecReduce_1  99# happyReduction_256
happyReduction_256 happy_x_1
	 =  case happyOut104 happy_x_1 of { happy_var_1 -> 
	happyIn109
		 (let L loc (Var op) = happy_var_1
             in
               L loc op
	)}

happyReduce_257 = happySpecReduce_1  99# happyReduction_257
happyReduction_257 happy_x_1
	 =  case happyOut107 happy_x_1 of { happy_var_1 -> 
	happyIn109
		 (let L loc (Con op) = happy_var_1
             in
               L loc op
	)}

happyReduce_258 = happySpecReduce_1  100# happyReduction_258
happyReduction_258 happy_x_1
	 =  case happyOut105 happy_x_1 of { happy_var_1 -> 
	happyIn110
		 (locate (getLoc happy_var_1) $ VarExp (unLoc happy_var_1)
	)}

happyReduce_259 = happySpecReduce_1  100# happyReduction_259
happyReduction_259 happy_x_1
	 =  case happyOut108 happy_x_1 of { happy_var_1 -> 
	happyIn110
		 (locate (getLoc happy_var_1) $ ConExp (unLoc happy_var_1)
	)}

happyReduce_260 = happySpecReduce_1  101# happyReduction_260
happyReduction_260 happy_x_1
	 =  case happyOut106 happy_x_1 of { happy_var_1 -> 
	happyIn111
		 (locate (getLoc happy_var_1) $ VarExp (unLoc happy_var_1)
	)}

happyReduce_261 = happySpecReduce_1  101# happyReduction_261
happyReduction_261 happy_x_1
	 =  case happyOut108 happy_x_1 of { happy_var_1 -> 
	happyIn111
		 (locate (getLoc happy_var_1) $ ConExp (unLoc happy_var_1)
	)}

happyReduce_262 = happySpecReduce_1  102# happyReduction_262
happyReduction_262 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn112
		 (L (getLoc happy_var_1) $ Con $ name (getLoc happy_var_1) ":"
	)}

happyReduce_263 = happySpecReduce_1  102# happyReduction_263
happyReduction_263 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn112
		 (L (getLoc happy_var_1) $ Con $ unLoc happy_var_1
	)}

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	L _ Teof -> happyDoAction 66# tk action sts stk;
	L _ (Tinteger _) -> cont 1#;
	L _ (Tfloat _) -> cont 2#;
	L _ (Tchar _) -> cont 3#;
	L _ (Tstring _) -> cont 4#;
	L _ (Tqvarid []  _) -> cont 5#;
	L _ (Tqvarid _   _) -> cont 6#;
	L _ (Tqconid []  _) -> cont 7#;
	L _ (Tqconid _   _) -> cont 8#;
	L _ (Tqvarsym []  _) -> cont 9#;
	L _ (Tqvarsym _   _) -> cont 10#;
	L _ (Tqconsym []  _) -> cont 11#;
	L _ (Tqconsym _   _) -> cont 12#;
	L _ Tcase -> cont 13#;
	L _ Tclass -> cont 14#;
	L _ Tdata -> cont 15#;
	L _ Tdefault -> cont 16#;
	L _ Tderiving -> cont 17#;
	L _ Tdo -> cont 18#;
	L _ Telse -> cont 19#;
	L _ Tif -> cont 20#;
	L _ Timport -> cont 21#;
	L _ Tin -> cont 22#;
	L _ Tinfix -> cont 23#;
	L _ Tinfixl -> cont 24#;
	L _ Tinfixr -> cont 25#;
	L _ Tinstance -> cont 26#;
	L _ Tlet -> cont 27#;
	L _ Tmodule -> cont 28#;
	L _ Tnewtype -> cont 29#;
	L _ Tof -> cont 30#;
	L _ Tthen -> cont 31#;
	L _ Ttype -> cont 32#;
	L _ Twhere -> cont 33#;
	L _ Tunderscore -> cont 34#;
	L _ Tas -> cont 35#;
	L _ Tqualified -> cont 36#;
	L _ Thiding -> cont 37#;
	L _ Tdotdot -> cont 38#;
	L _ Tcolon -> cont 39#;
	L _ Tdcolon -> cont 40#;
	L _ Tequal -> cont 41#;
	L _ Tlam -> cont 42#;
	L _ Tvbar -> cont 43#;
	L _ Tlarrow -> cont 44#;
	L _ Trarrow -> cont 45#;
	L _ Tat -> cont 46#;
	L _ Ttilda -> cont 47#;
	L _ Tdarrow -> cont 48#;
	L _ Tminus -> cont 49#;
	L _ Tbang -> cont 50#;
	L _ Tlparen -> cont 51#;
	L _ Trparen -> cont 52#;
	L _ Tcomma -> cont 53#;
	L _ Tsemi -> cont 54#;
	L _ Tlbrack -> cont 55#;
	L _ Trbrack -> cont 56#;
	L _ Tbackquote -> cont 57#;
	L _ Tlbrace -> cont 58#;
	L _ Trbrace -> cont 59#;
	L _ (Tanti_id _) -> cont 60#;
	L _ (Tanti_int _) -> cont 61#;
	L _ (Tanti_float _) -> cont 62#;
	L _ (Tanti_var _) -> cont 63#;
	L _ (Tanti_type _) -> cont 64#;
	L _ (Tanti_exp _) -> cont 65#;
	_ -> happyError' tk
	})

happyError_ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (>>=)
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => ((L Token)) -> P a
happyError' tk = happyError tk

parseModule = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut27 x))

parseBody = happySomeParser where
  happySomeParser = happyThen (happyParse 1#) (\x -> happyReturn (happyOut41 x))

parseTopDecls = happySomeParser where
  happySomeParser = happyThen (happyParse 2#) (\x -> happyReturn (happyOut42 x))

parseDecls = happySomeParser where
  happySomeParser = happyThen (happyParse 3#) (\x -> happyReturn (happyOut46 x))

parseType = happySomeParser where
  happySomeParser = happyThen (happyParse 4#) (\x -> happyReturn (happyOut53 x))

parseExp = happySomeParser where
  happySomeParser = happyThen (happyParse 5#) (\x -> happyReturn (happyOut72 x))

parseStmt = happySomeParser where
  happySomeParser = happyThen (happyParse 6#) (\x -> happyReturn (happyOut97 x))

happySeq = happyDontSeq


--
-- \subsection{Lexing}


runLayout :: (MonadParser m) => m ()
runLayout = do
    inp <- getTokens
    case inp of
      [] -> do  tokenStack <- getTokenStack
                layoutStack <- getLayoutStack
                (inp', tokenStack', layoutStack')
                    <- lay (tokenStack, layoutStack)
                setTokens $ filter (not . isWhitespace) inp'
                setTokenStack tokenStack'
                setLayoutStack layoutStack'
                runLayout
      _ -> return ()
    setNeedRightBrace False
  where
    isWhitespace :: L Token -> Bool
    isWhitespace (L _ (Tws _))  = True
    isWhitespace _              = False

lookForRightBrace :: (MonadParser m) => m ()
lookForRightBrace = do
    setNeedRightBrace True
    t <- getLastToken
    ts <- getTokens
    setTokens (t : ts)
    runLayout
    t <- nextToken
    case t of
      L _ Trbrace      -> return ()
      L (Loc pos _) _  -> throwExceptionAt pos $ Expected ["}"]

nextToken :: (MonadParser m) => m (L Token)
nextToken = do
    runLayout
    (t : ts) <- getTokens
    setLastToken t
    setTokens ts
    return t

lexer :: (MonadParser m) => (L Token -> m a) -> m a
lexer cont = do
    tok <- nextToken
    cont tok



locate :: Loc -> (SrcLoc -> a) -> L a
locate loc f = L loc (f (SrcLoc loc))



reloc :: (a -> b) -> L a -> L b
reloc f a = L (getLoc a) $ f (unLoc a)



name :: Loc -> String -> Name
name loc s = mkUnqualName s loc

qname :: Loc -> ([String], String) -> Name
qname loc (q, s) = mkQualName q s loc

locname :: Loc -> String -> L Name
locname loc s = L loc $ mkUnqualName s loc

locqname :: Loc -> ([String], String) -> L Name
locqname loc (q, s) = L loc $ mkQualName q s loc



happyError :: MonadParser m => L Token -> m a
happyError (L (Loc start _) _) = throwExceptionAt start ParserError

getCHAR     (L _ (Tchar x))     = x
getCHAR     _                   = internalErr $ text "not a CHAR"

getSTRING   (L _ (Tstring x))   = x
getSTRING   _                   = internalErr $ text "not a STRING"

getINTEGER  (L _ (Tinteger x))  = x
getINTEGER  _                   = internalErr $ text "not an INTEGER"

getFLOAT    (L _ (Tfloat x))    = x
getFLOAT    _                   = internalErr $ text "not a FLOAT"

getVARID   (L _ (Tqvarid [] x))  = x
getVARID    _                    = internalErr $ text "not a VARID"

getQVARID  (L _ (Tqvarid qs x))  = (qs, x)
getQVARID   _                    = internalErr $ text "not a VARID"

getCONID   (L _ (Tqconid [] x))  = x
getCONID   _                     = internalErr $ text "not a CONID"

getQCONID  (L _ (Tqconid qs x))  = (qs, x)
getQCONID   _                    = internalErr $ text "not a QCONID"

getVARSYM   (L _ (Tqvarsym [] x))  = x
getVARSYM   _                      = internalErr $ text "not a VARSYM"

getQVARSYM  (L _ (Tqvarsym qs x))  = (qs, x)
getQVARSYM  _                      = internalErr $ text "not a QVARSYM"

getCONSYM   (L _ (Tqconsym [] x))  = x
getCONSYM  _                       = internalErr $ text "not a CONSYM"

getQCONSYM  (L _ (Tqconsym qs x))  = (qs, x)
getQCONSYM  _                      = internalErr $ text "not a QCONSYM"

getANTI_ID  (L _ (Tanti_id s))        = s
getANTI_ID  _                         = internalErr $ text "not an ANTI_ID"

getANTI_INT  (L _ (Tanti_int s))      = s
getANTI_INT  _                        = internalErr $ text "not an ANTI_INT"

getANTI_FLOAT  (L _ (Tanti_float s))  = s
getANTI_FLOAT  _                      = internalErr $ text "not an ANTI_FLOAT"

getANTI_VAR  (L _ (Tanti_var v))      = v
getANTI_VAR  _                        = internalErr $ text "not an ANTI_VAR"

getANTI_TYPE  (L _ (Tanti_type ty))   = ty
getANTI_TYPE  _                       = internalErr $ text "not an ANTI_TYPE"

getANTI_EXP  (L _ (Tanti_exp e))      = e
getANTI_EXP  _                        = internalErr $ text "not an ANTI_EXP"

--
-- %if style == code
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<indbygget>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Int# Happy_IntList





{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n <# (0# :: Int#)) -> {- nothing -}

				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# (0# :: Int#))
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st

{-# LINE 127 "templates/GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ > 500
	narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
	intToInt16# i
#else
	(i `iShiftL#` 16#) `iShiftRA#` 16#
#endif
  where
#if __GLASGOW_HASKELL__ >= 503
	i = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
#else
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
#endif
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#





data HappyAddr = HappyA# Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case unsafeCoerce# x of { (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             off    = indexShortOffAddr happyGotoOffsets st1
             off_i  = (off +# nt)
             new_state = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (unsafeCoerce# (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
