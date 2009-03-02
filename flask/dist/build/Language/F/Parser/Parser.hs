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
-- Module      :  Language.F.Parser.Parser
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.F.Parser.Parser (
    parseBody,
    parseType,
    parseExp
  ) where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map

import Compiler.Opt
import Control.Monad.ContextException
import Control.Monad.Exception
import Data.Loc
import Data.Name
import Language.F.Parser.Exceptions
import Language.F.Parser.Monad
import Language.F.Parser.Lexer
import Language.F.Parser.Tokens
import Language.F
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
happyIn6 :: (L Exp) -> (HappyAbsSyn )
happyIn6 x = unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (L Exp)
happyOut6 x = unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (L Name) -> (HappyAbsSyn )
happyIn7 x = unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (L Name)
happyOut7 x = unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (L Name) -> (HappyAbsSyn )
happyIn8 x = unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (L Name)
happyOut8 x = unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (L Name) -> (HappyAbsSyn )
happyIn9 x = unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (L Name)
happyOut9 x = unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (L Name) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (L Name)
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (L Name) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (L Name)
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: (L TyCon) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> (L TyCon)
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (L TyCon) -> (HappyAbsSyn )
happyIn13 x = unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (L TyCon)
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
happyIn18 :: ([L Decl]) -> (HappyAbsSyn )
happyIn18 x = unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> ([L Decl])
happyOut18 x = unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ([L Decl]) -> (HappyAbsSyn )
happyIn19 x = unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ([L Decl])
happyOut19 x = unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (L Decl) -> (HappyAbsSyn )
happyIn20 x = unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (L Decl)
happyOut20 x = unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ([L Binding]) -> (HappyAbsSyn )
happyIn21 x = unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> ([L Binding])
happyOut21 x = unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (L Binding) -> (HappyAbsSyn )
happyIn22 x = unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (L Binding)
happyOut22 x = unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (L Kind) -> (HappyAbsSyn )
happyIn23 x = unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (L Kind)
happyOut23 x = unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (L Kind) -> (HappyAbsSyn )
happyIn24 x = unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (L Kind)
happyOut24 x = unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (L Type) -> (HappyAbsSyn )
happyIn25 x = unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (L Type)
happyOut25 x = unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: ([L Type]) -> (HappyAbsSyn )
happyIn26 x = unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> ([L Type])
happyOut26 x = unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: ([L Type]) -> (HappyAbsSyn )
happyIn27 x = unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> ([L Type])
happyOut27 x = unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: ([L Type]) -> (HappyAbsSyn )
happyIn28 x = unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> ([L Type])
happyOut28 x = unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (L (WildTyVar, Kind)) -> (HappyAbsSyn )
happyIn29 x = unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (L (WildTyVar, Kind))
happyOut29 x = unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: ([L (WildTyVar, Kind)]) -> (HappyAbsSyn )
happyIn30 x = unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> ([L (WildTyVar, Kind)])
happyOut30 x = unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (L Type) -> (HappyAbsSyn )
happyIn31 x = unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (L Type)
happyOut31 x = unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (L Type) -> (HappyAbsSyn )
happyIn32 x = unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (L Type)
happyOut32 x = unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (L Type) -> (HappyAbsSyn )
happyIn33 x = unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (L Type)
happyOut33 x = unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: (Int) -> (HappyAbsSyn )
happyIn34 x = unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> (Int)
happyOut34 x = unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: ([L ConDecl]) -> (HappyAbsSyn )
happyIn35 x = unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> ([L ConDecl])
happyOut35 x = unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (L ConDecl) -> (HappyAbsSyn )
happyIn36 x = unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (L ConDecl)
happyOut36 x = unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: ([L Var]) -> (HappyAbsSyn )
happyIn37 x = unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> ([L Var])
happyOut37 x = unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (L Exp) -> (HappyAbsSyn )
happyIn38 x = unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (L Exp)
happyOut38 x = unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: (L Exp) -> (HappyAbsSyn )
happyIn39 x = unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (L Exp)
happyOut39 x = unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (L Exp) -> (HappyAbsSyn )
happyIn40 x = unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (L Exp)
happyOut40 x = unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (L Exp) -> (HappyAbsSyn )
happyIn41 x = unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (L Exp)
happyOut41 x = unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (L Exp) -> (HappyAbsSyn )
happyIn42 x = unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (L Exp)
happyOut42 x = unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (L Exp) -> (HappyAbsSyn )
happyIn43 x = unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (L Exp)
happyOut43 x = unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: ([L Exp]) -> (HappyAbsSyn )
happyIn44 x = unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> ([L Exp])
happyOut44 x = unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (L Alt) -> (HappyAbsSyn )
happyIn45 x = unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> (L Alt)
happyOut45 x = unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: ([L Alt]) -> (HappyAbsSyn )
happyIn46 x = unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> ([L Alt])
happyOut46 x = unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (L (WildVar, Type)) -> (HappyAbsSyn )
happyIn47 x = unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (L (WildVar, Type))
happyOut47 x = unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: ([L (WildVar, Type)]) -> (HappyAbsSyn )
happyIn48 x = unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> ([L (WildVar, Type)])
happyOut48 x = unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: (L (WildTyVar, Kind)) -> (HappyAbsSyn )
happyIn49 x = unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> (L (WildTyVar, Kind))
happyOut49 x = unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: ([L (WildTyVar, Kind)]) -> (HappyAbsSyn )
happyIn50 x = unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> ([L (WildTyVar, Kind)])
happyOut50 x = unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: (L Pat) -> (HappyAbsSyn )
happyIn51 x = unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> (L Pat)
happyOut51 x = unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: (L Con) -> (HappyAbsSyn )
happyIn52 x = unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> (L Con)
happyOut52 x = unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: (L Var) -> (HappyAbsSyn )
happyIn53 x = unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> (L Var)
happyOut53 x = unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: (L Var) -> (HappyAbsSyn )
happyIn54 x = unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> (L Var)
happyOut54 x = unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyIn55 :: (L Con) -> (HappyAbsSyn )
happyIn55 x = unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> (L Con)
happyOut55 x = unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: (L Con) -> (HappyAbsSyn )
happyIn56 x = unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> (L Con)
happyOut56 x = unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyIn57 :: (L Con) -> (HappyAbsSyn )
happyIn57 x = unsafeCoerce# x
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> (L Con)
happyOut57 x = unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyInTok :: ((L Token)) -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> ((L Token))
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x96\x01\xdd\x01\x7d\x01\x14\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe9\x01\x00\x00\x00\x00\xea\x01\x35\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7d\x01\xe6\x01\xe5\x01\xe2\x00\x05\x02\x4e\x01\xd7\x01\x00\x00\x00\x00\x00\x00\x00\x00\xe2\x01\xe4\x01\xce\x01\xd5\x01\x00\x00\x00\x00\xe3\x01\xe3\x01\xe3\x01\x00\x00\xcd\x00\xc1\x01\xa6\x01\x00\x00\xce\x01\x96\x01\x00\x00\xe1\x01\xf5\x01\xca\x01\xae\x01\x42\x01\xcc\x01\x00\x00\x00\x00\x00\x00\x00\x00\xb4\x00\xb4\x00\x00\x00\xd8\x01\xdd\x01\x00\x00\xc2\x01\x00\x00\x37\x00\x3b\x00\xc4\x01\xbd\x01\xb5\x01\x00\x00\xb3\x01\xc0\x01\x00\x00\x80\x00\x94\x00\xe3\x01\xc1\x01\xe3\x01\xe3\x01\x00\x00\xdd\x01\xe3\x01\xe3\x01\xe3\xff\xdd\x01\x00\x00\xaf\x01\x00\x00\x00\x00\x40\x01\xab\x01\xa8\x01\x00\x00\x00\x00\x00\x00\x00\x00\xa5\x01\x28\x00\xb2\x01\x92\x00\x92\x00\xb7\x01\x00\x00\x9f\x01\xdd\x01\x00\x00\x93\x01\x92\x00\x95\x01\xa4\x01\xb8\x00\x00\x00\xdd\x01\x7d\x01\xe3\xff\x8f\x01\x92\x01\x00\x00\x00\x00\x00\x00\x7d\x01\x00\x00\x00\x00\x00\x00\xdd\x01\xe3\x01\xe3\x01\x00\x00\x8e\x01\x8d\x01\xdd\x01\x90\x00\xe3\xff\x89\x01\x00\x00\x00\x00\xdd\x01\xdd\x01\x00\x00\x00\x00\x00\x00\x81\x01\xe3\xff\x80\x01\xb7\x00\x00\x00\x05\x00\x00\x00\x00\x00\x88\x01\x00\x00\x77\x01\x6d\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x75\x01\x00\x00\x20\x01\x00\x00\x00\x00\x73\x01\x00\x00\x72\x01\x79\x01\xdd\x01\x6b\x01\x47\x01\x00\x00\x91\x01\x7d\x01\x51\x01\x7d\x01\x7d\x01\x7d\x01\x00\x00\x7d\x01\x00\x00\x34\x01\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x7d\x01\x00\x00\x00\x00\x52\x00\x00\x00\x41\x01\x17\x01\x00\x00\x00\x00\x00\x00\x00\x00\xee\x01\x2a\x01\x06\x01\x00\x00\x8b\x00\x0d\x00\x7d\x01\x85\x01\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\xe9\xff\x12\x01\xf6\x00\xdd\x01\x05\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfd\x00\xf1\x00\x00\x00\xce\x00\x00\x00\x0c\x00\xdd\x01\xdd\x01\xdf\x00\xdd\x00\x00\x00\xe3\xff\xe3\xff\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x05\x00\xca\x00\xbb\x00\xa8\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\x01\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x68\x01\x42\x03\xf3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xec\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\x00\x00\x00\x00\x00\x65\x00\x0e\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x03\x00\x00\x00\x00\x71\x03\x6c\x03\x67\x03\x00\x00\x28\x01\x27\x02\x27\x03\x00\x00\x00\x00\x9e\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\xc8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa5\x00\x67\x01\x00\x00\x00\x00\x22\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xba\x00\x00\x00\x00\x00\x00\x00\x5a\x01\x8c\x03\x0c\x02\x8c\x03\x8c\x03\x00\x00\x07\x03\x62\x03\x47\x03\x30\x01\x02\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x6c\x00\x2e\x01\x00\x00\x00\x00\xe7\x02\xe2\x02\x00\x00\x00\x00\x64\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc7\x02\xda\x00\x22\x01\x00\x00\x84\x00\x00\x00\x00\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\xc2\x02\x8c\x03\x8c\x03\x00\x00\x00\x00\x00\x00\xa7\x02\x2d\x01\x15\x01\x00\x00\x00\x00\x00\x00\xa2\x02\x87\x02\x00\x00\x00\x00\x00\x00\x00\x00\xa6\x00\x00\x00\x00\x00\x00\x00\x18\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x02\x00\x00\x00\x00\x00\x00\x03\x01\xa7\x00\x00\x00\xa1\x00\x8e\x00\x68\x00\x00\x00\x55\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x53\x00\x00\x00\x00\x00\x00\x00\x76\x01\x6d\x00\x1c\x00\x1c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x67\x02\x2c\x00\x2f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf2\xff\x00\x00\x60\x00\x62\x02\x47\x02\x00\x00\x00\x00\x00\x00\x8c\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x02\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\xfc\xff\x9b\xff\xf7\xff\x7c\xff\xf4\xff\x77\xff\x00\x00\xaa\xff\xa9\xff\xa8\xff\xa1\xff\xa0\xff\x9c\xff\x9d\xff\x7f\xff\xfb\xff\xfa\xff\xf9\xff\xf8\xff\xf6\xff\xf5\xff\xf3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf2\xff\xf0\xff\xbd\xff\xb9\xff\x00\x00\xda\xff\x00\x00\xd5\xff\xc0\xff\xbe\xff\x00\x00\x00\x00\x00\x00\xd8\xff\xc3\xff\x00\x00\x00\x00\x7e\xff\x00\x00\xe6\xff\xe5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xef\xff\xec\xff\xee\xff\xed\xff\xde\xff\x00\x00\xf1\xff\x00\x00\x00\x00\xe4\xff\x00\x00\xb7\xff\x00\x00\xc9\xff\x00\x00\x00\x00\x00\x00\xb8\xff\xb4\xff\xc6\xff\xc2\xff\x00\x00\x00\x00\xd0\xff\x00\x00\xce\xff\xcf\xff\xbf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81\xff\x00\x00\xe8\xff\x74\xff\x00\x00\x00\x00\x00\x00\xe9\xff\xe7\xff\x75\xff\x82\xff\xb4\xff\x00\x00\x00\x00\xde\xff\x00\x00\x00\x00\x9e\xff\x00\x00\x00\x00\xab\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdd\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb4\xff\x7a\xff\x76\xff\x9a\xff\x00\x00\x7b\xff\xcd\xff\xd9\xff\x00\x00\xd1\xff\xd2\xff\xd4\xff\xc9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb6\xff\xbc\xff\x00\x00\x00\x00\xba\xff\xd6\xff\xbb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x7d\xff\x00\x00\xe0\xff\xe1\xff\xe3\xff\xdf\xff\x00\x00\xc7\xff\xb5\xff\xc5\xff\xc1\xff\xd3\xff\xc4\xff\x00\x00\x98\xff\x00\x00\xb3\xff\x80\xff\x00\x00\xa6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9f\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x99\xff\x00\x00\xd7\xff\x00\x00\xdc\xff\x00\x00\x97\xff\xa5\xff\xa7\xff\xa3\xff\x00\x00\xa4\xff\x95\xff\x00\x00\x84\xff\x00\x00\x8b\xff\x88\xff\x87\xff\x86\xff\x85\xff\x00\x00\x00\x00\x00\x00\x8a\xff\x90\xff\x00\x00\x00\x00\x00\x00\xa2\xff\xdb\xff\x79\xff\x00\x00\xb2\xff\x00\x00\x00\x00\x00\x00\x00\x00\xae\xff\x00\x00\xe2\xff\x94\xff\x93\xff\x96\xff\x00\x00\x00\x00\x8f\xff\x83\xff\x89\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8e\xff\x00\x00\x00\x00\xb1\xff\x00\x00\xad\xff\xb0\xff\x78\xff\x00\x00\x00\x00\x00\x00\x00\x00\x91\xff\x92\xff\x8c\xff\x8d\xff\x00\x00\xac\xff\xaf\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x07\x00\x1e\x00\x25\x00\x08\x00\x05\x00\x0a\x00\x0b\x00\x03\x00\x2b\x00\x01\x00\x06\x00\x05\x00\x05\x00\x05\x00\x01\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x31\x00\x29\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x1b\x00\x1b\x00\x0a\x00\x2b\x00\x11\x00\x12\x00\x01\x00\x03\x00\x2e\x00\x2b\x00\x30\x00\x03\x00\x32\x00\x33\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x2f\x00\x2e\x00\x1c\x00\x30\x00\x1e\x00\x32\x00\x1d\x00\x1e\x00\x2e\x00\x1f\x00\x30\x00\x1e\x00\x32\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x01\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x23\x00\x2f\x00\x31\x00\x0a\x00\x0b\x00\x2d\x00\x31\x00\x01\x00\x27\x00\x2c\x00\x32\x00\x01\x00\x01\x00\x2c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x01\x00\x01\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x0f\x00\x10\x00\x2e\x00\x2e\x00\x30\x00\x2e\x00\x32\x00\x32\x00\x2e\x00\x32\x00\x30\x00\x33\x00\x32\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x2f\x00\x2f\x00\x05\x00\x2e\x00\x05\x00\x30\x00\x05\x00\x32\x00\x2f\x00\x1c\x00\x11\x00\x12\x00\x01\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x01\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x0e\x00\x2d\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x0f\x00\x10\x00\x2b\x00\x11\x00\x12\x00\x05\x00\x2f\x00\x2b\x00\x2e\x00\x2b\x00\x30\x00\x2b\x00\x32\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x2f\x00\x1c\x00\x2e\x00\x08\x00\x30\x00\x05\x00\x32\x00\x2f\x00\x2e\x00\x1c\x00\x30\x00\x30\x00\x32\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x2b\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x2e\x00\x2e\x00\x05\x00\x1e\x00\x32\x00\x32\x00\x30\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x2b\x00\x2c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x2b\x00\x2b\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x03\x00\x04\x00\x2e\x00\x2c\x00\x30\x00\x2c\x00\x32\x00\x2b\x00\x2e\x00\x1e\x00\x30\x00\x25\x00\x32\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x01\x00\x2e\x00\x1e\x00\x30\x00\x0b\x00\x32\x00\x03\x00\x04\x00\x2e\x00\x2c\x00\x30\x00\x1e\x00\x32\x00\x11\x00\x12\x00\x10\x00\x01\x00\x27\x00\x28\x00\x29\x00\x05\x00\x01\x00\x01\x00\x2d\x00\x2e\x00\x05\x00\x11\x00\x12\x00\x32\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x10\x00\x17\x00\x18\x00\x11\x00\x12\x00\x27\x00\x17\x00\x29\x00\x2f\x00\x2f\x00\x1e\x00\x2d\x00\x2e\x00\x09\x00\x2c\x00\x2d\x00\x32\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x01\x00\x0e\x00\x2f\x00\x1c\x00\x05\x00\x2b\x00\x13\x00\x14\x00\x22\x00\x2f\x00\x31\x00\x24\x00\x25\x00\x01\x00\x01\x00\x1c\x00\x1d\x00\x2c\x00\x2d\x00\x20\x00\x21\x00\x1f\x00\x17\x00\x24\x00\x25\x00\x0c\x00\x0d\x00\x0e\x00\x10\x00\x31\x00\x2b\x00\x2c\x00\x2d\x00\x11\x00\x2f\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x01\x00\x02\x00\x03\x00\x04\x00\x11\x00\x0e\x00\x07\x00\x08\x00\x1c\x00\x1c\x00\x13\x00\x14\x00\x01\x00\x02\x00\x03\x00\x04\x00\x2f\x00\x2f\x00\x07\x00\x08\x00\x2d\x00\x05\x00\x27\x00\x20\x00\x21\x00\x29\x00\x2a\x00\x2b\x00\x1a\x00\x2c\x00\x05\x00\x0f\x00\x07\x00\x08\x00\x2b\x00\x13\x00\x14\x00\x05\x00\x2f\x00\x07\x00\x08\x00\x2e\x00\x2b\x00\x12\x00\x32\x00\x2e\x00\x2f\x00\x2c\x00\x17\x00\x18\x00\x12\x00\x2c\x00\x2c\x00\x2c\x00\x2b\x00\x17\x00\x18\x00\x2d\x00\x2f\x00\x2b\x00\x1e\x00\x30\x00\x25\x00\x26\x00\x05\x00\x32\x00\x07\x00\x08\x00\x2b\x00\x25\x00\x26\x00\x16\x00\x2f\x00\x30\x00\x1e\x00\x2b\x00\x2d\x00\x12\x00\x2c\x00\x2f\x00\x30\x00\x2c\x00\x17\x00\x18\x00\x05\x00\x2c\x00\x07\x00\x08\x00\x1e\x00\x31\x00\x2d\x00\x2c\x00\x05\x00\x22\x00\x07\x00\x08\x00\x25\x00\x26\x00\x05\x00\x2c\x00\x07\x00\x08\x00\x2b\x00\x2c\x00\x2d\x00\x12\x00\x2f\x00\x2d\x00\x30\x00\x05\x00\x17\x00\x18\x00\x1e\x00\x22\x00\x2c\x00\x0b\x00\x0c\x00\x31\x00\x07\x00\x28\x00\x29\x00\x1e\x00\x2b\x00\x33\x00\x25\x00\x26\x00\x2f\x00\x23\x00\x22\x00\x30\x00\x2b\x00\x1b\x00\x05\x00\x1d\x00\x2f\x00\x01\x00\x2b\x00\x03\x00\x04\x00\x05\x00\x2f\x00\x07\x00\x2a\x00\x01\x00\x31\x00\x31\x00\xff\xff\xff\xff\x2c\x00\x2d\x00\x33\x00\x11\x00\x12\x00\x13\x00\xff\xff\x15\x00\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\xff\xff\x07\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\xff\xff\x15\x00\x16\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x01\x00\x07\x00\x03\x00\x04\x00\x05\x00\xff\xff\x07\x00\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x01\x00\x07\x00\x03\x00\x04\x00\x05\x00\xff\xff\x07\x00\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x01\x00\x07\x00\x03\x00\x04\x00\x05\x00\xff\xff\x07\x00\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x01\x00\x07\x00\x03\x00\x04\x00\x05\x00\xff\xff\x07\x00\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x01\x00\x07\x00\x03\x00\x04\x00\x05\x00\xff\xff\x07\x00\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x01\x00\x07\x00\x03\x00\x04\x00\x05\x00\xff\xff\x07\x00\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x01\x00\x07\x00\x03\x00\x04\x00\x05\x00\xff\xff\x07\x00\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x01\x00\x07\x00\x03\x00\x04\x00\x05\x00\xff\xff\x07\x00\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\x19\x00\x1a\x00\x1b\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x01\x00\x07\x00\x03\x00\x04\x00\x05\x00\xff\xff\x07\x00\xff\xff\xff\xff\xff\xff\xff\xff\x11\x00\x12\x00\x13\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\x01\x00\x07\x00\x03\x00\x04\x00\x05\x00\x01\x00\x07\x00\x03\x00\x04\x00\x05\x00\x01\x00\x07\x00\x03\x00\x04\x00\x05\x00\xff\xff\x07\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\xff\xff\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\xff\xff\x03\x00\x04\x00\x05\x00\xff\xff\x07\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1a\x00\x1b\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x19\x00\xdf\x00\x2f\x00\x5e\x00\x17\x00\x5f\x00\x60\x00\x42\x00\x85\x00\x21\x00\x43\x00\x17\x00\x17\x00\x69\x00\x32\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\xe0\x00\xef\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x61\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\xd1\x00\xe7\x00\xdd\x00\xdd\x00\xf9\x00\x26\x00\x32\x00\xd8\x00\x10\x00\x3b\x00\x11\x00\xd8\x00\x12\x00\x62\x00\xa7\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\xa8\x00\xe4\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x00\x01\x10\x00\x7a\x00\x11\x00\x7b\x00\x12\x00\xd9\x00\xda\x00\x10\x00\xf3\x00\x11\x00\xf2\x00\x12\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\xcf\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x5d\x00\xf4\x00\xdb\x00\x5f\x00\x60\x00\xf8\x00\xdb\x00\xcf\x00\x92\x00\x94\x00\xf9\x00\x32\x00\x32\x00\x93\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x32\x00\xe5\x00\xd7\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\xbf\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x76\x00\x77\x00\x10\x00\xe1\x00\x11\x00\xd6\x00\x12\x00\xe2\x00\x10\x00\xd7\x00\x11\x00\x62\x00\x12\x00\xc0\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\xb1\x00\x6a\x00\x17\x00\x10\x00\x17\x00\x11\x00\x17\x00\x12\x00\x75\x00\x8b\x00\xfa\x00\x26\x00\x32\x00\xa9\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x32\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x45\x00\x8c\x00\xc1\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x98\x00\x77\x00\xeb\x00\x9d\x00\x26\x00\x17\x00\xd4\x00\x53\x00\x10\x00\x3b\x00\x11\x00\x53\x00\x12\x00\xc2\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\xc4\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x36\x00\x7b\x00\x10\x00\x3b\x00\x11\x00\x17\x00\x12\x00\x75\x00\x10\x00\x8d\x00\x11\x00\xfe\x00\x12\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x3b\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x9b\x00\x9b\x00\x17\x00\x00\x01\x9c\x00\xaf\x00\xff\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\xd1\x00\xd2\x00\x05\x00\x06\x00\x07\x00\x08\x00\x09\x00\x53\x00\xeb\x00\xac\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x6d\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x08\x00\x09\x00\x10\x00\xfc\x00\x11\x00\xfd\x00\x12\x00\x3b\x00\x10\x00\xf1\x00\x11\x00\x6e\x00\x12\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x32\x00\x10\x00\xf2\x00\x11\x00\x65\x00\x12\x00\x08\x00\x09\x00\x10\x00\xf7\x00\x11\x00\xec\x00\x12\x00\xa2\x00\x26\x00\xbd\x00\x21\x00\xc5\x00\xc6\x00\xc7\x00\x4f\x00\x21\x00\x32\x00\xc8\x00\xc9\x00\x4f\x00\xab\x00\x26\x00\x12\x00\x05\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x74\x00\x50\x00\x51\x00\x83\x00\x26\x00\xe2\x00\xa3\x00\xc7\x00\xd4\x00\x75\x00\xed\x00\xc8\x00\xc9\x00\x3d\x00\xba\x00\xbb\x00\x12\x00\x05\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x3d\x00\x64\x00\x65\x00\x66\x00\x21\x00\x1b\x00\x75\x00\x3e\x00\x4f\x00\x20\x00\x1c\x00\x1d\x00\xd5\x00\x70\x00\xbf\x00\x3f\x00\x40\x00\x32\x00\x32\x00\x3e\x00\x67\x00\x80\x00\x81\x00\x1e\x00\x1f\x00\xc4\x00\x89\x00\x3f\x00\x40\x00\x33\x00\x34\x00\x35\x00\x97\x00\xb4\x00\x20\x00\x68\x00\x69\x00\xb5\x00\x21\x00\x05\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\xcb\x00\xcc\x00\xcd\x00\xce\x00\xb7\x00\x1b\x00\x19\x00\x1a\x00\xb8\x00\xb9\x00\x1c\x00\x1d\x00\xcb\x00\xcc\x00\xcd\x00\xce\x00\x75\x00\x36\x00\x19\x00\x1a\x00\xc8\xff\x17\x00\x92\x00\x1e\x00\x1f\x00\xe7\x00\xe8\x00\xe9\x00\xbd\x00\xbc\x00\x17\x00\x38\x00\x19\x00\x1a\x00\x20\x00\x39\x00\x3a\x00\x17\x00\x21\x00\x19\x00\x1a\x00\x9f\x00\xcf\x00\x2c\x00\x9d\x00\xe4\x00\x21\x00\xa2\x00\x2d\x00\x2e\x00\x2c\x00\xa6\x00\x93\x00\xab\x00\xcf\x00\x2d\x00\x2e\x00\x7d\x00\x21\x00\x3b\x00\xb0\x00\xb3\x00\x2f\x00\x30\x00\x17\x00\xb1\x00\x19\x00\x1a\x00\x31\x00\x2f\x00\x30\x00\x74\x00\x32\x00\x5e\x00\x79\x00\x31\x00\x7d\x00\x2c\x00\x7e\x00\x32\x00\x48\x00\x7f\x00\x2d\x00\x2e\x00\x17\x00\x82\x00\x19\x00\x1a\x00\x8d\x00\x41\x00\x7d\x00\x8f\x00\x17\x00\x4d\x00\x19\x00\x1a\x00\x2f\x00\x30\x00\x17\x00\x90\x00\x19\x00\x1a\x00\x31\x00\x4e\x00\x4f\x00\x2c\x00\x32\x00\x91\x00\x95\x00\x17\x00\x2d\x00\x2e\x00\x97\x00\x59\x00\x9a\x00\x65\x00\x66\x00\x42\x00\x19\x00\x5a\x00\x5b\x00\x45\x00\x55\x00\xff\xff\x2f\x00\x30\x00\x32\x00\x5d\x00\x5c\x00\x5e\x00\x31\x00\xd1\x00\x17\x00\x67\x00\x32\x00\x21\x00\x55\x00\x08\x00\x22\x00\x23\x00\x32\x00\x24\x00\x71\x00\x05\x00\x6c\x00\x6d\x00\x00\x00\x00\x00\x68\x00\x69\x00\xff\xff\x25\x00\x26\x00\x88\x00\x00\x00\x4a\x00\x4b\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x21\x00\x00\x00\x08\x00\x22\x00\x23\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x00\x26\x00\x49\x00\x00\x00\x4a\x00\x4b\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x21\x00\x00\x00\x08\x00\x22\x00\x23\x00\x21\x00\x24\x00\x08\x00\x22\x00\x23\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x00\x26\x00\x01\x01\x00\x00\x00\x00\x25\x00\x26\x00\xed\x00\x28\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x21\x00\x00\x00\x08\x00\x22\x00\x23\x00\x21\x00\x24\x00\x08\x00\x22\x00\x23\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x00\x26\x00\xee\x00\x00\x00\x00\x00\x25\x00\x26\x00\xf5\x00\x28\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x21\x00\x00\x00\x08\x00\x22\x00\x23\x00\x21\x00\x24\x00\x08\x00\x22\x00\x23\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x00\x26\x00\xb5\x00\x00\x00\x00\x00\x25\x00\x26\x00\x9f\x00\x28\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x21\x00\x00\x00\x08\x00\x22\x00\x23\x00\x21\x00\x24\x00\x08\x00\x22\x00\x23\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x00\x26\x00\xa0\x00\x00\x00\x00\x00\x25\x00\x26\x00\xa4\x00\x28\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x21\x00\x00\x00\x08\x00\x22\x00\x23\x00\x21\x00\x24\x00\x08\x00\x22\x00\x23\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x00\x26\x00\xa6\x00\x00\x00\x00\x00\x25\x00\x26\x00\xad\x00\x28\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x21\x00\x00\x00\x08\x00\x22\x00\x23\x00\x21\x00\x24\x00\x08\x00\x22\x00\x23\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x00\x26\x00\x71\x00\x00\x00\x00\x00\x25\x00\x26\x00\x72\x00\x28\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x21\x00\x00\x00\x08\x00\x22\x00\x23\x00\x21\x00\x24\x00\x08\x00\x22\x00\x23\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x00\x26\x00\x82\x00\x00\x00\x00\x00\x25\x00\x26\x00\x87\x00\x28\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x21\x00\x00\x00\x08\x00\x22\x00\x23\x00\x21\x00\x24\x00\x08\x00\x22\x00\x23\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x00\x26\x00\x95\x00\x00\x00\x00\x00\x25\x00\x26\x00\x46\x00\x28\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x21\x00\x00\x00\x08\x00\x22\x00\x23\x00\x21\x00\x24\x00\x08\x00\x22\x00\x23\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x00\x26\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x85\x00\x29\x00\x2a\x00\x21\x00\x00\x00\x08\x00\x22\x00\x23\x00\x21\x00\x24\x00\x08\x00\x22\x00\x23\x00\x21\x00\x24\x00\x08\x00\x22\x00\x23\x00\x21\x00\x24\x00\x08\x00\x22\x00\x23\x00\x00\x00\x24\x00\x00\x00\x00\x00\x86\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x53\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x55\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x56\x00\x29\x00\x2a\x00\x21\x00\x00\x00\x08\x00\x22\x00\x23\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x57\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (3, 139) [
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
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
	(139 , happyReduce_139)
	]

happy_n_terms = 52 :: Int
happy_n_nonterms = 52 :: Int

happyReduce_3 = happySpecReduce_1  0# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (locate (getLoc happy_var_1) $ LitExp $ IntegerLit (getINTEGER happy_var_1)
	)}

happyReduce_4 = happySpecReduce_1  0# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (locate (getLoc happy_var_1) $ LitExp $ FloatLit (getFLOAT happy_var_1)
	)}

happyReduce_5 = happySpecReduce_1  0# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (locate (getLoc happy_var_1) $ LitExp $ CharLit (getCHAR happy_var_1)
	)}

happyReduce_6 = happySpecReduce_1  0# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (locate (getLoc happy_var_1) $ LitExp $ StringLit (getSTRING happy_var_1)
	)}

happyReduce_7 = happySpecReduce_1  1# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn7
		 (locname (getLoc happy_var_1) (getVARID happy_var_1)
	)}

happyReduce_8 = happySpecReduce_1  2# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (happy_var_1
	)}

happyReduce_9 = happySpecReduce_1  2# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (locqname (getLoc happy_var_1) (getQVARID happy_var_1)
	)}

happyReduce_10 = happySpecReduce_1  3# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (locname (getLoc happy_var_1) (getCONID happy_var_1)
	)}

happyReduce_11 = happySpecReduce_1  4# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (happy_var_1
	)}

happyReduce_12 = happySpecReduce_1  4# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (locqname (getLoc happy_var_1) (getQCONID happy_var_1)
	)}

happyReduce_13 = happySpecReduce_1  5# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (happy_var_1
	)}

happyReduce_14 = happySpecReduce_1  6# happyReduction_14
happyReduction_14 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (reloc TyCon happy_var_1
	)}

happyReduce_15 = happySpecReduce_1  7# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (reloc TyCon happy_var_1
	)}

happyReduce_16 = happySpecReduce_1  8# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn14
		 (locname (getLoc happy_var_1) (getVARSYM happy_var_1)
	)}

happyReduce_17 = happySpecReduce_1  8# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn14
		 (locname (getLoc happy_var_1) "!"
	)}

happyReduce_18 = happySpecReduce_1  8# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn14
		 (locname (getLoc happy_var_1) "*"
	)}

happyReduce_19 = happySpecReduce_1  8# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn14
		 (locname (getLoc happy_var_1) "."
	)}

happyReduce_20 = happySpecReduce_1  9# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (happy_var_1
	)}

happyReduce_21 = happySpecReduce_1  9# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (locqname (getLoc happy_var_1) (getQVARSYM happy_var_1)
	)}

happyReduce_22 = happySpecReduce_1  10# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (locname (getLoc happy_var_1) (getCONSYM happy_var_1)
	)}

happyReduce_23 = happySpecReduce_1  11# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (happy_var_1
	)}

happyReduce_24 = happySpecReduce_1  11# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (locqname (getLoc happy_var_1) (getQCONSYM happy_var_1)
	)}

happyReduce_25 = happySpecReduce_1  12# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 (reverse happy_var_1
	)}

happyReduce_26 = happySpecReduce_1  13# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ([happy_var_1]
	)}

happyReduce_27 = happySpecReduce_2  13# happyReduction_27
happyReduction_27 happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (happy_var_2 : happy_var_1
	)}}

happyReduce_28 = happyReduce 4# 14# happyReduction_28
happyReduction_28 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	case happyOut23 happy_x_4 of { happy_var_4 -> 
	happyIn20
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
         in
           locate loc $
           DataDecl (unLoc happy_var_2) (unLoc happy_var_4) []
	) `HappyStk` happyRest}}}

happyReduce_29 = happyReduce 8# 14# happyReduction_29
happyReduction_29 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { happy_var_2 -> 
	case happyOut23 happy_x_4 of { happy_var_4 -> 
	case happyOut35 happy_x_7 of { happy_var_7 -> 
	case happyOutTok happy_x_8 of { happy_var_8 -> 
	happyIn20
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_8]
         in
           locate loc $
           DataDecl (unLoc happy_var_2) (unLoc happy_var_4) (reverse $ map unLoc happy_var_7)
	) `HappyStk` happyRest}}}}}

happyReduce_30 = happyReduce 4# 14# happyReduction_30
happyReduction_30 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
         in
           locate loc $
           LetDecl (NonRec $ unLoc happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_31 = happyReduce 4# 14# happyReduction_31
happyReduction_31 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn20
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
         in
           locate loc $
           LetDecl (Rec $ reverse $ map unLoc happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_32 = happyReduce 4# 14# happyReduction_32
happyReduction_32 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn20
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
         in
           locate loc $
           SigDecl (unLoc happy_var_1) (unLoc happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_33 = happySpecReduce_0  15# happyReduction_33
happyReduction_33  =  happyIn21
		 ([]
	)

happyReduce_34 = happySpecReduce_1  15# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 ([happy_var_1]
	)}

happyReduce_35 = happySpecReduce_3  15# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_36 = happyReduce 5# 16# happyReduction_36
happyReduction_36 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	case happyOut38 happy_x_5 of { happy_var_5 -> 
	happyIn22
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_5]
         in
           locate loc $
           Binding (unLoc happy_var_1) (unLoc happy_var_3) defaultBindInfo (unLoc happy_var_5)
	) `HappyStk` happyRest}}}

happyReduce_37 = happySpecReduce_1  17# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (happy_var_1
	)}

happyReduce_38 = happySpecReduce_3  17# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (let loc = getLoc happy_var_1 <--> getLoc happy_var_3
         in
           L loc $ unLoc happy_var_1 :=> unLoc happy_var_3
	)}}

happyReduce_39 = happySpecReduce_1  18# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (L (getLoc happy_var_1) (:*)
	)}

happyReduce_40 = happyReduce 5# 18# happyReduction_40
happyReduction_40 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut25 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn24
		 (let loc = getLoc happy_var_1 <--> getLoc happy_var_5
         in
           L loc $
           (unLoc happy_var_2) :~ (unLoc happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_41 = happySpecReduce_3  18# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (happy_var_2
	)}

happyReduce_42 = happySpecReduce_1  19# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (happy_var_1
	)}

happyReduce_43 = happySpecReduce_3  19# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 (let loc = getLoc happy_var_1 <--> getLoc happy_var_3
        in
          locate loc $
          AppTy  (AppTy  (TyConTy (TyCon builtinArrow) (SrcLoc loc))
                         (unLoc happy_var_1)
                         (SrcLoc loc))
                 (unLoc happy_var_3)
	)}}

happyReduce_44 = happyReduce 4# 19# happyReduction_44
happyReduction_44 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	case happyOut25 happy_x_4 of { happy_var_4 -> 
	happyIn25
		 (let  {  loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
             ;  f (tv, k) ty = ForAll tv k ty (SrcLoc (getLoc ty))
             }
        in
          L loc $
          foldr f (unLoc happy_var_4) (reverse $ map unLoc happy_var_2)
	) `HappyStk` happyRest}}}

happyReduce_45 = happySpecReduce_3  19# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
        in
          locate loc $
          TransCo (unLoc happy_var_1) (unLoc happy_var_3)
	)}}

happyReduce_46 = happySpecReduce_3  19# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
        in
          locate loc $
          AppCo (unLoc happy_var_1) (unLoc happy_var_3)
	)}}

happyReduce_47 = happySpecReduce_2  19# happyReduction_47
happyReduction_47 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
        in
          locate loc $
          SymCo (unLoc happy_var_2)
	)}}

happyReduce_48 = happySpecReduce_2  19# happyReduction_48
happyReduction_48 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
        in
          locate loc $
          LeftCo (unLoc happy_var_2)
	)}}

happyReduce_49 = happySpecReduce_2  19# happyReduction_49
happyReduction_49 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
        in
          locate loc $
          RightCo (unLoc happy_var_2)
	)}}

happyReduce_50 = happySpecReduce_3  19# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
        in
          locate loc $
          ForAll WildTyVar (unLoc happy_var_1) (unLoc happy_var_3)
	)}}

happyReduce_51 = happySpecReduce_0  20# happyReduction_51
happyReduction_51  =  happyIn26
		 ([]
	)

happyReduce_52 = happySpecReduce_1  20# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 ([happy_var_1]
	)}

happyReduce_53 = happySpecReduce_1  20# happyReduction_53
happyReduction_53 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (reverse happy_var_1
	)}

happyReduce_54 = happySpecReduce_1  21# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 ([happy_var_1]
	)}

happyReduce_55 = happySpecReduce_3  21# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_56 = happySpecReduce_3  22# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_57 = happySpecReduce_1  23# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (L (getLoc happy_var_1) $ (TameTyVar $ TyVar (unLoc happy_var_1), (:*))
	)}

happyReduce_58 = happySpecReduce_3  23# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn29
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
         in
           L loc $ (TameTyVar $ TyVar $ (unLoc happy_var_1), unLoc happy_var_3)
	)}}

happyReduce_59 = happySpecReduce_3  23# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_2 of { happy_var_2 -> 
	happyIn29
		 (happy_var_2
	)}

happyReduce_60 = happySpecReduce_0  24# happyReduction_60
happyReduction_60  =  happyIn30
		 ([]
	)

happyReduce_61 = happySpecReduce_1  24# happyReduction_61
happyReduction_61 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ([happy_var_1]
	)}

happyReduce_62 = happySpecReduce_3  24# happyReduction_62
happyReduction_62 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_3 of { happy_var_3 -> 
	happyIn30
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_63 = happySpecReduce_1  25# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (happy_var_1
	)}

happyReduce_64 = happySpecReduce_2  25# happyReduction_64
happyReduction_64 happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
        in
          locate loc $ AppTy (unLoc happy_var_1) (unLoc happy_var_2)
	)}}

happyReduce_65 = happySpecReduce_1  26# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (happy_var_1
	)}

happyReduce_66 = happySpecReduce_1  26# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (let loc = getLoc happy_var_1
        in
          locate loc $ TyVarTy (TyVar (unLoc happy_var_1))
	)}

happyReduce_67 = happySpecReduce_3  26# happyReduction_67
happyReduction_67 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (let  {  loc  = getLoc happy_var_1 <--> getLoc happy_var_3
              ;  tys  = reverse (map unLoc happy_var_2)
              }
         in
           L loc $
           appsT (TyConTy (TupleTyCon (length tys)) (SrcLoc loc)) tys
	)}}}

happyReduce_68 = happySpecReduce_3  26# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn32
		 (let loc = getLoc happy_var_1 <--> getLoc happy_var_3
         in
           L loc $
           AppTy (TyConTy (TyCon builtinNil) (SrcLoc loc)) (unLoc happy_var_2) (SrcLoc loc)
	)}}}

happyReduce_69 = happySpecReduce_3  26# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn32
		 (happy_var_2
	)}

happyReduce_70 = happySpecReduce_1  27# happyReduction_70
happyReduction_70 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 (let loc = getLoc happy_var_1
        in
          locate loc $
          TyConTy (unLoc happy_var_1)
	)}

happyReduce_71 = happySpecReduce_2  27# happyReduction_71
happyReduction_71 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn33
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
        in
          locate loc $
          TyConTy (TupleTyCon 0)
	)}}

happyReduce_72 = happySpecReduce_2  27# happyReduction_72
happyReduction_72 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn33
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
        in
          locate loc $
          TyConTy (TyCon builtinNil)
	)}}

happyReduce_73 = happySpecReduce_3  27# happyReduction_73
happyReduction_73 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	case happyOutTok happy_x_3 of { happy_var_3 -> 
	happyIn33
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
        in
          locate loc $
          TyConTy (TyCon (name (getLoc happy_var_2) "->"))
	)}}}

happyReduce_74 = happyReduce 4# 27# happyReduction_74
happyReduction_74 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn33
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
        in
          locate loc $
          TyConTy (TupleTyCon (2 + happy_var_3))
	) `HappyStk` happyRest}}}

happyReduce_75 = happySpecReduce_0  28# happyReduction_75
happyReduction_75  =  happyIn34
		 (0
	)

happyReduce_76 = happySpecReduce_2  28# happyReduction_76
happyReduction_76 happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_2 of { happy_var_2 -> 
	happyIn34
		 (1 + happy_var_2
	)}

happyReduce_77 = happySpecReduce_1  29# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 ([happy_var_1]
	)}

happyReduce_78 = happySpecReduce_3  29# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_3 of { happy_var_3 -> 
	happyIn35
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_79 = happySpecReduce_3  30# happyReduction_79
happyReduction_79 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
         in
           locate loc $ ConDecl (unLoc happy_var_1) (unLoc happy_var_3) []
	)}}

happyReduce_80 = happyReduce 6# 30# happyReduction_80
happyReduction_80 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	case happyOut25 happy_x_6 of { happy_var_6 -> 
	happyIn36
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_6]
         in
           locate loc $
           ConDecl  (unLoc happy_var_1) (unLoc happy_var_6)
                    (reverse $ map unLoc happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_81 = happySpecReduce_0  31# happyReduction_81
happyReduction_81  =  happyIn37
		 ([]
	)

happyReduce_82 = happySpecReduce_1  31# happyReduction_82
happyReduction_82 happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 ([happy_var_1]
	)}

happyReduce_83 = happySpecReduce_3  31# happyReduction_83
happyReduction_83 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_84 = happySpecReduce_3  32# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
        in
          locate loc $
          CastExp (unLoc happy_var_1) (unLoc happy_var_3)
	)}}

happyReduce_85 = happySpecReduce_1  32# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn38
		 (happy_var_1
	)}

happyReduce_86 = happySpecReduce_1  33# happyReduction_86
happyReduction_86 happy_x_1
	 =  case happyOut40 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (happy_var_1
	)}

happyReduce_87 = happySpecReduce_1  33# happyReduction_87
happyReduction_87 happy_x_1
	 =  case happyOut41 happy_x_1 of { happy_var_1 -> 
	happyIn39
		 (happy_var_1
	)}

happyReduce_88 = happyReduce 6# 34# happyReduction_88
happyReduction_88 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_2 of { happy_var_2 -> 
	case happyOut25 happy_x_4 of { happy_var_4 -> 
	case happyOut38 happy_x_6 of { happy_var_6 -> 
	happyIn40
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
        in
          locate loc $
          LamExp (unLoc happy_var_2) (unLoc happy_var_4) (unLoc happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_89 = happyReduce 4# 34# happyReduction_89
happyReduction_89 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_2 of { happy_var_2 -> 
	case happyOut38 happy_x_4 of { happy_var_4 -> 
	happyIn40
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
        in
          locate loc $
          TyLamExp (TyVar $ (unLoc happy_var_2)) (:*) (unLoc happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_90 = happyReduce 6# 34# happyReduction_90
happyReduction_90 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_2 of { happy_var_2 -> 
	case happyOut23 happy_x_4 of { happy_var_4 -> 
	case happyOut38 happy_x_6 of { happy_var_6 -> 
	happyIn40
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_6]
        in
          locate loc $
          TyLamExp (TyVar $ (unLoc happy_var_2)) (unLoc happy_var_4) (unLoc happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_91 = happyReduce 6# 34# happyReduction_91
happyReduction_91 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	case happyOut38 happy_x_6 of { happy_var_6 -> 
	happyIn40
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_6]
        in
          locate loc $
          LetExp (NonRec $ unLoc happy_var_3) (unLoc happy_var_6)
	) `HappyStk` happyRest}}}

happyReduce_92 = happyReduce 6# 34# happyReduction_92
happyReduction_92 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	case happyOut38 happy_x_6 of { happy_var_6 -> 
	happyIn40
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_6]
        in
          locate loc $
          LetExp (Rec $ reverse $ map unLoc happy_var_3) (unLoc happy_var_6)
	) `HappyStk` happyRest}}}

happyReduce_93 = happyReduce 7# 35# happyReduction_93
happyReduction_93 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_2 of { happy_var_2 -> 
	case happyOut53 happy_x_4 of { happy_var_4 -> 
	case happyOut46 happy_x_6 of { happy_var_6 -> 
	case happyOutTok happy_x_7 of { happy_var_7 -> 
	happyIn41
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_7]
        in
          locate loc $
          CaseExp (unLoc happy_var_2) (unLoc happy_var_4) (reverse $ map unLoc happy_var_6)
	) `HappyStk` happyRest}}}}}

happyReduce_94 = happySpecReduce_1  35# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	happyIn41
		 (happy_var_1
	)}

happyReduce_95 = happySpecReduce_1  36# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn42
		 (happy_var_1
	)}

happyReduce_96 = happyReduce 4# 36# happyReduction_96
happyReduction_96 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn42
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
         in
           locate loc $ TyAppExp (unLoc happy_var_1) (unLoc happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_97 = happySpecReduce_2  36# happyReduction_97
happyReduction_97 happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn42
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
         in
           locate loc $ AppExp (unLoc happy_var_1) (unLoc happy_var_2)
	)}}

happyReduce_98 = happySpecReduce_1  37# happyReduction_98
happyReduction_98 happy_x_1
	 =  case happyOut54 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (locate (getLoc happy_var_1) $ VarExp (unLoc happy_var_1)
	)}

happyReduce_99 = happySpecReduce_1  37# happyReduction_99
happyReduction_99 happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (locate (getLoc happy_var_1) $ ConExp (unLoc happy_var_1)
	)}

happyReduce_100 = happySpecReduce_1  37# happyReduction_100
happyReduction_100 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (happy_var_1
	)}

happyReduce_101 = happySpecReduce_3  37# happyReduction_101
happyReduction_101 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_2 of { happy_var_2 -> 
	happyIn43
		 (happy_var_2
	)}

happyReduce_102 = happyReduce 5# 37# happyReduction_102
happyReduction_102 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_2 of { happy_var_2 -> 
	case happyOut44 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn43
		 (let  {  loc     = combineLocs [getLoc happy_var_1, getLoc happy_var_5]
             ;  exps    = unLoc happy_var_2 : reverse (map unLoc happy_var_4)
             ;  conExp  = ConExp (TupleCon (length exps)) (fromLoc loc)
             }
        in
          L loc $
          appsE conExp exps
	) `HappyStk` happyRest}}}}

happyReduce_103 = happySpecReduce_1  38# happyReduction_103
happyReduction_103 happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 ([happy_var_1]
	)}

happyReduce_104 = happySpecReduce_3  38# happyReduction_104
happyReduction_104 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn44
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_105 = happySpecReduce_3  39# happyReduction_105
happyReduction_105 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn45
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_3]
         in
           L loc $ Alt (unLoc happy_var_1) (unLoc happy_var_3)
	)}}

happyReduce_106 = happySpecReduce_1  40# happyReduction_106
happyReduction_106 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 ([happy_var_1]
	)}

happyReduce_107 = happySpecReduce_3  40# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	happyIn46
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_108 = happySpecReduce_3  40# happyReduction_108
happyReduction_108 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (happy_var_1
	)}

happyReduce_109 = happyReduce 5# 41# happyReduction_109
happyReduction_109 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut25 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn47
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_5]
         in
           L loc $ (TameVar $ Var (unLoc happy_var_2), unLoc happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_110 = happyReduce 5# 41# happyReduction_110
happyReduction_110 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn47
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_5]
         in
           L loc $ (WildVar, unLoc happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_111 = happySpecReduce_0  42# happyReduction_111
happyReduction_111  =  happyIn48
		 ([]
	)

happyReduce_112 = happySpecReduce_1  42# happyReduction_112
happyReduction_112 happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	happyIn48
		 ([happy_var_1]
	)}

happyReduce_113 = happySpecReduce_2  42# happyReduction_113
happyReduction_113 happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_2 of { happy_var_2 -> 
	happyIn48
		 (happy_var_2 : happy_var_1
	)}}

happyReduce_114 = happyReduce 5# 43# happyReduction_114
happyReduction_114 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut23 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn49
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_5]
         in
           L loc $ (TameTyVar $ TyVar (unLoc happy_var_2), unLoc happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_115 = happyReduce 5# 43# happyReduction_115
happyReduction_115 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_4 of { happy_var_4 -> 
	case happyOutTok happy_x_5 of { happy_var_5 -> 
	happyIn49
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_5]
         in
           L loc $ (WildTyVar, unLoc happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_116 = happySpecReduce_0  44# happyReduction_116
happyReduction_116  =  happyIn50
		 ([]
	)

happyReduce_117 = happySpecReduce_1  44# happyReduction_117
happyReduction_117 happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn50
		 ([happy_var_1]
	)}

happyReduce_118 = happySpecReduce_2  44# happyReduction_118
happyReduction_118 happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { happy_var_1 -> 
	case happyOut49 happy_x_2 of { happy_var_2 -> 
	happyIn50
		 (happy_var_2 : happy_var_1
	)}}

happyReduce_119 = happySpecReduce_1  45# happyReduction_119
happyReduction_119 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn51
		 (locate (getLoc happy_var_1) $ LitPat $ IntegerLit (getINTEGER happy_var_1)
	)}

happyReduce_120 = happySpecReduce_1  45# happyReduction_120
happyReduction_120 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn51
		 (locate (getLoc happy_var_1) $ LitPat $ FloatLit (getFLOAT happy_var_1)
	)}

happyReduce_121 = happySpecReduce_1  45# happyReduction_121
happyReduction_121 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn51
		 (locate (getLoc happy_var_1) $ LitPat $ CharLit (getCHAR happy_var_1)
	)}

happyReduce_122 = happySpecReduce_1  45# happyReduction_122
happyReduction_122 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn51
		 (locate (getLoc happy_var_1) $ LitPat $ StringLit (getSTRING happy_var_1)
	)}

happyReduce_123 = happySpecReduce_1  45# happyReduction_123
happyReduction_123 happy_x_1
	 =  case happyOut47 happy_x_1 of { happy_var_1 -> 
	happyIn51
		 (locate (getLoc happy_var_1) $ VarPat (unLoc happy_var_1)
	)}

happyReduce_124 = happySpecReduce_3  45# happyReduction_124
happyReduction_124 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_2 of { happy_var_2 -> 
	case happyOut48 happy_x_3 of { happy_var_3 -> 
	happyIn51
		 (let loc = combineLocs (getLoc happy_var_1 : map getLoc happy_var_3)
         in
           locate loc $ ConPat  (unLoc happy_var_1)
                                (reverse $ map unLoc happy_var_2)
                                (reverse $ map unLoc happy_var_3)
	)}}}

happyReduce_125 = happySpecReduce_2  46# happyReduction_125
happyReduction_125 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn52
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
        in
          L loc $ TupleCon 0
	)}}

happyReduce_126 = happySpecReduce_2  46# happyReduction_126
happyReduction_126 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn52
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_2]
        in
          L loc $ Con builtinNil
	)}}

happyReduce_127 = happyReduce 4# 46# happyReduction_127
happyReduction_127 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_3 of { happy_var_3 -> 
	case happyOutTok happy_x_4 of { happy_var_4 -> 
	happyIn52
		 (let loc = combineLocs [getLoc happy_var_1, getLoc happy_var_4]
        in
          L loc $ TupleCon (2 + happy_var_3)
	) `HappyStk` happyRest}}}

happyReduce_128 = happySpecReduce_1  46# happyReduction_128
happyReduction_128 happy_x_1
	 =  case happyOut56 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 (happy_var_1
	)}

happyReduce_129 = happySpecReduce_1  47# happyReduction_129
happyReduction_129 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn53
		 (L (getLoc happy_var_1) $ Var (unLoc happy_var_1)
	)}

happyReduce_130 = happySpecReduce_3  47# happyReduction_130
happyReduction_130 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn53
		 (L (getLoc happy_var_2) $ Var (unLoc happy_var_2)
	)}

happyReduce_131 = happySpecReduce_1  48# happyReduction_131
happyReduction_131 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn54
		 (L (getLoc happy_var_1) $ Var (unLoc happy_var_1)
	)}

happyReduce_132 = happySpecReduce_3  48# happyReduction_132
happyReduction_132 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn54
		 (L (getLoc happy_var_2) $ Var $ unLoc happy_var_2
	)}

happyReduce_133 = happySpecReduce_3  48# happyReduction_133
happyReduction_133 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { happy_var_2 -> 
	happyIn54
		 (L (getLoc happy_var_2) $ Var $ qname (getLoc happy_var_2) (getQVARSYM happy_var_2)
	)}

happyReduce_134 = happySpecReduce_1  49# happyReduction_134
happyReduction_134 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn55
		 (L (getLoc happy_var_1) $ Con (unLoc happy_var_1)
	)}

happyReduce_135 = happySpecReduce_3  49# happyReduction_135
happyReduction_135 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn55
		 (L (getLoc happy_var_2) $ Con (unLoc happy_var_2)
	)}

happyReduce_136 = happySpecReduce_1  50# happyReduction_136
happyReduction_136 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (L (getLoc happy_var_1) $ Con (unLoc happy_var_1)
	)}

happyReduce_137 = happySpecReduce_3  50# happyReduction_137
happyReduction_137 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_2 of { happy_var_2 -> 
	happyIn56
		 (happy_var_2
	)}

happyReduce_138 = happySpecReduce_1  51# happyReduction_138
happyReduction_138 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (L (getLoc happy_var_1) $ Con (name (getLoc happy_var_1) ":")
	)}

happyReduce_139 = happySpecReduce_1  51# happyReduction_139
happyReduction_139 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (L (getLoc happy_var_1) $ Con (unLoc happy_var_1)
	)}

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	L _ Teof -> happyDoAction 51# tk action sts stk;
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
	L _ Taxiom -> cont 13#;
	L _ Tcase -> cont 14#;
	L _ Tdata -> cont 15#;
	L _ Timport -> cont 16#;
	L _ Tin -> cont 17#;
	L _ Tleft -> cont 18#;
	L _ Tlet -> cont 19#;
	L _ Tletrec -> cont 20#;
	L _ Tmodule -> cont 21#;
	L _ Tof -> cont 22#;
	L _ Tright -> cont 23#;
	L _ Tsym -> cont 24#;
	L _ Ttype -> cont 25#;
	L _ Twhere -> cont 26#;
	L _ Tunderscore -> cont 27#;
	L _ Tdot -> cont 28#;
	L _ Tcolon -> cont 29#;
	L _ Tdcolon -> cont 30#;
	L _ Tequal -> cont 31#;
	L _ Tlam -> cont 32#;
	L _ TLam -> cont 33#;
	L _ Trarrow -> cont 34#;
	L _ Tdarrow -> cont 35#;
	L _ Tbang -> cont 36#;
	L _ Tstar -> cont 37#;
	L _ Tforall -> cont 38#;
	L _ Tcosim -> cont 39#;
	L _ Tcotrans -> cont 40#;
	L _ Tcoapp -> cont 41#;
	L _ Tcast -> cont 42#;
	L _ Tlparen -> cont 43#;
	L _ Trparen -> cont 44#;
	L _ Tcomma -> cont 45#;
	L _ Tsemi -> cont 46#;
	L _ Tlbrack -> cont 47#;
	L _ Trbrack -> cont 48#;
	L _ Tlbrace -> cont 49#;
	L _ Trbrace -> cont 50#;
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

parseBody = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut18 x))

parseType = happySomeParser where
  happySomeParser = happyThen (happyParse 1#) (\x -> happyReturn (happyOut25 x))

parseExp = happySomeParser where
  happySomeParser = happyThen (happyParse 2#) (\x -> happyReturn (happyOut38 x))

happySeq = happyDontSeq


--
-- \subsection{Lexing}


lexer :: (MonadParser m) => (L Token -> m a) -> m a
lexer cont = do
    tok <- token
    cont tok
  where
    token :: (MonadParser m) => m (L Token)
    token = do
        ts <- getTokens
        case ts of
          []          ->  return (L internalLoc Teof)
          (t : rest)  ->  setTokens rest >> return t



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
