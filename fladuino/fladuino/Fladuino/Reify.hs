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
-- Module      :  Fladuino.Reify
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Fladuino.Reify where

import Data.Name
import qualified Language.Hs.Syntax as H

class Reify a where
    reify :: a -> H.Type

instance Reify () where
    reify _ = H.TyConTy (H.TupleTyCon 0)

instance Reify Integer where
    reify _ = H.TyConTy (H.TyCon prelInteger)

instance Reify Char where
    reify _ = H.TyConTy (H.TyCon prelChar)

instance Reify Bool where
    reify _ = H.TyConTy (H.TyCon prelBool)

instance Reify Float where
    reify _ = H.TyConTy (H.TyCon prelFloat)

instance forall a . Reify a => Reify (Maybe a) where
    reify _ = H.AppTy (H.TyConTy (H.TyCon tycon)) tya
      where
        tycon  = prelMaybe
        tya    = reify (undefined :: a)

instance forall a b . (Reify a, Reify b) => Reify (Either a b) where
    reify _ = H.AppTy  (H.AppTy (H.TyConTy (H.TyCon tycon)) tya) tyb
      where
        tycon  = prelEither
        tya    = reify (undefined :: a)
        tyb    = reify (undefined :: b)

instance forall a b . (Reify a, Reify b) => Reify (a, b) where
    reify _ = H.AppTy (H.AppTy (H.TyConTy (H.TupleTyCon 2)) tya) tyb
      where
        tya = reify (undefined :: a)
        tyb = reify (undefined :: b)

instance forall a b c . (Reify a, Reify b, Reify c) => Reify (a, b, c) where
    reify _ = H.AppTy (H.AppTy (H.AppTy (H.TyConTy (H.TupleTyCon 3)) tya) tyb) tyc
      where
        tya = reify (undefined :: a)
        tyb = reify (undefined :: b)
        tyc = reify (undefined :: c)

instance forall a b . (Reify a, Reify b) => Reify (a -> b) where
    reify _ = H.AppTy (H.AppTy (H.TyConTy (H.TyCon tycon)) ty1) ty2
      where
        tycon  = builtinArrow
        ty1    = reify (undefined :: a)
        ty2    = reify (undefined :: b)
