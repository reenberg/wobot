{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

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
-- Module      :  Flask.Exceptions
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Flask.Exceptions where

import Control.Exception
import Data.Typeable

import Language.C.Syntax as C
import Language.C.Pretty ()
import Text.PrettyPrint.Mainland

data SomeFlaskException  =   forall a . (Pretty a, Exception a)
                          =>  SomeFlaskException a
  deriving (Typeable)

instance Show SomeFlaskException where
    show (SomeFlaskException e) = show e

instance Pretty SomeFlaskException where
    ppr (SomeFlaskException e) = ppr e

instance Exception SomeFlaskException

flaskToException :: (Pretty a, Exception a)
                  => a -> SomeException
flaskToException = toException . SomeFlaskException

flaskFromException :: (Pretty a, Exception a)
                    => SomeException -> Maybe a
flaskFromException x = do
    SomeFlaskException a <- fromException x
    cast a

data CTypeError = CTypeError C.Type C.Type
  deriving (Typeable)

instance Exception CTypeError where
    toException   = flaskToException
    fromException = flaskFromException

instance Pretty CTypeError where
    ppr (CTypeError tau1 tau2) =
        text "expected type" <+> ppr tau1 <+>
        text "but got type" <+> ppr tau2
