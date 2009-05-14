{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

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
-- Module      :  Control.Monad.Exception
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Control.Monad.Exception where

import Prelude

import Control.Exception hiding (catch)
import Control.Monad.Error
import Data.Typeable

import Text.PrettyPrint.Mainland

class MonadError SomeException m => MonadException m where
    throwException :: (Exception e, Pretty e) => e -> m a
    throwException = throwError . toException

    catchException  ::  (Exception e, Pretty e)
                    =>  m a
                    ->  (e -> m a)
                    ->  m a
    catchException m h =
        m `catchError` \e@(SomeException e') ->
            case cast e of
              Just e''  -> h e''
              Nothing   -> case cast e' of
                             Just e''  -> h e''
                             Nothing   -> throw e'

instance Error SomeException where
    noMsg      = toException $ NoMsgException
    strMsg msg = toException $ StrMsgException msg

data NoMsgException = NoMsgException
  deriving (Typeable)

instance Pretty NoMsgException where
    ppr NoMsgException = empty

instance Exception NoMsgException

data StrMsgException = StrMsgException [Char]
  deriving (Typeable)

instance Pretty StrMsgException where
    ppr (StrMsgException msg) = text msg

instance Exception StrMsgException

data PanicException = PanicException Doc
  deriving (Typeable)

instance Pretty PanicException where
    ppr (PanicException doc) = text "panic:" <+> doc

instance Exception PanicException

panic :: (MonadException m) => Doc -> m a
panic = throwException . PanicException

data InternalErrorException = InternalErrorException Doc
  deriving (Typeable)

instance Pretty InternalErrorException where
    ppr (InternalErrorException doc) = text "internal error:" <+> doc

instance Exception InternalErrorException

internalErr :: Doc -> a
internalErr = throw . InternalErrorException

instance Pretty IOException where
    ppr = text . show

catchIO :: (MonadIO m, MonadException m) => IO a -> m a
catchIO m = do
    either_a <-  liftIO $ do
                     a <- m
                     return $ Left a
                 `catch` \e -> return $ Right e
    case either_a of
      Left a   -> return a
      Right e  -> throwException e
