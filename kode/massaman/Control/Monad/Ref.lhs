\section{The Ref Monad Class} \label{monad-ref}

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
-- Module      :  Control.Monad.Ref
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Ref where

import Control.Monad
import Control.Monad.ST
import Data.IORef
import Data.STRef
\end{code}

The {\tt MonadRef} type class abstracts over the details of manipulating
references, allowing one to write code that can operate in either the ST monad
or the IO monad. The cost of this generality is carrying around an extra class
parameter that specifies the type of the reference within the monad. Functional
dependencies are a must here as far as I can tell.

\begin{code}
class (Monad m) => MonadRef r m | m -> r where
    newRef    :: a -> m (r a)
    readRef   :: r a -> m a
    writeRef  :: r a -> a -> m ()
    modifyRef :: r a -> (a -> a) -> m ()
\end{code}

Instances of {\tt MonadRef} for the ST and IO monads are trivial to write.

\begin{code}
instance MonadRef (STRef s) (ST s) where
    newRef     = newSTRef
    readRef    = readSTRef
    writeRef   = writeSTRef
    modifyRef  = modifySTRef

instance MonadRef IORef IO where
    newRef     = newIORef
    readRef    = readIORef
    writeRef   = writeIORef
    modifyRef  = modifyIORef
\end{code}
