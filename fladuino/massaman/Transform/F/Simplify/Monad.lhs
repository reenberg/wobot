\begin{code}
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
-- Module      :  Transform.F.Simplify.Monad
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Transform.F.Simplify.Monad (
    SimplEnv,
    emptySimplEnv,
    MonadSimpl(..)
  ) where

import Check.F
import Language.F
\end{code}

\begin{code}
data SimplEnv = SimplEnv
    {  maxIterations    :: !Int
    ,  dropCount        :: !Int
    ,  preInlineCount   :: !Int
    ,  postInlineCount  :: !Int
    ,  inlineCount      :: !Int
    }
  deriving (Eq)

emptySimplEnv :: SimplEnv
emptySimplEnv = SimplEnv
    {  maxIterations    = 10
    ,  dropCount        = 0
    ,  preInlineCount   = 0
    ,  postInlineCount  = 0
    ,  inlineCount      = 0
    }
\end{code}

\begin{code}
class (MonadTc m)
    => MonadSimpl m where
    getSimplEnv   :: m SimplEnv
    putSimplEnv   :: SimplEnv -> m ()

    getsSimplEnv :: (SimplEnv -> a) -> m a
    getsSimplEnv f = getSimplEnv >>= \s -> return (f s)

    modifySimplEnv :: (SimplEnv -> SimplEnv) -> m ()
    modifySimplEnv f = getSimplEnv >>= \s -> putSimplEnv (f s)

    resetSimplEnv :: m ()
    resetSimplEnv = putSimplEnv emptySimplEnv

    savingSimplEnv :: m a -> m a
    savingSimplEnv m = do
        env  <- getSimplEnv
        a    <- m
        putSimplEnv env
        return a

    getMaxIterations :: m Int
    getMaxIterations = getsSimplEnv maxIterations

    setMaxIterations :: Int -> m ()
    setMaxIterations i =
        modifySimplEnv $ \s -> s { maxIterations = i }

    droppedBinding :: Binding -> m ()
    droppedBinding _ =
        modifySimplEnv $ \s -> s { dropCount = dropCount s + 1 }

    preInlinedBinding :: Binding -> m ()
    preInlinedBinding _ =
        modifySimplEnv $ \s -> s { preInlineCount = preInlineCount s + 1 }

    postInlinedBinding :: Binding -> m ()
    postInlinedBinding _ =
        modifySimplEnv $ \s -> s { postInlineCount = postInlineCount s + 1 }

    inlinedVar :: Var -> m ()
    inlinedVar _ =
        modifySimplEnv $ \s -> s { inlineCount = inlineCount s + 1 }
\end{code}
