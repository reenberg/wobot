\begin{code}
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
-- Module      :  Control.Monad.Trace
-- Copyright   :  (c) Harvard University 2007-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Control.Monad.Trace where

import IO (hPutStrLn, stderr)
import Foreign (unsafePerformIO)

import Text.PrettyPrint.Mainland
\end{code}

\begin{code}
data TraceEnv = TraceEnv { depth :: Int }

emptyTraceEnv :: TraceEnv
emptyTraceEnv = TraceEnv { depth = 0 }

class Monad m => MonadTrace m where
    getTraceEnv  :: m TraceEnv
    putTraceEnv  :: TraceEnv -> m ()

    getsTraceEnv :: (TraceEnv -> a) -> m a
    getsTraceEnv f = getTraceEnv >>= \s -> return (f s)

    modifyTraceEnv :: (TraceEnv -> TraceEnv) -> m ()
    modifyTraceEnv f = getTraceEnv >>= \s -> putTraceEnv (f s)

    traceIndent :: m Int
    traceIndent = return 2

    traceDepth :: m Int
    traceDepth = getsTraceEnv depth

    setTraceDepth :: Int -> m ()
    setTraceDepth d = modifyTraceEnv (\s -> s { depth = d })

    trace :: String -> Doc -> m ()
    trace prefix doc = do
        d <- traceDepth
        i <- traceIndent
        let d'    = length prefix + 1 + d*i
        let doc'  = text (replicate (d*i) ' ') <> nest d' doc
        return $!
            unsafePerformIO $
            hPutStrLn stderr (pretty 80 (text prefix <+> doc'))

    traceNest :: m a -> m a
    traceNest m = do
        d <- traceDepth
        setTraceDepth (d + 1)
        a <- m
        setTraceDepth d
        return a
\end{code}
