%if False
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
-- Module      :  Transform.F.Simplify.Metrics
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Transform.F.Simplify.Metrics where

import Data.Name
import Language.F
\end{code}
%endif

A trivial expression is an expression that does not duplicate work, i.e., it is
a literal, a variable, or a type abstraction, type application or cast where the
expression argument is trivial. Note that this definition depends on the
assumption that type abstraction/application/casts do not actually perform any
work.

\begin{code}
isTrivial :: Exp -> Bool
isTrivial  (TyLamExp _ _ e _)  = isTrivial e
isTrivial  (TyAppExp e _ _)    = isTrivial e
isTrivial  (LitExp _ _)        = True
isTrivial  (VarExp _ _)        = True
isTrivial  _                   = False
\end{code}

\begin{code}
isBot :: Var -> Bool
isBot (Var n)  | n == prelUndefined  = True
               | n == prelError      = True
               | otherwise           = False
\end{code}

\begin{code}
isWhnfOrBot :: Exp -> Bool
isWhnfOrBot  (TyLamExp _ _ e _)  = isWhnfOrBot e
isWhnfOrBot  (TyAppExp e _ _)    = isWhnfOrBot e
isWhnfOrBot  (LitExp _ _)        = True
isWhnfOrBot  (VarExp _ _)        = True
isWhnfOrBot  (LamExp _ _ _ _)    = True
isWhnfOrBot  e                   = case stripToApplicand e of
                                     ConExp _ _ ->  True
                                     VarExp v _ ->  isBot v
                                     _ ->           False
\end{code}

\begin{code}
loopBreakerScore :: Binding -> Int
loopBreakerScore (Binding _ _ bindinfo e _)
    | isTrivial e         = 4
    | isConstructorApp e  = 3
    | occinfo == Once     = 2
    | otherwise           = 0
  where
    occinfo = bind_occinfo bindinfo
\end{code}

\begin{code}
isConstructorApp :: Exp -> Bool
isConstructorApp e = case stripToApplicand e of
                       ConExp _ _ ->  True
                       _ ->           False
\end{code}

\begin{code}
stripTyExps :: Exp -> Exp
stripTyExps  (TyLamExp _ _ e _)  = stripTyExps e
stripTyExps  (TyAppExp e _ _)    = stripTyExps e
stripTyExps  e                   = e
\end{code}

\begin{code}
stripToApplicand :: Exp -> Exp
stripToApplicand  (TyLamExp _ _ e _)  = stripToApplicand e
stripToApplicand  (AppExp e _ _)      = stripToApplicand e
stripToApplicand  e                   = strip e
  where
    strip  (TyAppExp e _ _)    = strip e
    strip  e                   = e
\end{code}
