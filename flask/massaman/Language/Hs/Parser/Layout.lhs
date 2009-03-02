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
-- Module      :  Language.Hs.Parser.Layout
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.Hs.Parser.Layout(
    preprocess,
    preprocessModule,
    lay
  ) where

import Control.Monad.ContextException
import Data.Loc
import Language.Hs.Parser.Exceptions
import Language.Hs.Parser.Monad
import Language.Hs.Parser.Tokens
\end{code}

\subsection{Layout}

\subsubsection{Adding indentation lexemes}

\begin{code}
indentation :: String -> Int
indentation s = index 0 '\n' (reverse s)
    where
      index :: Int -> Char -> String -> Int
      index  _  _  []  = 0
      index  i  x  (c:cs)
          | c == x     = i + 1
          | otherwise  = index (i + 1) x cs

addLbrace :: L Token -> L Token
addLbrace (L loc@(Loc (Pos _ _ col _) _) _)  = L loc (Timplbrace col)

compressWs :: [L Token] -> [L Token]
compressWs  []                                          = []
compressWs  (L loc1 (Tws s1) : L loc2 (Tws s2) : rest)  =
    compressWs $ L loc' (Tws (s1 ++ s2)) : rest
  where
    loc' = combineLocs [loc1, loc2]
compressWs  (t : ts)                                    = t : compressWs ts

preprocessModule :: [L Token] -> [L Token]
preprocessModule []                    = []
preprocessModule (L _ (Tws _) : rest)  = preprocessModule rest
preprocessModule ts@(L _ Tlbrace : _)  = preprocess ts
preprocessModule ts@(L _ Tmodule : _)  = preprocess ts
preprocessModule (t : rest)            = addLbrace t : preprocess  (t : rest)

preprocess :: [L Token] -> [L Token]
preprocess = pre . compressWs
  where
    pre [] = []
    pre (kw@(L loc t) : rest)
        | isLetWhereDoOf t =
      case kw : rest of
        []                                              -> error "can't happen"
        kw : []                                         -> kw : L loc (Timplbrace 0) : []
        kw : ws@(L _ (Tws _)) : []                      -> kw : ws : L loc (Timplbrace 0) : []
        kw : ws@(L _ (Tws _)) : l@(L _ Tlbrace) : rest  -> kw : ws : l : pre rest
        kw : ws@(L _ (Tws _)) : l : rest                -> kw : ws : addLbrace l : l : pre rest
        kw : l@(L _ Tlbrace) : rest                     -> kw : l : pre rest
        kw : l : rest                                   -> kw : addLbrace l : l : pre rest

    pre (ws@(L loc (Tws s)) : l : rest)
        | n == 0     = ws : pre (l : rest)
        | otherwise  = ws : L loc (Tindent n) : pre (l : rest)
      where
        n = indentation s

    pre (hd : tl) = hd : pre tl

    isLetWhereDoOf :: Token -> Bool
    isLetWhereDoOf Tlet    = True
    isLetWhereDoOf Twhere  = True
    isLetWhereDoOf Tdo     = True
    isLetWhereDoOf Tof     = True
    isLetWhereDoOf _       = False
\end{code}

\begin{code}
semiToken :: Loc -> L Token
semiToken loc = L loc Tsemi

lbraceToken :: Loc -> L Token
lbraceToken loc = L loc Tlbrace

rbraceToken :: Loc -> L Token
rbraceToken loc = L loc Trbrace
\end{code}

\renewcommand{\L}[2]{{\rm L} \;\; {#1} \;\; {#2}}
\newcommand{\ndent}[1]{<#1>}
\newcommand{\nbrack}[1]{\{#1\}}

\begin{code}
lay  ::  MonadParser m
     =>  ([L Token], [Int])
     ->  m ([L Token], [L Token], [Int])
\end{code}

\begin{align*}
\L{(\ndent{n}:ts)}{(m:ms)} &= {;} : (\L{ts}{(m:ms)})          &{\rm if} \; m = n \\
                           &= \}  : (\L{(\ndent{n}:ts)}{ms})  &{\rm if} \; n < m
\end{align*}

\begin{code}
lay (t@(L loc (Tindent n)) : ts, m : ms)
    |  m == n  = return ([semiToken loc], ts, m:ms)
    |  n < m   = return ([rbraceToken loc], t:ts, ms)
\end{code}

\begin{align*}
\L{(<n>:ts)}{ms} &= \L{ts}{ms}
\end{align*}

\begin{code}
lay (L _ (Tindent _) : ts, ms) = lay (ts, ms)
\end{code}

\begin{align*}
\L{(\nbrack{n}:ts)}{(m:ms)} = \{ : (\L{ts}{(n:m:ms)}) &\quad {\rm if} \; n > m
\end{align*}

\begin{code}
lay (L loc (Timplbrace n) : ts, m : ms)
    | n > m = return ([lbraceToken loc], ts, n:m:ms)
\end{code}

\begin{align*}
\L{(\nbrack{n}:ts)}{[\;]} = \{ : (\L{ts}{[n]}) &\quad {\rm if} \; n > 0
\end{align*}

\begin{code}
lay (L loc (Timplbrace n) : ts, [])
    | n > 0 = return ([lbraceToken loc], ts, [n])
\end{code}

\begin{align*}
\L{(\nbrack{n}:ts)}{ms} &= \{ : \} : (\L{(\ndent{n}:ts}{ms})
\end{align*}

\begin{code}
lay (L loc (Timplbrace n) : ts, ms)
    = return ([lbraceToken loc, rbraceToken loc], L loc (Tindent n) : ts, ms)
\end{code}

\begin{align*}
\L{(\}:ts)}{(0:ms)} &= \} : (\L{ts}{ms})
\end{align*}

\begin{code}
lay (t@(L _ Trbrace) : ts, 0:ms)
    = return ([t], ts, ms)
\end{code}

\begin{align*}
\L{(\}:ts)}{ms} &= \text{parse error}
\end{align*}

\begin{code}
lay (L loc Trbrace : _, _)
    = return ([L loc Timprbrace], [], [])
\end{code}

\begin{align*}
\L{(\{:ts)}{ms} &= \{ : \L{ts}{(0:ms)}
\end{align*}

\begin{code}
lay (t@(L _ Tlbrace) : ts, ms)
    = return ([t], ts, 0:ms)
\end{code}

\begin{align*}
\L{(t:ts)}{(m:ms)} &= \} : (\L{(t:ts)}{ms}) \quad {\rm if} \; m \ne 0 \; \text{and parse-error($t$)}
\end{align*}

\begin{code}
lay (t@(L loc _):ts, m:ms) =
    do  needRightBrace <- doesNeedRightBrace
        if m /= 0 && needRightBrace
          then return ([rbraceToken loc], t:ts, ms)
          else return ([t], ts, m:ms)
\end{code}

\begin{align*}
\L{(t:ts)}{ms} &= t : (\L{ts}{ms})
\end{align*}

\begin{code}
lay (t : ts, ms) = return ([t], ts, ms)
\end{code}

\begin{align*}
\L{[\;]}{[\;]} = [\;]
\end{align*}

\begin{code}
lay ([], []) = do  eof <- getEofPos
                   return ([L (Loc eof eof) Teof], [], [])
\end{code}

\begin{align*}
\L{[\;]}{(m:ms)} = \} : \L{[\;]}{ms} \quad {\rm if} \; m \ne 0
\end{align*}

\begin{code}
lay ([], m:ms)
    | m /= 0 = do  eof <- getEofPos
                   return ([rbraceToken (Loc eof eof)], [], ms)
\end{code}

\noindent
If none of the previous cases match, there has been a layout failure.

\begin{code}
lay ([], _) = do
    eof <- getEofPos
    throwExceptionAt eof LayoutError
\end{code}
