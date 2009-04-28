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
-- Module      :  Language.Hs.Parser
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.Hs.Parser (
    module Language.Hs.Parser.Exceptions,
    module Language.Hs.Parser.Lexer,
    module Language.Hs.Parser.Monad,
    module Language.Hs.Parser.Parser,
    module Language.Hs.Parser.Tokens,
    module Language.Hs.Parser.Utils,
    parse
  ) where

import Prelude hiding (exp)

import Compiler.Opt
import Control.Exception
import qualified Data.ByteString.Char8 as B
import Data.Loc
import Language.Hs.Parser.Exceptions
import Language.Hs.Parser.Layout
import Language.Hs.Parser.Lexer
import Language.Hs.Parser.Monad
import Language.Hs.Parser.Parser
import Language.Hs.Parser.Tokens
import Language.Hs.Parser.Utils

parse  ::  B.ByteString
       ->  Pos
       ->  Bool
       ->  Opts
       ->  ParseContext
       ->  P a
       ->  Either SomeException a
parse buf start isModule opts ctx p =
    flip evalP (emptyPState opts sc ctx) $ do
    setOpts opts
    setParseContext ctx
    setBuffer buf
    setLastPos start
    setPos start
    ts <- tokens
    setTokenStack $  if isModule
                     then preprocessModule ts
                     else preprocess ts
    p
  where
    sc :: Int
    sc = case ctx of
           ParseDirect      -> 0
           ParseExpression  -> exp
           ParsePattern     -> pat

    tokens :: P [L Token]
    tokens = do
        t <- lexToken
        case t of
          L (Loc _ pos) Teof  -> setEofPos pos >> return []
          _                   -> tokens >>= \ts -> return (t : ts)
\end{code}
