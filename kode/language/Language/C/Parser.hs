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
-- Module      :  Language.C.Parser
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.C.Parser (
    module Language.C.Parser.Exceptions,
    module Language.C.Parser.Lexer,
    module Language.C.Parser.Monad,
    module Language.C.Parser.Parser,
    module Language.C.Parser.Tokens,
    parse
  ) where

import Prelude hiding (exp)

import Control.Exception

import qualified Data.ByteString.Char8 as B
import Data.Loc
import Language.C.Parser.Exceptions
import Language.C.Parser.Lexer
import Language.C.Parser.Parser
import Language.C.Parser.Monad
import qualified Language.C.Parser.Monad
import Language.C.Parser.Tokens

parse :: ParseContext -> Bool -> Bool
      -> P a
      -> Pos
      -> B.ByteString
      -> Either SomeException a
parse ctx c99 gcc p start buf =
    flip evalP (emptyPState sc ctx c99 gcc) $ do
    setParseContext ctx
    setBuffer buf
    setLastPos start
    setPos start
    p
  where
    sc :: Int
    sc = case ctx of
           ParseDirect      -> 0
           ParseExpression  -> exp
           ParsePattern     -> pat
