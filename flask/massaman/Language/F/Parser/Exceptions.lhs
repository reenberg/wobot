%if False
\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

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
-- Module      :  Language.F.Parser.Exceptions
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.F.Parser.Exceptions (
    SomeParserException(..),
    LexerError(..),
    ParserError(..),
    UnexpectedEOF(..),
    EmptyCharacterLiteral(..),
    IllegalCharacterLiteral(..),
    IllegalNumericLiteral(..),
    LayoutError(..),
    Unclosed(..),
    Expected(..),
    BadExp(..),
    BadPat(..)
  ) where

import Data.Typeable

import Control.Exception
import Text.PrettyPrint.Mainland
\end{code}
%endif

\section{The Exception Type}

\begin{code}
data SomeParserException  =   forall a . (Pretty a, Exception a)
                          =>  SomeParserException a
  deriving (Typeable)

instance Show SomeParserException where
    show (SomeParserException e) = show e

instance Pretty SomeParserException where
    ppr (SomeParserException e) = ppr e

instance Exception SomeParserException

parserToException :: (Pretty a, Exception a)
                  => a -> SomeException
parserToException = toException . SomeParserException

parserFromException :: (Pretty a, Exception a)
                    => SomeException -> Maybe a
parserFromException x = do
    SomeParserException a <- fromException x
    cast a

data LexerError = LexerError Doc
  deriving (Typeable)

instance Exception LexerError where
    toException   = parserToException
    fromException = parserFromException

instance Pretty LexerError where
    ppr (LexerError doc) = text "lexer error:" <> doc

data ParserError = ParserError
  deriving (Typeable)

instance Exception ParserError where
    toException   = parserToException
    fromException = parserFromException

instance Pretty ParserError where
    ppr ParserError = text "parse error"

data UnexpectedEOF = UnexpectedEOF
  deriving (Typeable)

instance Exception UnexpectedEOF where
    toException   = parserToException
    fromException = parserFromException

instance Pretty UnexpectedEOF where
    ppr UnexpectedEOF = text "unexpected end of file"

data EmptyCharacterLiteral = EmptyCharacterLiteral
  deriving (Typeable)

instance Exception EmptyCharacterLiteral where
    toException   = parserToException
    fromException = parserFromException

instance Pretty EmptyCharacterLiteral where
    ppr EmptyCharacterLiteral = text "empty character literal"

data IllegalCharacterLiteral = IllegalCharacterLiteral
  deriving (Typeable)

instance Exception IllegalCharacterLiteral where
    toException   = parserToException
    fromException = parserFromException

instance Pretty IllegalCharacterLiteral where
    ppr IllegalCharacterLiteral = text "illegal character literal"

data IllegalNumericLiteral = IllegalNumericLiteral
  deriving (Typeable)

instance Exception IllegalNumericLiteral where
    toException   = parserToException
    fromException = parserFromException

instance Pretty IllegalNumericLiteral where
    ppr IllegalNumericLiteral = text "illegal numeric literal"

data LayoutError = LayoutError
  deriving (Typeable)

instance Exception LayoutError where
    toException   = parserToException
    fromException = parserFromException

instance Pretty LayoutError where
    ppr LayoutError = text "parse error"

data Unclosed = Unclosed String
  deriving (Typeable)

instance Exception Unclosed where
    toException   = parserToException
    fromException = parserFromException

instance Pretty Unclosed where
    ppr (Unclosed s) = text $ "unclosed '" ++ s ++ "'"

data Expected = Expected [String]
  deriving (Typeable)

instance Exception Expected where
    toException   = parserToException
    fromException = parserFromException

instance Pretty Expected where
    ppr (Expected [alt]) = text $ "expected " ++ alt

    ppr (Expected alts) =
        text $ concat $ "expected " : go alts
      where
        go :: [String] -> [String]
        go []       = [""]
        go [s]      = [" or " ++ s]
        go (s : ss) = (", " ++ s) : go ss

data BadExp = BadExp
  deriving (Typeable)

instance Exception BadExp where
    toException   = parserToException
    fromException = parserFromException

instance Pretty BadExp where
    ppr BadExp = text "bad expression"

data BadPat = BadPat
  deriving (Typeable)

instance Exception BadPat where
    toException   = parserToException
    fromException = parserFromException

instance Pretty BadPat where
    ppr BadPat = text "bad pattern"
\end{code}
