\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
-- Module      :  Language.Hs.Parser.Monad
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.Hs.Parser.Monad where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.ByteString.Char8 as B

import Compiler.Opt
import Control.Monad.ContextException
import Control.Monad.Exception
import Data.Loc
import Language.Hs.Parser.Exceptions
import Language.Hs.Parser.Tokens
\end{code}

\begin{code}
data AlexInput = AlexInput {-#UNPACK#-} !Pos
                           {-#UNPACK#-} !B.ByteString
                           {-#UNPACK#-} !Int

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (AlexInput pos buf off)
    | off < B.length buf  =  c `seq` pos' `seq` off' `seq`
                             Just (c, AlexInput pos' buf off')
    | otherwise           =  Nothing
  where
    c     = B.index buf off
    pos'  = advancePos pos c
    off'  = off + 1

    advancePos :: Pos -> Char -> Pos
    advancePos (Pos f l _ coff)    '\n'  = Pos f  (l+1)  1        (coff + 1)
    advancePos (Pos f l c coff)    _     = Pos f  l      (c + 1)  (coff + 1)

alexGetCharOrFail :: MonadParser m => AlexInput -> m (Char, AlexInput)
alexGetCharOrFail inp@(AlexInput pos _ _) =
    case alexGetChar inp of
      Nothing          -> throwExceptionAt pos UnexpectedEOF
      Just (c, input)  -> return (c, input)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput  _  _    0)    = '\n'
alexInputPrevChar (AlexInput  _  buf  off)  = B.index buf (off - 1)
\end{code}

\begin{code}
data ParseContext  =  ParseDirect
                   |  ParseExpression
                   |  ParsePattern

data PEnv = PEnv
    {  buf             :: !B.ByteString  -- ^ Buffer we're parsing
    ,  off             :: !Int           -- ^ Current offset in the buffer
    ,  last_pos        :: Pos            -- ^ End position of last token parsed
    ,  pos             :: Pos            -- ^ Current source code position
    ,  lex_state       :: [Int]
    ,  context         :: !ParseContext
    ,  lastToken       :: L Token
    ,  tokens          :: [L Token]
    ,  tokenStack      :: [L Token]
    ,  needRightBrace  :: !Bool
    ,  layoutStack     :: [Int]
    ,  eofPos          :: Pos
    }

emptyPEnv :: Int -> ParseContext -> PEnv
emptyPEnv sc ctx = PEnv
    {  buf             = B.pack ""
    ,  off             = 0
    ,  last_pos        = error "No last position"
    ,  pos             = error "No current position"
    ,  lex_state       = [sc]
    ,  context         = ctx
    ,  lastToken       = error "no last token"
    ,  tokens          = []
    ,  tokenStack      = []
    ,  needRightBrace  = False
    ,  layoutStack     = []
    ,  eofPos          = error "no EOF position"
    }

class MonadContextException m => MonadParser m where
    getPEnv   :: m PEnv
    putPEnv   :: PEnv -> m ()

    getsPEnv :: (PEnv -> a) -> m a
    getsPEnv f = getPEnv >>= \s -> return (f s)

    modifyPEnv :: (PEnv -> PEnv) -> m ()
    modifyPEnv f = getPEnv >>= \s -> putPEnv (f s)

    getBuffer :: m B.ByteString
    getBuffer = getsPEnv buf

    setBuffer  :: B.ByteString -> m ()
    setBuffer buf = modifyPEnv $ \s ->
                        s { buf = buf }

    getLastPos  :: m Pos
    getLastPos = getsPEnv last_pos

    setLastPos  :: Pos -> m ()
    setLastPos pos = modifyPEnv $ \s ->
                         s { last_pos = pos }

    getPos :: m Pos
    getPos = getsPEnv pos

    setPos :: Pos -> m ()
    setPos pos = modifyPEnv $ \s ->
                     s { pos = pos }

    pushLexState :: Int -> m ()
    pushLexState ls = modifyPEnv $ \s ->
                          s { lex_state = ls : lex_state s }

    popLexState :: m Int
    popLexState = do
        ls <- getLexState
        modifyPEnv $ \s ->
            s { lex_state = tail (lex_state s) }
        return ls

    getLexState :: m Int
    getLexState = getsPEnv (head . lex_state)

    getParseContext :: m ParseContext
    getParseContext = getsPEnv context

    setParseContext :: ParseContext -> m ()
    setParseContext ctx = modifyPEnv $ \s ->
                              s { context = ctx }

    getInput  :: m AlexInput
    getInput = getsPEnv $ \s ->
                   AlexInput (pos s) (buf s) (off s)

    setInput  :: AlexInput -> m ()
    setInput (AlexInput p b o) = modifyPEnv $ \s ->
                                     s { buf = b, off = o, pos = p }

    getTokens :: m [L Token]
    getTokens = getsPEnv tokens

    setTokens :: [L Token] -> m ()
    setTokens ts = modifyPEnv $ \s ->
                       s { tokens = ts }

    getLastToken :: m (L Token)
    getLastToken = getsPEnv lastToken

    setLastToken :: L Token -> m ()
    setLastToken t = modifyPEnv $ \s ->
                         s{ lastToken = t }

    getTokenStack :: m [L Token]
    getTokenStack = getsPEnv tokenStack

    setTokenStack :: [L Token] -> m ()
    setTokenStack stack  = modifyPEnv $ \s ->
                               s { tokenStack = stack }

    doesNeedRightBrace :: m Bool
    doesNeedRightBrace = getsPEnv needRightBrace

    setNeedRightBrace :: Bool -> m ()
    setNeedRightBrace flag = modifyPEnv $ \s ->
                                 s { needRightBrace = flag }

    getLayoutStack  :: m [Int]
    getLayoutStack = getsPEnv layoutStack

    setLayoutStack  :: [Int] -> m ()
    setLayoutStack stack = modifyPEnv $ \s ->
                               s { layoutStack = stack }

    getEofPos :: m Pos
    getEofPos = getsPEnv eofPos

    setEofPos :: Pos -> m ()
    setEofPos pos = modifyPEnv $ \s ->
                        s{ eofPos=pos }
\end{code}

\begin{code}
data PState = PState  {  opts        :: Opts
                      ,  contextenv  :: ContextEnv
                      ,  penv        :: PEnv
                      }

emptyPState :: Opts -> Int -> ParseContext -> PState
emptyPState opts sc ctx = PState
    {  opts        = opts
    ,  contextenv  = emptyContextEnv
    ,  penv        = emptyPEnv sc ctx
    }

newtype P a = P { unP :: StateT PState (ErrorT SomeException Identity) a }
  deriving (MonadError SomeException,
            MonadState PState,
            MonadContextException,
            MonadOpts,
            MonadParser)

instance Monad P where
    m >>= f   = P $ unP m >>= unP . f
    m1 >> m2  = P $ unP m1 >> unP m2
    return    = P . return
    fail msg  = P $ throwException $ StrMsgException msg

runP :: P a -> PState -> Either SomeException (a, PState)
runP m = runIdentity . runErrorT . runStateT (unP m)

evalP :: P a -> PState -> Either SomeException a
evalP m = runIdentity . runErrorT . evalStateT (unP m)

instance MonadContextException (StateT PState (ErrorT SomeException Identity)) where
    getContextEnv      = gets    $ \s -> contextenv s
    putContextEnv env  = modify  $ \s -> s { contextenv = env }

instance MonadOpts (StateT PState (ErrorT SomeException Identity)) where
    optVal opt    = gets (opt . opts)
    getOpts       = gets opts
    setOpts opts  = modify $ \s -> s{ opts=opts }

instance MonadParser (StateT PState (ErrorT SomeException Identity)) where
    getPEnv        = gets penv
    putPEnv sigma  = modify $ \s -> s { penv = sigma }
\end{code}
