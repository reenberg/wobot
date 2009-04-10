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
-- Module      :  Language.C.Parser.Monad
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.C.Parser.Monad (
    AlexInput(..),
    alexGetChar,
    alexGetCharOrFail,
    alexInputPrevChar,
    ParseContext(..),
    MonadParser(..),
    PEnv(..),
    P,
    emptyPState,
    evalP,
    runP
  ) where

import Control.Exception
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.ContextException
import Control.Monad.Exception
import Data.Loc

import Language.C.Parser.Exceptions
import Language.C.Parser.Tokens

data AlexInput = AlexInput {-#UNPACK#-} !Pos
                           {-#UNPACK#-} !B.ByteString
                           {-#UNPACK#-} !Int

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (AlexInput pos buf off)
    | off < B.length buf  = c `seq` pos' `seq` off' `seq`
                            Just (c, AlexInput pos' buf off')
    | otherwise           = Nothing
  where
    c     = B.index buf off
    pos'  = advancePos pos c
    off'  = off + 1

    advancePos :: Pos -> Char -> Pos
    advancePos (Pos f l _ coff) '\n' = Pos f (l+1) 0       (coff + 1)
    advancePos (Pos f l c coff) _    = Pos f l     (c + 1) (coff + 1)

alexGetCharOrFail :: AlexInput -> P (Char, AlexInput)
alexGetCharOrFail inp@(AlexInput pos _ _) =
    case alexGetChar inp of
      Nothing          -> throwExceptionAt pos UnexpectedEOF
      Just (c, input)  -> return (c, input)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput _ _ 0)      = '\n'
alexInputPrevChar (AlexInput _ buf off)  = B.index buf (off - 1)

data ParseContext  =  ParseDirect
                   |  ParseExpression
                   |  ParsePattern

data PEnv = PEnv
    { buf       :: {-#UNPACK#-} !B.ByteString -- ^ Buffer we're parsing
    , off       :: {-#UNPACK#-} !Int          -- ^ Current offset in the buffer
    , last_pos  :: {-#UNPACK#-} !Pos          -- ^ End position of last token parsed
    , pos       :: {-#UNPACK#-} !Pos          -- ^ Current source code position
    , lex_state :: [Int]
    , context   :: ParseContext
    , kw        :: Map.Map String Token
    , typedefs  :: Set.Set String
    , scopes    :: [Set.Set String]
    , c99       :: Bool
    , gcc       :: Bool
    }

emptyPEnv :: Int -> ParseContext -> Bool -> Bool -> PEnv
emptyPEnv sc ctx c99 gcc = PEnv
    { buf       = B.pack ""
    , off       = 0
    , last_pos  = Pos "" 0 0 0
    , pos       = Pos "" 0 0 0
    , lex_state = [sc]
    , context   = ctx
    , kw        = Map.fromList $
                  keywords
                  ++ (if gcc then gccKeywords else [])
    , typedefs  = Set.empty
    , scopes    = []
    , c99       = c99
    , gcc       = gcc
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

    getKeywords :: m (Map.Map String Token)
    getKeywords = getsPEnv kw

    addTypedef :: String -> m ()
    addTypedef id = modifyPEnv  $ \s ->
        s{ typedefs = Set.insert id (typedefs s) }

    addVariable :: String -> m ()
    addVariable id = modifyPEnv  $ \s ->
        s{ typedefs = Set.delete id (typedefs s) }

    pushScope :: m ()
    pushScope = modifyPEnv  $ \s ->
        s{ scopes = typedefs s : scopes s }

    popScope :: m ()
    popScope = modifyPEnv  $ \s ->
        s{ scopes   = tail (scopes s)
         , typedefs = head (scopes s)
         }

    isTypedef :: String -> m Bool
    isTypedef id = getsPEnv $ \s ->
                       Set.member id (typedefs s)

    getC99 :: m Bool
    getC99 = getsPEnv c99

    getGcc :: m Bool
    getGcc = getsPEnv gcc

data PState = PState { contextenv  :: {-#UNPACK#-} !ContextEnv
                     , penv        :: {-#UNPACK#-} !PEnv
                     }

emptyPState :: Int -> ParseContext -> Bool -> Bool -> PState
emptyPState sc ctx c99 gcc = PState
    { contextenv  = emptyContextEnv
    , penv        = emptyPEnv sc ctx c99 gcc
    }

newtype P a = P { unP :: StateT PState (ErrorT SomeException Identity) a }
  deriving (MonadError SomeException,
            MonadState PState,
            MonadContextException,
            MonadParser)

instance Monad P where
    m >>= f   = P $ unP m >>= unP . f
    m1 >> m2  = P $ unP m1 >> unP m2
    return    = P . return
    fail msg  = P $ throwException $ StrMsgException msg

evalP :: P a -> PState -> Either SomeException a
evalP m = runIdentity . runErrorT . evalStateT (unP m)

runP :: P a -> PState -> Either SomeException (a, PState)
runP m = runIdentity . runErrorT . runStateT (unP m)

instance MonadContextException (StateT PState (ErrorT SomeException Identity)) where
    getContextEnv      = gets    $ \s -> contextenv s
    putContextEnv env  = modify  $ \s -> s { contextenv = env }

instance MonadParser (StateT PState (ErrorT SomeException Identity)) where
    getPEnv        = gets penv
    putPEnv sigma  = modify $ \s -> s { penv = sigma }
