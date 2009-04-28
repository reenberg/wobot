{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

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
-- Module      :  Fladuino.Driver
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Fladuino.Driver where

import Prelude hiding (exp)

import Control.Exception (SomeException)
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans
import Data.IORef

import qualified Check.F
import qualified Check.Hs
import Compiler
import Control.Monad.Exception
import qualified Control.Monad.CGen as CGen
import Control.Monad.ContextException
import Control.Monad.Ref
import Control.Monad.Trace
import Control.Monad.Unique
import Fladuino.Monad
import qualified Transform.F.ToC as ToC
import qualified Transform.F.Simplify as Simplify

newtype FladuinoM a = FladuinoM
    { unFladuinoM :: StateT FladuinoState (ErrorT SomeException IO) a }

data FladuinoState = FladuinoState
    {  uniq        :: IORef Uniq
    ,  opts        :: Opts
    ,  contextenv  :: ContextEnv
    ,  traceenv    :: TraceEnv
    ,  tcenv       :: Check.Hs.TcEnv IORef FladuinoM
    ,  ftcenv      :: Check.F.TcEnv
    ,  simplenv    :: Simplify.SimplEnv
    ,  cgenenv     :: CGen.CGenEnv
    ,  tocenv      :: ToC.ToCEnv FladuinoM
    ,  fladuinoenv    :: FladuinoEnv FladuinoM
    }

emptyFladuinoState :: Opts -> IO FladuinoState
emptyFladuinoState opts = do
    u <- newRef 0
    return FladuinoState  {  uniq        = u
                       ,  opts        = opts
                       ,  contextenv  = emptyContextEnv
                       ,  traceenv    = emptyTraceEnv
                       ,  tcenv       = Check.Hs.emptyTcEnv
                       ,  ftcenv      = Check.F.emptyTcEnv
                       ,  simplenv    = Simplify.emptySimplEnv
                       ,  cgenenv     = CGen.emptyCGenEnv
                       ,  tocenv      = ToC.emptyToCEnv
                       ,  fladuinoenv    = emptyFladuinoEnv
                       }

runFladuinoM  ::  FladuinoM a
           ->  FladuinoState
           ->  IO (Either SomeException (a, FladuinoState))
runFladuinoM m s = runErrorT (runStateT (unFladuinoM m) s)

evalFladuinoM  ::  FladuinoM a
            ->  FladuinoState
            ->  IO (Either SomeException a)
evalFladuinoM m s = runErrorT (evalStateT (unFladuinoM m) s)

instance Monad FladuinoM where
    m >>= f   = FladuinoM $ unFladuinoM m >>= unFladuinoM . f
    m1 >> m2  = FladuinoM $ unFladuinoM m1 >> unFladuinoM m2
    return    = FladuinoM . return
    fail msg  = throwException $ StrMsgException msg

instance MonadIO FladuinoM where
    liftIO = FladuinoM . liftIO

instance MonadRef IORef FladuinoM where
    newRef a      = FladuinoM $ lift $ lift $ newRef a
    readRef r     = FladuinoM $ lift $ lift $ readRef r
    writeRef r a  = FladuinoM $ lift $ lift $ writeRef r a
    modifyRef r f = FladuinoM $ lift $ lift $ modifyRef r f

instance MonadError SomeException FladuinoM where
    throwError e            = FladuinoM $ throwError e
    m `catchError` handler  = FladuinoM $ unFladuinoM m `catchError` \e ->
                                  unFladuinoM (handler e)

instance MonadState FladuinoState FladuinoM where
    get    = FladuinoM $ get
    put s  = FladuinoM $ put s

instance MonadUnique FladuinoM where
    newUnique = do
        ref <- gets uniq
        uniq <- readRef ref
        writeRef ref (uniq + 1)
        return uniq

instance MonadOpts FladuinoM where
    optVal opt    = FladuinoM $ gets (opt . opts)
    getOpts       = FladuinoM $ gets opts
    setOpts opts  = FladuinoM $ modify $ \s -> s{ opts=opts }

instance MonadContextException FladuinoM where
    getContextEnv      = FladuinoM $ gets    $ \s -> contextenv s
    putContextEnv env  = FladuinoM $ modify  $ \s -> s { contextenv = env }

instance MonadTrace FladuinoM where
    getTraceEnv      = FladuinoM $ gets traceenv
    putTraceEnv env  = FladuinoM $ modify $ \s -> s { traceenv = env }

instance Check.Hs.MonadTc IORef FladuinoM where
    getTcEnv      = gets tcenv
    putTcEnv env  = modify $ \s -> s { tcenv = env }

instance Check.F.MonadTc FladuinoM where
    getTcEnv      = FladuinoM $ gets ftcenv
    putTcEnv env  = FladuinoM $ modify $ \s -> s { ftcenv = env }

instance Simplify.MonadSimpl FladuinoM where
    getSimplEnv      = FladuinoM $ gets simplenv
    putSimplEnv env  = FladuinoM $ modify $ \s -> s { simplenv = env }

instance CGen.MonadCGen FladuinoM where
    getCGenEnv      = gets cgenenv
    putCGenEnv env  = modify $ \s -> s { cgenenv = env }

instance ToC.MonadToC IORef FladuinoM where
    getToCEnv      = gets tocenv
    putToCEnv env  = modify $ \s -> s { tocenv = env }

instance MonadCompiler FladuinoM where

instance MonadFladuino FladuinoM where
    getFladuinoEnv      = gets fladuinoenv
    putFladuinoEnv env  = modify $ \s -> s { fladuinoenv = env }
