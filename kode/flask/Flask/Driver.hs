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
-- Module      :  Flask.Driver
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Flask.Driver where

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
import qualified Control.Monad.NesCGen as NesCGen
import Control.Monad.ContextException
import Control.Monad.Ref
import Control.Monad.Trace
import Control.Monad.Unique
import Flask.Monad
import qualified Transform.F.ToC as ToC
import qualified Transform.F.Simplify as Simplify

newtype FlaskM a = FlaskM
    { unFlaskM :: StateT FlaskState (ErrorT SomeException IO) a }

data FlaskState = FlaskState
    {  uniq        :: IORef Uniq
    ,  opts        :: Opts
    ,  contextenv  :: ContextEnv
    ,  traceenv    :: TraceEnv
    ,  tcenv       :: Check.Hs.TcEnv IORef FlaskM
    ,  ftcenv      :: Check.F.TcEnv
    ,  simplenv    :: Simplify.SimplEnv
    ,  cgenenv     :: CGen.CGenEnv
    ,  nescgenenv  :: NesCGen.NesCGenEnv
    ,  tocenv      :: ToC.ToCEnv FlaskM
    ,  flaskenv    :: FlaskEnv FlaskM
    }

emptyFlaskState :: Opts -> IO FlaskState
emptyFlaskState opts = do
    u <- newRef 0
    return FlaskState  {  uniq        = u
                       ,  opts        = opts
                       ,  contextenv  = emptyContextEnv
                       ,  traceenv    = emptyTraceEnv
                       ,  tcenv       = Check.Hs.emptyTcEnv
                       ,  ftcenv      = Check.F.emptyTcEnv
                       ,  simplenv    = Simplify.emptySimplEnv
                       ,  cgenenv     = CGen.emptyCGenEnv
                       ,  nescgenenv  = NesCGen.emptyNesCGenEnv
                       ,  tocenv      = ToC.emptyToCEnv
                       ,  flaskenv    = emptyFlaskEnv
                       }

runFlaskM  ::  FlaskM a
           ->  FlaskState
           ->  IO (Either SomeException (a, FlaskState))
runFlaskM m s = runErrorT (runStateT (unFlaskM m) s)

evalFlaskM  ::  FlaskM a
            ->  FlaskState
            ->  IO (Either SomeException a)
evalFlaskM m s = runErrorT (evalStateT (unFlaskM m) s)

instance Monad FlaskM where
    m >>= f   = FlaskM $ unFlaskM m >>= unFlaskM . f
    m1 >> m2  = FlaskM $ unFlaskM m1 >> unFlaskM m2
    return    = FlaskM . return
    fail msg  = throwException $ StrMsgException msg

instance MonadIO FlaskM where
    liftIO = FlaskM . liftIO

instance MonadRef IORef FlaskM where
    newRef a      = FlaskM $ lift $ lift $ newRef a
    readRef r     = FlaskM $ lift $ lift $ readRef r
    writeRef r a  = FlaskM $ lift $ lift $ writeRef r a
    modifyRef r f = FlaskM $ lift $ lift $ modifyRef r f

instance MonadError SomeException FlaskM where
    throwError e            = FlaskM $ throwError e
    m `catchError` handler  = FlaskM $ unFlaskM m `catchError` \e ->
                                  unFlaskM (handler e)

instance MonadState FlaskState FlaskM where
    get    = FlaskM $ get
    put s  = FlaskM $ put s

instance MonadUnique FlaskM where
    newUnique = do
        ref <- gets uniq
        uniq <- readRef ref
        writeRef ref (uniq + 1)
        return uniq

instance MonadOpts FlaskM where
    optVal opt    = FlaskM $ gets (opt . opts)
    getOpts       = FlaskM $ gets opts
    setOpts opts  = FlaskM $ modify $ \s -> s{ opts=opts }

instance MonadContextException FlaskM where
    getContextEnv      = FlaskM $ gets    $ \s -> contextenv s
    putContextEnv env  = FlaskM $ modify  $ \s -> s { contextenv = env }

instance MonadTrace FlaskM where
    getTraceEnv      = FlaskM $ gets traceenv
    putTraceEnv env  = FlaskM $ modify $ \s -> s { traceenv = env }

instance Check.Hs.MonadTc IORef FlaskM where
    getTcEnv      = gets tcenv
    putTcEnv env  = modify $ \s -> s { tcenv = env }

instance Check.F.MonadTc FlaskM where
    getTcEnv      = FlaskM $ gets ftcenv
    putTcEnv env  = FlaskM $ modify $ \s -> s { ftcenv = env }

instance Simplify.MonadSimpl FlaskM where
    getSimplEnv      = FlaskM $ gets simplenv
    putSimplEnv env  = FlaskM $ modify $ \s -> s { simplenv = env }

instance CGen.MonadCGen FlaskM where
    getCGenEnv      = gets cgenenv
    putCGenEnv env  = modify $ \s -> s { cgenenv = env }

instance NesCGen.MonadNesCGen FlaskM where
    getNesCGenEnv      = gets nescgenenv
    putNesCGenEnv env  = modify $ \s -> s { nescgenenv = env }

instance ToC.MonadToC IORef FlaskM where
    getToCEnv      = gets tocenv
    putToCEnv env  = modify $ \s -> s { tocenv = env }

instance MonadCompiler FlaskM where

instance MonadFlask FlaskM where
    getFlaskEnv      = gets flaskenv
    putFlaskEnv env  = modify $ \s -> s { flaskenv = env }
