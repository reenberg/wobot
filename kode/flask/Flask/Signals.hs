{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
-- Module      :  Flask.Signals
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Flask.Signals where

import Prelude hiding (exp)

import Control.Monad.State
import Data.IORef
import qualified Data.Map as Map

import Compiler
import Control.Monad.CGen
import Data.Loc
import Data.Name
import qualified Language.Hs.Syntax
import qualified Language.Hs as H
import Language.Hs.Quote
import qualified Language.F as F
import Language.C.Syntax
import Language.C.Quote
import qualified Transform.F.ToC as ToC

import Flask.Driver
import Flask.LiftN
import Flask.Monad
import Flask.Reify

newtype S a = S { unS :: FlaskM (SCode FlaskM) }

sconst :: forall a b eta . (Reify b, LiftN eta a)
       => eta
       -> S b
       -> S a
sconst k from = S $ do
    nconst <- unN $ (liftN k :: N a)
    sfrom  <- unS from
    addStream "sconst"
              (n_type nconst)
              (SConst nconst sfrom)
              (gen nconst)
              (const (return ())) $ \this -> do
    connect sfrom this tau_b (varIn this "")
  where
    tau_b :: H.Type
    tau_b = reify (undefined :: b)

    gen :: NCode -> SCode m -> FlaskM ()
    gen nconst this =
        addDecls [$decls|$var:v_in _ = $var:v_out $var:v_const|]
      where
        v_in    = varIn this ""
        v_out   = s_vout this
        v_const = n_var nconst

smap :: forall eta a b . (Reify a, Reify b, LiftN eta (a -> b))
     => eta
     -> S a
     -> S b
smap f a = S $ do
    nf   <- unN $ (liftN f :: N (a -> b))
    sa   <- unS a
    addStream "smap"
              tau_b
              (SMap nf sa)
              (gen nf)
              (const (return ())) $ \this -> do
    connect sa this tau_a (varIn this "")
  where
    tau_a, tau_b :: H.Type
    tau_a = reify (undefined :: a)
    tau_b = reify (undefined :: b)

    gen :: NCode -> SCode m -> FlaskM ()
    gen nf this = do
        -- addDecls [$decls|$var:v_in :: $ty:tau_a -> $ty:tau_b|]
        addDecls [$decls|$var:v_in x = $var:v_out ($var:v_f x)|]
      where
        v_in  = varIn this ""
        v_out = s_vout this
        v_f   = n_var nf

strace :: (S a -> S ()) -> S a -> S a
strace f s_from = S $ do
    sfrom  <-  unS s_from
    _      <-  unS (f s_from)
    return sfrom

sfilter :: forall a eta . (Reify a, LiftN eta (a -> Bool))
        => eta
        -> S a
        -> S a
sfilter f a = S $ do
    nf <- unN $ (liftN f :: N (a -> Bool))
    sa <- unS a
    addStream  "sfilt"
               (s_type sa)
               (SFilter nf sa)
               (gen nf)
               (const (return ())) $ \this -> do
    liveVar (n_var nf)
    connect sa this tau_a (varIn this "")
  where
    tau_a :: H.Type
    tau_a = reify (undefined :: a)

    gen :: NCode -> SCode n -> FlaskM ()
    gen nf this = do
        -- addDecls [$decls|$var:v_in :: $ty:tau_a -> $ty:tau_a|]
        addDecls [$decls|$var:v_in x = if $var:v_f x then $var:v_out x else ()|]
      where
        v_in  = varIn this ""
        v_out = s_vout this
        v_f   = n_var nf

smerge :: forall a . Reify a
       => S a
       -> S a
       -> S a
smerge a b = S $ do
    sa   <- unS a
    sb   <- unS b
    addStream  "smerge"
               (s_type sa)
               (SMerge sa sb)
               gen
               (const (return ())) $ \this -> do
    connect sa this tau_a (varIn this "")
    connect sb this tau_a (varIn this "")
  where
    tau_a :: H.Type
    tau_a = reify (undefined :: a)

    gen :: SCode m -> FlaskM ()
    gen this =
        addDecls [$decls|$var:v_in = $var:v_out|]
      where
        v_in  = varIn this ""
        v_out = s_vout this

sunzip :: (Reify a, Reify b) => S (a, b) -> (S a, S b)
sunzip s = (smap [$exp|fst|] s, smap [$exp|snd|] s)

szip :: forall a b . (Reify a, Reify b)
     => S a
     -> S b
     -> S (a, b)
szip a b = S $ do
    sa <- unS a
    sb <- unS b
    addStream  "szip"
               tau_out
               (SZip sa sb)
               (genHs sa sb)
               (genC sa sb) $ \this -> do
    liveVar (s_vout this)
    connect sa this tau_a (varIn this "a")
    connect sb this tau_b (varIn this "b")
  where
    tau_a, tau_b, tau_out :: H.Type
    tau_a   = reify (undefined :: a)
    tau_b   = reify (undefined :: b)
    tau_out = [$ty|($ty:tau_a, $ty:tau_b)|]

    genHs :: SCode m -> SCode m -> SCode m -> FlaskM ()
    genHs sa sb this = do
        addCImport c_v_in_a [$ty|$ty:tau_a -> ()|] [$cexp|$id:c_v_in_a|]
        addCImport c_v_in_b [$ty|$ty:tau_b -> ()|] [$cexp|$id:c_v_in_b|]
      where
        tau_a     = s_type sa
        tau_b     = s_type sb
        v_in_a    = varIn this "a"
        c_v_in_a  = show v_in_a
        v_in_b    = varIn this "b"
        c_v_in_b  = show v_in_b

    genC :: SCode m -> SCode m -> SCode m -> FlaskM ()
    genC sa sb this = do
        tauf_a  <- toF tau_a
        tauf_b  <- toF tau_b
        tauf_to <- toF tau_to

        (stm_out, _) <- newScope False $ do
            e_out <- hcall v_out $
                     ToC.CUnboxedData tauf_to
                            [ToC.CLowered tauf_a [$cexp|$id:buf_a|],
                             ToC.CLowered tauf_b [$cexp|$id:buf_b|]]
            addCStm [$cstm|$exp:e_out;|]

        genZipInput tau_a tauf_a v_in_a buf_a_set buf_a stm_out
        genZipInput tau_b tauf_b v_in_b buf_b_set buf_b stm_out
      where
        tau_a     = s_type sa
        tau_b     = s_type sb
        tau_to    = [$ty|($ty:tau_a, $ty:tau_b)|]
        v_out     = s_vout this
        v_in_a    = varIn this "a"
        buf_a_set = ident this "_a_set"
        buf_a     = ident this "_a"
        v_in_b    = varIn this "b"
        buf_b_set = ident this "_b_set"
        buf_b     = ident this "_b"

        genZipInput :: MonadFlask m
                    => H.Type
                    -> F.Type
                    -> H.Var
                    -> String
                    -> String
                    -> Stm
                    -> m ()
        genZipInput tau tauf v_in buf_set buf stm_out = do
            cty <- toC tau
            addCDecldef  [$cedecl|int $id:buf_set;|]
            addCDecldef  [$cedecl|$ty:cty $id:buf;|]
            addCInitStm  [$cstm|$id:buf_set = 0;|]
            (params, ce_params) <- ToC.flattenParams tauf
            (stm_params, _)     <- newScope False $ do
                e_params <- ToC.concrete ce_params
                addCStm [$cstm|$id:buf_set = 1;|]
                addCStm [$cstm|$id:buf = $exp:e_params;|]
            addCFundef [$cedecl|
void $id:c_v_in($params:params)
{
    $stm:stm_params
    if ($id:buf_a_set && $id:buf_b_set) {
      $stm:stm_out
      $id:buf_a_set = 0;
      $id:buf_b_set = 0;
    }
}
|]
          where
            c_v_in = show v_in

sintegrate  ::  forall a b c eta . (Reify a, Reify b, Reify c,
                                    LiftN eta ((a, c) -> (b, c)))
            =>  N c
            ->  eta
            ->  S a
            ->  S b
sintegrate zero f a = S $ do
    nzero <- unN $ (liftN zero :: N c)
    nf    <- unN $ (liftN f :: N ((a, c) -> (b, c)))
    sa    <- unS a
    addStream "sintegrate"
              tau_b
              (SIntegrate nzero nf sa)
              genHs
              (genC nzero nf) $ \this -> do
    liveVar (n_var nzero)
    liveVar (n_var nf)
    liveVar (s_vout this)
    connect sa this tau_a (varIn this "")
  where
    tau_a, tau_b, tau_state, tau_f_in, tau_f_out :: H.Type
    tau_a     = reify (undefined :: a)
    tau_b     = reify (undefined :: b)
    tau_state = reify (undefined :: c)
    tau_f_in  = reify (undefined :: a, undefined :: c)
    tau_f_out = reify (undefined :: b, undefined :: c)

    genHs :: SCode m -> FlaskM ()
    genHs this =
        addCImport c_v_in [$ty|$ty:tau_a -> ()|] [$cexp|$id:c_v_in|]
      where
        v_in     = varIn this ""
        c_v_in   = show v_in

    genC :: NCode -> NCode -> SCode m -> FlaskM ()
    genC nzero nf this = do
        cty_state <- toC tau_state
        ce_zero   <- toC v_zero
        addCDecldef [$cedecl|$ty:cty_state $id:state;|]
        addCInitStm [$cstm|$id:state = $exp:ce_zero;|]

        tauf_a     <- toF tau_a
        tauf_state <- toF tau_state
        tauf_f_in  <- toF tau_f_in
        tauf_b     <- toF tau_b
        cty_f_out  <- toC tau_f_out
        (params_stm, params) <- newScope False $ do
            (params, params_ce) <- ToC.flattenParams tauf_a
            ce_int  <-  hcall v_f $
                        ToC.CUnboxedData tauf_f_in
                              [params_ce,
                               ToC.CLowered tauf_state [$cexp|$id:state|]]
            ce_out <-  hcall v_out $
                       ToC.CLowered tauf_b [$cexp|_out.element1|]
            addCStm [$cstm|_out = $exp:ce_int;|]
            addCStm [$cstm|$id:state = _out.element2;|]
            addCStm [$cstm|$exp:ce_out;|]
            return params
        addCFundef [$cedecl|
void $id:c_v_in($params:params)
{
    $ty:cty_f_out _out;

    $stm:params_stm;
}
|]
      where
        v_in     = varIn this ""
        c_v_in   = show v_in
        v_out    = s_vout this
        state    = ident this "state"
        v_zero   = n_var nzero
        v_f      = n_var nf

sjust :: forall a . Reify a => S (Maybe a) -> S a
sjust maybe_a = S $ do
    smaybe_a <- unS maybe_a
    addStream "sjust"
              tau_a
              (SJust smaybe_a)
              genHs
              genC $ \this -> do
    liveVar (s_vout this)
    connect smaybe_a this tau_maybe_a (varIn this "")
  where
    tau_a, tau_maybe_a :: H.Type
    tau_a       = reify (undefined :: a)
    tau_maybe_a = reify (undefined :: Maybe a)

    genHs :: SCode m -> FlaskM ()
    genHs this =
        addCImport c_v_in [$ty|$ty:tau_maybe_a -> ()|] [$cexp|$id:c_v_in|]
      where
        v_in   = varIn this ""
        c_v_in = show v_in

    genC :: SCode m -> FlaskM ()
    genC this = do
        tauf_maybe_a        <- toF tau_maybe_a
        tauf_a              <- toF tau_a
        (params, params_ce) <- ToC.flattenParams tauf_maybe_a
        e_tag               <- ToC.dataTag params_ce
        [e_just]            <- ToC.dataMembers (F.con "Just") params_ce
                               >>= mapM ToC.concrete
        e_out               <-  hcall v_out $
                                ToC.CLowered tauf_a e_just
        addCFundef [$cedecl|
void $id:c_v_in($params:params)
{
    if ($exp:e_tag == Just)
       $exp:e_out;
}
|]
      where
        v_in   = varIn this ""
        c_v_in = show v_in
        v_out  = s_vout this

clock :: Int -> S ()
clock period = S $ do
    addStream  "clock"
               unitTy
               (SClock period)
               (gen period)
               (const (return ())) $ \this -> do
    addTimer period
    connectTimer period this (varIn this "")
  where
    gen :: Int -> SCode m -> FlaskM ()
    gen period this = do
        addDecls [$decls|$var:v_in :: () -> ()|]
        addDecls [$decls|$var:v_in x = $var:v_out ()|]
      where
        v_in  = varIn this ""
        v_out = s_vout this

adc :: String -> S () -> S Float
adc name from =  S $ do
    sfrom <- unS from
    addStream  "sadc"
               floatTy
               (SADC name)
               (genHs sfrom)
               (genC sfrom) $ \this -> do
    liveVar (s_vout this)
    connect sfrom this unitTy (varIn this "")
  where
    genHs :: SCode m -> SCode m -> FlaskM ()
    genHs sfrom this =
        addCImport c_v_in [$ty|() -> ()|] [$cexp|$id:c_v_in|]
      where
        v_in     = varIn this ""
        c_v_in   = show v_in

    genC :: SCode m -> SCode m -> FlaskM ()
    genC sfrom this = do
        e_out <- hcall v_out $ ToC.CLowered floatGTy [$cexp|data|]
        --usesProvides True [$ncusesprovides|uses interface ADC as $id:name;|]
        i <- adcFlag
        return ()
        --adcGetData i [$cexp|call $id:name.getData()|]
        {-let c_i = toInteger i
        addCFundef [$cedecl|
void $id:c_v_in()
{
    atomic {
        if (adc_pending == 0)
            call $id:name.getData();
        adc_pending |= 1 << $int:c_i;
    }
}
|]
        addCFundef [$cedecl|
task void $id:adc_task()
{
    typename uint16_t data;

    atomic {
        data = adc_val;
        adc_pending &= ~(1 << $int:c_i);
        if (adc_pending != 0)
            post adc_process_pending();
    }

    $exp:e_out;
}
|]
        addCVardef [$cedecl|
async event typename result_t $id:name.dataReady(typename uint16_t data)
{
    adc_val = data;
    post $id:adc_task();
    return SUCCESS;
}
|]-}
      where
        v_in     = varIn this ""
        c_v_in   = show v_in
        v_out    = s_vout this
        adc_task = ident this "task"

        adcFlag :: MonadFlask m => m Int
        adcFlag = do
            i <- getsFlaskEnv $ \s -> f_adcs s
            modifyFlaskEnv $ \s -> s { f_adcs = i + 1 }
            return i

        adcGetData :: MonadFlask m => Int -> Exp -> m ()
        adcGetData flag e =
            modifyFlaskEnv $ \s ->
                s { f_adc_getdata = Map.insert flag e (f_adc_getdata s) }

recv :: forall a . Reify a => FlowChannel -> FlowActivity -> S a
recv chan activity = S $ do
    tauf <- toF tau
    addStream "srecv"
              tau
              (SRecv chan activity)
              gen
              (const (return ())) $ \this -> do
    connectFlow chan activity tauf this (varIn this "")
  where
    tau :: H.Type
    tau = reify (undefined :: a)

    gen :: SCode m -> FlaskM ()
    gen this =
        addDecls [$decls|$var:v_in = $var:v_out|]
      where
        v_in  = varIn this ""
        v_out = s_vout this

send :: forall a . (Reify a) => FlowChannel -> FlowActivity -> S a -> S ()
send chan act a = S $ do
    sa   <- unS a
    tauf <- toF tau
    sendOnFlow chan act tauf
    addStream "ssend"
              tau
              (SSend chan act sa)
              genHs
              (genC sa) $ \this -> do
    connect sa this tau (varIn this "")
  where
    tau :: H.Type
    tau = reify (undefined :: a)

    genHs :: SCode m -> FlaskM ()
    genHs this =
        addCImport c_v_in [$ty|$ty:tau -> ()|] [$cexp|$id:c_v_in|]
      where
        v_in   = varIn this ""
        c_v_in = show v_in

    genC :: SCode m -> SCode m -> FlaskM ()
    genC sa this = do
        tauf <- toF tau
        (params, ce_params) <- ToC.flattenParams tauf
        {-(stm_params, _)     <- newScope False $ do
            e <- ToC.concrete ce_params
            addCStm [$cstm|call Flow.anycast($int:chan, &$exp:e, sizeof($exp:e));|]-}
        return ()
        {-addCFundef [$cedecl|
void $id:c_v_in($params:params)
{
    $stm:stm_params
}
|]-}
      where
        v_in   = varIn this ""
        c_v_in = show v_in

sloop :: forall a b c . (Reify a, Reify b, Reify c)
      => Int
      -> N c
      -> (S (a, c) -> S (b, c))
      -> S a
      -> S b
sloop depth zero f a = S $ do
    nzero <- unN $ (liftN zero :: N c)
    sa    <- unS a
    enter <- addStream  "sloop_enter"
                        tau_f_in
                        (SBlackbox $ "sloop_enter " ++ show (n_id nzero, s_id sa))
                        genEnterHs
                        (genEnterC nzero sa) $
             \this -> do
             liveVar (n_var nzero)
             liveVar (s_vout this)
             connect sa this tau_a (varIn this "")
    sf <- unS $ f $ S (return enter)
    exit <- addStream  "sloop_exit"
                       tau_b
                       (SBlackbox $ "sloop_exit " ++ show (s_id sf))
                       genExitHs
                       (genExitC sf enter) $
            \this -> do
            liveVar (s_vout this)
            connect sf this tau_f_out (varIn this "")
    return exit
  where
    tau_a, tau_b, tau_state, tau_f_in :: H.Type
    tau_a     = reify (undefined :: a)
    tau_b     = reify (undefined :: b)
    tau_state = reify (undefined :: c)
    tau_f_in  = reify (undefined :: a, undefined :: c)
    tau_f_out = reify (undefined :: b, undefined :: c)

    cdepth :: Integer
    cdepth = toInteger depth

    genEnterHs :: SCode m -> FlaskM ()
    genEnterHs this =
        addCImport c_v_in [$ty|$ty:tau_a -> ()|] [$cexp|$id:c_v_in|]
      where
        v_in     = varIn this ""
        c_v_in   = show v_in

    genEnterC :: NCode -> SCode m -> SCode m -> FlaskM ()
    genEnterC nzero sa this = do
        tauf_a     <- toF tau_a
        cty_a      <- toC tau_a
        cty_state  <- toC tau_state
        ce_zero    <- toC v_zero
        addCDecldef [$cedecl|$ty:cty_state $id:state;|]
        addCInitStm [$cstm|$id:state = $exp:ce_zero;|]
        addCDecldef [$cedecl|int $id:count;|]
        addCInitStm [$cstm|$id:count = 0;|]
        addCDecldef [$cedecl|$ty:cty_a $id:pending[$int:cdepth];|]

        {-tauf_state <- toF tau_state
        tauf_f_in  <- toF tau_f_in-}
        (params_stm, params) <- newScope False $ do
            (params, params_ce) <- ToC.flattenParams tauf_a
            e_params <- ToC.concrete params_ce
            {-addCStm [$cstm|
atomic {
    if($id:count < $int:cdepth - 1) {
        $id:pending[$id:count++] = $exp:e_params;
        post $id:task();
    }
}
|]-}
            return params
        return ()
        {-addCFundef [$cedecl|
void $id:c_v_in($params:params)
{
    $stm:params_stm;
}
|]
        e_out      <- hcall v_out $
                      ToC.CUnboxedData tauf_f_in
                           [ToC.CLowered tauf_a [$cexp|$id:pending[$id:count-1]|],
                            ToC.CLowered tauf_state [$cexp|$id:state|]]
        addCFundef [$cedecl|
task void $id:task()
{
    $exp:e_out;
}
|]-}
      where
        v_in     = varIn this ""
        c_v_in   = show v_in
        v_out    = s_vout this
        v_zero   = n_var nzero
        state    = ident this "state"
        count    = ident this "count"
        pending  = ident this "pending"
        task     = ident this "task"

    genExitHs :: SCode m -> FlaskM ()
    genExitHs this =
        addCImport c_v_in [$ty|$ty:tau_f_out -> ()|] [$cexp|$id:c_v_in|]
      where
        v_in     = varIn this ""
        c_v_in   = show v_in

    genExitC :: SCode m -> SCode m -> SCode m -> FlaskM ()
    genExitC f enter this = do
        tauf_b     <- toF tau_b
        tauf_f_out <- toF tau_f_out
        (params_stm, params) <- newScope False $ do
            (params, params_ce) <- ToC.flattenParams tauf_f_out
            [ce_b, ce_state]    <- ToC.dataMembers undefined params_ce
                                   >>= mapM ToC.concrete
            ce_out              <- hcall v_out $
                                   ToC.CLowered tauf_b ce_b
            addCStm [$cstm|$id:state = $exp:ce_state;|]
            addCStm [$cstm|$exp:ce_out;|]
            return params
        return ()
        {-addCFundef [$cedecl|
void $id:c_v_in($params:params)
{
    $stm:params_stm;
    $id:count--;
    if ($id:count > 0)
        post $id:task();
}
|]-}
      where
        v_in     = varIn this ""
        c_v_in   = show v_in
        v_out    = s_vout this
        state    = ident enter "state"
        count    = ident enter "count"
        pending  = ident enter "pending"
        task     = ident enter "task"

nfold :: forall a c . (Reify a, Reify c)
      => FlowChannel
      -> N c
      -> (S (a, c) -> S (a, c))
      -> S a
      -> S ()
nfold chan zero f sin =
    smerge (recv chan Passive) sin >>> sloop 1 zero f >>> send chan Passive

nfold' :: forall a c . (Reify a, Reify c)
       => FlowChannel
       -> N c
       -> (S (Either a a, c) -> S (a, c))
       -> S a
       -> S ()
nfold' chan zero f sin =
    smerge  (smap [$exp|Left|] (recv chan Passive :: S a))
            (smap [$exp|Right|] sin)
    >>> sloop 1 zero f >>> send chan Passive

infixl 5 >>>
(>>>) = flip ($)
