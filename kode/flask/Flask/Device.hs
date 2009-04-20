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
-- Module      :  Flask.Device
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  athas@sigkill.dk
--
--------------------------------------------------------------------------------

module Flask.Device where

import Prelude hiding (exp)

import Control.Monad.State
import Control.Monad.Trans
import Data.IORef
import Data.List (intersperse, concat)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO

import qualified Check.F
import qualified Check.Hs
import Compiler
import Control.Monad.CGen
import Data.Loc
import Data.Name
import Data.List
import Data.String.Quote
import qualified Language.Hs.Syntax
import qualified Language.Hs as H
import Language.Hs.Quote
import Language.C.Syntax
import Language.C.Quote
import Text.PrettyPrint.Mainland
import qualified Transform.F.ToC as ToC
import qualified Transform.Hs.Desugar as Desugar
import qualified Transform.Hs.Rename as Rename

import Flask.Driver
import Flask.Monad
import Flask.Signals
import Flask.Reify
import Flask.LiftN

statevar :: Device d => MonadFlask m => d -> String -> m String
statevar d s = do
  devices <- getsFlaskEnv f_devices
  case findIndex (\d2 -> d_id d2 == unique_id d) devices of
    Just n -> return $ "device_" ++ (show n) ++ s
    Nothing -> return $ error $ "Unknown device " ++ unique_id d ++ " encountered."


data Pin = Pin Integer Bool
           deriving Eq

instance Device Pin where
    setup d@(Pin pin initstate) = 
        do 
          sv <- statevar d "state"
          addCDecldef [$cedecl|unsigned char $id:sv;|]
          addCInitStm [$cstm|pinMode($int:pin, OUTPUT);|]
          addCInitStm [$cstm|$id:sv = $id:val;|]
          addCInitStm [$cstm|digitalWrite($int:pin, $id:val);|]
          where
            val = if initstate then "HIGH" else "LOW"
    unique_id (Pin pin initstate) = "pin_" ++ (show pin)

modDev :: forall a d. (Reify a, Device d) => d -> (d -> String -> String -> Definition) -> S a -> S ()
modDev d f a = S $ do
    sa   <- unS a
    addDevice d
    addStream "toggle"
              unitTy
              (DeviceWrite sa $ unique_id d)
              genHs
              genC $ \this -> do
    connect sa this tau (varIn this "")
  where
    tau :: H.Type
    tau = reify (undefined :: a)

    genHs :: SCode m -> FlaskM ()
    genHs this =
        addCImport c_v_in [$ty|$ty:tau -> ()|] [$cexp|$id:c_v_in|]
        where
          v_in    = varIn this ""
          c_v_in = show v_in

    genC :: SCode m -> FlaskM ()
    genC this = do
        tauf <- toF tau
        sv <- statevar d "state"
        (params, ce_params) <- ToC.flattenParams tauf
        addCFundef $ f d c_v_in sv
        where
          v_in   = varIn this ""
          c_v_in = show v_in

toggle :: forall a . (Reify a) => Pin -> S a -> S ()
toggle pin = modDev pin $ \(Pin pin _ ) c_v_in sv ->
             [$cedecl|void $id:c_v_in()
                               { 
                                       if ($id:sv == HIGH) $id:sv = LOW;
                                       else $id:sv = HIGH;
                                       digitalWrite($int:pin, $id:sv);
                               }|]

turnOn :: forall a . (Reify a) => Pin -> S a -> S ()
turnOn pin = modDev pin $ \(Pin pin _ ) c_v_in sv ->
             [$cedecl|void $id:c_v_in()
                               { 
                                       $id:sv = HIGH;
                                       digitalWrite($int:pin, $id:sv);
                               }|]

turnOff :: forall a . (Reify a) => Pin -> S a -> S ()
turnOff pin = modDev pin $ \(Pin pin _ ) c_v_in sv ->
             [$cedecl|void $id:c_v_in()
                               { 
                                       $id:sv = LOW;
                                       digitalWrite($int:pin, $id:sv);
                               }|]

-- It would be nice if the succint code below could be used, but we need to share the state data between toggle/turnOn/turnOff.
{-toggle d@(Pin pin startstate) a = S $ do
  sa <- unS a
  addDevice d
  unS (a >>> sref (unique_id d) >>> onezero >>> digitalWrite)
    where
      onezero = sintegrate zero int
          where
            startval = if startstate then 1 else 0
            zero :: N Integer
            zero = liftN [$exp|$int:startval|]
            int :: N ((a, Integer) -> ((Integer, Integer), Integer))
            int = liftN [$decls|f (x, 0) = (($int:pin, 1), 1); f (x, 1) = (($int:pin, 0), 0)|]-}
