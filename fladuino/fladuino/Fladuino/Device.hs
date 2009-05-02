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
-- Module      :  Fladuino.Device
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  athas@sigkill.dk
--
--------------------------------------------------------------------------------

module Fladuino.Device where

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

import Fladuino.Driver
import Fladuino.Monad
import Fladuino.Signals
import Fladuino.Reify
import Fladuino.LiftN

statevar :: Device d => MonadFladuino m => d -> String -> m String
statevar d s = do
  devices <- getsFladuinoEnv f_devices
  case findIndex (\(DRef d2) -> uniqueId d2 == uniqueId d) devices of
    Just n -> return $ "device_" ++ (show n) ++ s
    Nothing -> return $ error $ "Unknown device " ++ uniqueId d ++ " encountered."


data DigitalOutputPin = DigitalOutputPin Integer Bool
           deriving Eq

data AnalogOutputPin = AnalogOutputPin Integer Integer
           deriving Eq

data DigitalInputPin = DigitalInputPin Integer
           deriving Eq

data AnalogInputPin = AnalogInputPin Integer
           deriving Eq

instance Device DigitalOutputPin where
    setup d@(DigitalOutputPin pin initstate) = 
        do 
          sv <- statevar d "state"
          addCDecldef [$cedecl|unsigned char $id:sv;|]
          addCInitStm [$cstm|pinMode($int:pin, OUTPUT);|]
          addCInitStm [$cstm|$id:sv = $id:val;|]
          addCInitStm [$cstm|digitalWrite($int:pin, $id:val);|]
          where
            val = if initstate then "HIGH" else "LOW"
    deviceClass _ = "digital_output_pin"
    uniqueId d@(DigitalOutputPin pin initstate) = deviceClass d ++ (show pin)

instance Device AnalogOutputPin where
    setup d@(AnalogOutputPin pin initstate) = 
        do 
          addCInitStm [$cstm|pinMode($int:pin, OUTPUT);|]
          addCInitStm [$cstm|analogWrite($int:pin, $int:initstate);|]
    deviceClass _ = "analog_output_pin"
    uniqueId d@(AnalogOutputPin pin initstate) = deviceClass d ++ (show pin)

modDev :: forall a d. (Reify a, Device d) => String -> d -> (d -> String -> String -> ([Param], [Exp]) -> Definition) -> S a -> S ()
modDev name d f a = S $ do
    sa   <- unS a
    addDevice d
    addStream name
              unitTy
              (DeviceWrite sa $ uniqueId d ++ "_" ++ name)
              genHs
              genC $ \this -> do
    connect sa this tau (varIn this "")
  where
    tau :: H.Type
    tau = reify (undefined :: a)

    genHs :: SCode m -> FladuinoM ()
    genHs this =
        addCImport c_v_in [$ty|$ty:tau -> ()|] [$cexp|$id:c_v_in|]
        where
          v_in    = varIn this ""
          c_v_in = show v_in

    genC :: SCode m -> FladuinoM ()
    genC this = do
        tauf <- toF tau
        sv <- statevar d "state"
        (params, ce_params) <- ToC.flattenParams tauf
        e <- ToC.flattenArgs ce_params
        addCFundef $ f d c_v_in sv (params, e)
        where
          v_in   = varIn this ""
          c_v_in = show v_in

toggle :: forall a . (Reify a) => DigitalOutputPin -> S a -> S ()
toggle pin = modDev "toggle" pin $ \(DigitalOutputPin pin _ ) c_v_in sv _ ->
             [$cedecl|void $id:c_v_in()
                               { 
                                       if ($id:sv == HIGH) $id:sv = LOW;
                                       else $id:sv = HIGH;
                                       digitalWrite($int:pin, $id:sv);
                               }|]

turnOn :: forall a . (Reify a) => DigitalOutputPin -> S a -> S ()
turnOn pin = modDev "turnOn" pin $ \(DigitalOutputPin pin _ ) c_v_in sv _ ->
             [$cedecl|void $id:c_v_in()
                               { 
                                       $id:sv = HIGH;
                                       digitalWrite($int:pin, $id:sv);
                               }|]

turnOff :: forall a . (Reify a) => DigitalOutputPin -> S a -> S ()
turnOff pin = modDev "turnOff" pin $ \(DigitalOutputPin pin _ ) c_v_in sv _ ->
             [$cedecl|void $id:c_v_in()
                               { 
                                       $id:sv = LOW;
                                       digitalWrite($int:pin, $id:sv);
                               }|]

setValue :: AnalogOutputPin -> S Integer -> S ()
setValue pin = modDev "setValue" pin $ \(AnalogOutputPin pin _ ) c_v_in sv (params, e) ->
               let valueexp = e!!0
               in [$cedecl|void $id:c_v_in($params:params)
                                 {
                                   analogWrite($int:pin, $exp:valueexp);
                                 }|]

diode pin initiallyOn = DigitalOutputPin pin initiallyOn

-- It would be nice if the succint code below could be used, but we need to share the state data between toggle/turnOn/turnOff.
{-toggle d@(DigitalOutputPin pin startstate) a = S $ do
  sa <- unS a
  addDevice d
  unS (a >>> sref (uniqueId d) >>> onezero >>> digitalWrite)
    where
      onezero = sintegrate zero int
          where
            startval = if startstate then 1 else 0
            zero :: N Integer
            zero = liftN [$exp|$int:startval|]
            int :: N ((a, Integer) -> ((Integer, Integer), Integer))
            int = liftN [$decls|f (x, 0) = (($int:pin, 1), 1); f (x, 1) = (($int:pin, 0), 0)|]-}

class (Device a) => AnalogInputDevice a where
    genReadCode :: a -> String -> [Stm]
    
valueOf :: forall a d. (Reify a, AnalogInputDevice d) => d -> S a -> S Integer
valueOf d a = S $ do
    sa   <- unS a
    addDevice d
    addStream "valueOf"
              tau_b
              (DeviceRead sa $ uniqueId d)
              genHs
              genC $ \this -> do
    connect sa this tau (varIn this "")
  where
    tau :: H.Type
    tau = reify (undefined :: a)
    tau_b :: H.Type
    tau_b = reify (undefined :: Integer)

    genHs :: SCode m -> FladuinoM ()
    genHs this =
        addCImport c_v_in [$ty|$ty:tau -> ()|] [$cexp|$id:c_v_in|]
        where
          v_in    = varIn this ""
          c_v_in = show v_in

    genC :: SCode m -> FladuinoM ()
    genC this = do
        tauf <- toF tau
        tauf_out <- toF tau_b
        (params, ce_params) <- ToC.flattenParams tauf
        ce_out <-  hcall v_out $ ToC.CLowered tauf_out [$cexp|v|]
        let stms = genReadCode d "v"
        addCFundef $ [$cedecl|
void $id:c_v_in($params:params)
{
  unsigned int v;
  $stms:stms;
  $exp:ce_out;
}
|]
        where
          v_in   = varIn this ""
          c_v_in = show v_in
          v_out = s_vout this

data Potentiometer = Potentiometer Integer
                     deriving Eq

instance Device Potentiometer where
    setup _ = return ()
    deviceClass _ = "potentiometer"
    uniqueId d@(Potentiometer pin) = deviceClass d ++ (show pin)

instance AnalogInputDevice Potentiometer where
    genReadCode (Potentiometer pin) resultvar = [[$cstm|$id:resultvar = analogRead($int:pin);|]]

class (Event e t) => ValueEvent e t | e -> t where
    setupEvent :: e -> FladuinoM H.Var

connectEvent :: forall t e. (ValueEvent e t) => e -> SCode FladuinoM -> H.Var -> FladuinoM ()
connectEvent event stream v = do
  liveVar v
  eref <- lookupEvent event
  case eref of
    Just eref -> modifyFladuinoEnv $ \s ->
                              s { f_event_connections = update (eref, (stream, v)) (f_event_connections s) }
    Nothing -> do addEvent event
                  connectEvent event stream v
    where
      update :: Eq a => (a, b) -> [(a,[b])] -> [(a,[b])]
      update (key, value) [] = [(key, [value])]
      update (key, value) ((key2, values):xs)
          | (key == key2) = (key, value:values):xs
          | otherwise = (key2, values) : update (key, value) xs

      addEvent event = do v <- setupEvent event
                          modifyFladuinoEnv $ \s ->
                              s { f_events = (ERef (event, v)) : (f_events s) }
                          liveVar v

onEvent :: forall e t. (Reify t, ValueEvent e t) => e -> S t
onEvent event = S $ do
    addStream  "onEvent"
               tau_t
               (OnEvent $ show event)
               gen
               (const (return ())) $ \this -> do
    connectEvent event this (varIn this "")
  where
    tau_t :: H.Type
    tau_t = reify (undefined :: t)

    gen :: SCode m -> FladuinoM ()
    gen this = do
        addDecls [$decls|$var:v_in :: $ty:tau_t -> ()|]
        addDecls [$decls|$var:v_in x = $var:v_out x|]
      where
        v_in  = varIn this ""
        v_out = s_vout this
