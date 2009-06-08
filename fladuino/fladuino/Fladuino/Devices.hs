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
-- Module      :  Fladuino.Devices
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  athas@sigkill.dk, jesper.reenberg@gmail.com, dybber@dybber.dk
--
--------------------------------------------------------------------------------

module Fladuino.Devices where

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
import Fladuino.Device

data DigitalOutputPin = DigitalOutputPin Integer Bool
           deriving (Eq, Show)

data AnalogOutputPin = AnalogOutputPin Integer Integer
           deriving (Eq, Show)

data DigitalInputPin = DigitalInputPin Integer
           deriving (Eq, Show)

data AnalogInputPin = AnalogInputPin Integer
           deriving (Eq, Show)

instance Device DigitalOutputPin where
    setupDevice d@(DigitalOutputPin pin initstate) = 
        do sv <- statevar d "state"
           addCDecldef [$cedecl|unsigned char $id:sv;|]
           addCInitStm [$cstm|$id:sv = digitalRead($int:pin);|]
    usages (DigitalOutputPin pin initstate) = [DPinUsage pin [] (DigitalOutput initstate)]

instance Device DigitalInputPin where
    usages (DigitalInputPin pin) = [DPinUsage pin [] DigitalInput]

instance Device AnalogOutputPin where
    usages (AnalogOutputPin pin initstate) = [DPinUsage pin ["PWM"] (AnalogOutput initstate)]

modDev :: forall a d. (Reify a, Device d) => String -> d -> (d -> String -> String -> ([Param], [Exp]) -> Definition) -> S a -> S ()
modDev name d f a = S $ do
    sa   <- unS a
    addStream name
              unitTy
              (DeviceWrite sa $ uniqueId d ++ "_" ++ name)
              genHs
              genC $ \this -> do
    addDevice d
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

toggle :: forall a. (Reify a) => DigitalOutputPin -> S a -> S ()
toggle pin = modDev "toggle" pin $ \(DigitalOutputPin pin _ ) c_v_in sv (params, e) ->
             [$cedecl|void $id:c_v_in($params:params)
                               { 
                                       if ($id:sv == HIGH) $id:sv = LOW;
                                       else $id:sv = HIGH;
                                       digitalWrite($int:pin, $id:sv);
                               }|]

turnOn :: forall a. (Reify a) => DigitalOutputPin -> S a -> S ()
turnOn pin = modDev "turnOn" pin $ \(DigitalOutputPin pin _ ) c_v_in sv (params, e) ->
             [$cedecl|void $id:c_v_in($params:params)
                               { 
                                       $id:sv = HIGH;
                                       digitalWrite($int:pin, $id:sv);
                               }|]

turnOff :: forall a. (Reify a) => DigitalOutputPin -> S a -> S ()
turnOff pin = modDev "turnOff" pin $ \(DigitalOutputPin pin _ ) c_v_in sv (params, e) ->
             [$cedecl|void $id:c_v_in($params:params)
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
    addStream "valueOf"
              tau_b
              (DeviceRead sa $ uniqueId d)
              genHs
              genC $ \this -> do
    addDevice d
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

-- Potentiometer device --
data Potentiometer = Potentiometer Integer
                     deriving (Eq, Show)

instance Device Potentiometer where
    usages (Potentiometer pin) = [APinUsage pin []]

instance AnalogInputDevice Potentiometer where
    genReadCode (Potentiometer pin) resultvar = [[$cstm|$id:resultvar = analogRead($int:pin);|]]


-- This actually make no sence, as the atmega interrupt system is not sensitive
-- enough.  It only triggers logical change interrupts when the messured voltage
-- changes aprox > 50% of the supply voltage.
data PotentiometerChangeEvent = PotentiometerChangeEvent Potentiometer
                                deriving (Eq, Show)

instance Event PotentiometerChangeEvent () where
    setupEvent e@(PotentiometerChangeEvent (d@(Potentiometer pin))) = 
                                                 return $ mkEvent e Nothing Nothing
    interruptPins (PotentiometerChangeEvent (Potentiometer pin)) = [APin pin]
                   

-- Simple Interrupt Event --
data InterruptEvent = InterruptEvent Integer
                    deriving (Eq, Show)
instance Event InterruptEvent () where
    interruptPins (InterruptEvent n) = [DPin n]
    setupEvent e = return $ mkEvent e Nothing Nothing

-- Button device and Events --
data PushButton = PushButton Integer
                  deriving (Eq, Show)

instance Device PushButton where
    usages (PushButton pin) = [DPinUsage pin ["interrupt"] DigitalInput]

data PushButtonPressEvent = PushButtonPressEvent PushButton
                            deriving (Eq, Show)

instance Event PushButtonPressEvent () where
    setupEvent e@(PushButtonPressEvent (d@(PushButton pin))) = 
                                            do addDevice d
                                               pv <- statevar d "press_predicate" 
                                               let v = H.Var (mkName pv)
                                               addCImport pv [$ty|() -> Bool|] [$cexp|$id:pv|]
                                               addCFundef [$cedecl|int $id:pv () {
                                                                     return (digitalRead($int:pin) == HIGH);
                                                                   }|]
                                               return $ mkEvent e Nothing (Just v)
    interruptPins (PushButtonPressEvent (PushButton pin)) = [DPin pin]

data PushButtonReleaseEvent = PushButtonReleaseEvent PushButton
                              deriving (Eq, Show)

instance Event PushButtonReleaseEvent () where
    setupEvent e@(PushButtonReleaseEvent (d@(PushButton pin))) = 
                                              do addDevice d 
                                                 pv <- statevar d "rel_predicate" 
                                                 let v = H.Var (mkName pv)
                                                 addCImport pv [$ty|() -> Bool|] [$cexp|$id:pv|]
                                                 addCFundef [$cedecl|int $id:pv () {
                                                                       return (digitalRead($int:pin) == LOW);
                                                                     }|]
                                                 return $ mkEvent e Nothing (Just v)
    interruptPins (PushButtonReleaseEvent (PushButton pin)) = [DPin pin]

class (Device d) => Button d where
    onPress   :: d -> S ()
    onRelease :: d -> S ()

instance Button PushButton where
    onPress   = onEvent . PushButtonPressEvent
    onRelease = onEvent . PushButtonReleaseEvent

-- Simple unidirectional data ports --

data DigitalOutputPort = DigitalOutputPort Integer [Integer]
                         deriving (Eq, Show)

instance Device DigitalOutputPort where
    usages (DigitalOutputPort controlpin datapins) = (DPinUsage controlpin [] $ DigitalOutput False) :
                                                     map (\pin -> DPinUsage pin [] $ DigitalOutput False) datapins

writeValue :: DigitalOutputPort -> S Integer -> S ()
writeValue pin = modDev "writeValue" pin $ \(DigitalOutputPort controlpin datapins) c_v_in sv (params, e) ->
                 let valueexp = e!!0
                     stms = map (\(n, pin) -> [$cstm|digitalWrite($int:pin, ($exp:valueexp >> $int:n) & 1 ? HIGH : LOW);|]) $ zip [0..] (reverse datapins)
                 in [$cedecl|void $id:c_v_in($params:params)
                                 {
                                   digitalWrite($int:controlpin, LOW);
                                   $stms:stms
                                   digitalWrite($int:controlpin, HIGH);
                                 }|]

data DigitalInputPort = DigitalInputPort Integer [Integer]
                        deriving (Eq, Show)

instance Device DigitalInputPort where
    usages (DigitalInputPort controlpin datapins) = 
                       (DPinUsage controlpin ["interrupt"] DigitalInput) :
                       map (\pin -> DPinUsage pin [] DigitalInput) datapins

data ValueReceivedEvent = ValueReceivedEvent DigitalInputPort
                         deriving (Eq, Show)

instance Event ValueReceivedEvent Integer where
    setupEvent e@(ValueReceivedEvent (d@(DigitalInputPort controlpin datapins))) = 
         do addDevice d
            pv <- statevar d "receive_predicate"
            vv <- statevar d "receive_value"
            let pf = H.Var (mkName pv)
            let vf = H.Var (mkName vv)
            addCImport pv [$ty|() -> Bool|] [$cexp|$id:pv|]
            addCFundef [$cedecl|int $id:pv () {
                                  return (digitalRead($int:controlpin) == HIGH);
                                }|]
            addCImport vv [$ty|() -> Integer|] [$cexp|$id:vv|]
            let stms = map (\pin -> [$cstm|ret = ((ret << 1) | digitalRead($int:pin));|]) 
                       . sort $ datapins
            addCFundef [$cedecl|unsigned int $id:vv () {
                                  unsigned int ret = 0;
                                  $stms:stms
                                  return ret;
                                }|]
            return $ mkEvent e (Just vf) (Just pf)
    interruptPins (ValueReceivedEvent (d@(DigitalInputPort controlpin _))) = [DPin controlpin]
