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
import Data.Maybe
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

data Pin = Pin Integer [Capability]

data Platform = Platform 
    { p_digital_pins :: [Pin]
    , p_analog_pins :: [Pin]
    , p_capabilities :: [Capability] }

emptyPlatform :: Platform
emptyPlatform = Platform { p_digital_pins = []
                         , p_analog_pins = []
                         , p_capabilities = [] }

duemilanove = emptyPlatform { p_digital_pins = [ Pin n $ pc n | n <- [0..13] ]
                            , p_analog_pins = [ Pin n ["analog", "interrupt"] | n <- [0..6] ]
                            , p_capabilities = ["Duemilanove"] }
              where
                pc pin | pin `elem` [3, 5, 6, 9, 10, 11] = ["PWM", "interrupt"]
                pc _ = ["interrupt"]

diecimila = duemilanove { p_capabilities = ["Diecimila"] }

data PConf = PConf Platform [Usage]

startConf :: Platform -> PConf
startConf p = PConf p []

supports :: PConf -> Usage -> Bool
supports (PConf p _) (DPinUsage pin caps _) = isJust $ find f (p_digital_pins p)
    where f (Pin pin' caps') = pin == pin' && null (caps \\ caps')
supports (PConf p _) (APinUsage pin caps _) = isJust $ find f (p_analog_pins p)
    where f (Pin pin' caps') = pin == pin' && null (caps \\ caps')

conflicts :: PConf -> Usage -> Bool
conflicts (PConf _ us) (DPinUsage pin _ _) = isJust $ find f us
    where f (DPinUsage pin' _ _) = pin == pin'
          f _ = False
conflicts (PConf _ us) (APinUsage pin _ _) = isJust $ find f us
    where f (APinUsage pin' _ _) = pin == pin'
          f _ = False
conflicts _ _ = False

extendConfiguration :: MonadFladuino m => PConf -> [Usage] -> m PConf
extendConfiguration pc@(PConf p _) (CapabilityRequired cap : us)
    | cap `elem` p_capabilities p = extendConfiguration pc us
    | otherwise = fail $ "Selected platform does not supply required '" ++ cap ++ " capability"
extendConfiguration pc@(PConf p usages) (u@(DPinUsage pin caps _) : us)
    | pc `conflicts` u = fail $ "Digital pin " ++ show pin ++ " reserved twice"
    | pc `supports` u = extendConfiguration (PConf p $ u : usages) us
    | otherwise = fail $ "Cannot provide digital pin " ++ show pin ++ " with capabilities " ++ show caps
extendConfiguration pc@(PConf p usages) (u@(APinUsage pin caps _) : us)
    | pc `conflicts` u = fail $ "Analog pin " ++ show pin ++ " reserved twice"
    | pc `supports` u = extendConfiguration (PConf p $ u : usages) us
    | otherwise = fail $ "Cannot provide analog pin " ++ show pin ++ " with capabilities " ++ show caps
extendConfiguration pc [] = return pc

statevar :: Device d => MonadFladuino m => d -> String -> m String
statevar d s = do
  devices <- getsFladuinoEnv f_devices
  case findIndex (\(DRef d2) -> uniqueId d2 == uniqueId d) devices of
    Just n -> return $ "device_" ++ (uniqueId d) ++ s
    Nothing -> return $ error $ "Unknown device " ++ uniqueId d ++ " encountered."
    
                      
class (Eq e, Show e, Reify t) => Event e t | e -> t where
    interruptPins :: e -> [Integer]
    setupEvent :: e -> FladuinoM (ERep FladuinoM)

eventValueType :: forall e t. (Event e t) => e -> H.Type
eventValueType _ = reify (undefined :: t)

mkEvent :: forall e t. (Event e t) => e -> Maybe H.Var -> Maybe H.Var -> ERep FladuinoM
mkEvent e val pred = ERep { e_value = val
                          , e_predicate = pred
                          , e_id = show e
                          , e_interrupts = interruptPins e
                          , e_type = reify (undefined :: t) }

lookupEvent :: forall t e m. (Event e t, MonadFladuino m) => e -> m (Maybe (ERep m))
lookupEvent event = do 
  events <- getsFladuinoEnv f_events 
  case find (\(ERep { e_id = id}) -> show event == id) events of
    Just erep  -> return $ Just erep
    Nothing -> return Nothing

connectEvent :: forall t e. (Event e t) => e -> SCode FladuinoM -> H.Var -> FladuinoM ()
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

      addEvent event = do erep <- setupEvent event
                          let erep' = erep { e_type = eventValueType event }
                          modifyFladuinoEnv $ \s ->
                              s { f_events = erep' : (f_events s) }
                          liveVar v

onEvent :: forall e t. (Reify t, Event e t) => e -> S t
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
