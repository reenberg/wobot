{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Fladuino.Pololu3pi where

import Prelude hiding (exp)

import Language.C.Quote
import Language.C.Syntax
import qualified Language.Hs.Syntax
import Language.Hs as H
import Language.Hs.Quote
import Data.Loc
import Data.Name
import Control.Monad.CGen
import Text.PrettyPrint.Mainland

import Fladuino
import Fladuino.Devices

-- Device for reading battery level in 3pi robots --
data BatteryReader = BatteryReader
                        deriving (Eq, Show)

instance Device BatteryReader where
    usages _ = [CapabilityRequired "3pi battery reader"]
    uniqueId _ = "3pi_battery_reader" -- there only need to be one

instance AnalogInputDevice BatteryReader where
    genReadCode _ resultvar = [[$cstm|$id:resultvar = read_battery_millivolts();|]]


-- Device for reading 3pi reflection sensors --
data ReflectanceSensorArray = ReflectanceSensorArray 
                                  Integer -- ^ Number of Sensors in array (5 for 3pi)
                              deriving (Eq, Show)

instance Device ReflectanceSensorArray where
    setupDevice (ReflectanceSensorArray n) = 
        do addCVardef [$cedecl|unsigned int $id:arrayname[$int:n];|]
           addCInitStm [$cstm|calibrate_line_sensors(IR_EMITTERS_ON);|]                                                 
        where arrayname = "sensors" ++ show n
    uniqueId _ = "reflectance_sensor_array" -- only one should be needed


calibrateSensors :: forall a. (Reify a) => ReflectanceSensorArray -> S a -> S ()
calibrateSensors d@(ReflectanceSensorArray n) = modDev "calibrateSensors" d $
                  \_ c_v_in _ (params, _) ->
                         [$cedecl|void $id:c_v_in($params:params)
                          { 
                            calibrate_line_sensors(IR_EMITTERS_ON);
                          }|]


data LineReader = LineReader ReflectanceSensorArray
                  deriving (Eq, Show)

instance Device LineReader where
    uniqueId _ = "line_reader"

instance AnalogInputDevice LineReader where
    genReadCode (LineReader (ReflectanceSensorArray n)) resultvar = 
        [[$cstm|$id:resultvar = read_line($id:arrayname, IR_EMITTERS_ON);|]]
        where arrayname = "sensors" ++ show n

data Motors = Motors
              deriving (Eq, Show)

instance Device Motors where
    setupDevice Motors = addCInclude "pololu/motors.h"
    uniqueId _ = "motors"

-- This just calls the pololu set_motors function with the integers on
-- the input as argument
set_motors_native :: Motors -> S (Integer, Integer) -> S ()
set_motors_native Motors = modDev "motors" Motors $
             \Motors c_v_in _ (params, e) -> 
                 let motor0Value = e !! 0
                     motor1Value = e !! 1
                 in [$cedecl|void $id:c_v_in($params:params)
                             { 
                               set_motors($exp:motor0Value, $exp:motor1Value);
                             }|]
-- This interprets the two integers on the input as rotational and 
set_motors :: Motors -> S (Float, Float) -> S ()
set_motors Motors from = from >>> f >>> set_motors_native Motors
    where
      f :: S (Float, Float) -> S (Integer, Integer)
      f = smap computeSpeeds
      computeSpeeds :: N ((Float, Float) -> (Integer, Integer))
      computeSpeeds = liftN [$decls|
compute (v, omega) = let motorLvel = v * (if omega <= 0.0 then 1.0 else 1.0 - omega)
                         motorRvel = v * (if omega >= 0.0 then 1.0 else 0.0 - omega)
                     in (round (motorLvel * 255.0), round (motorRvel * 255.0))
                       |]

{-

data ReflectanceSensor = ReflectanceSensor ReflectanceSensorArray 
                                           Integer -- The index of the sensor in the array

instance Device ReflectanceSensor where
    setupDevice _ = return ()
    deviceClass _ = "reflectance_sensor"
    uniqueId d@(ReflectanceSensor _ i) = deviceClass d ++ "_" ++ show i

instance AnalogInputDevice ReflectanceSensor where
    -}
