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

sensorarrayname (ReflectanceSensorArray n) = "sensors" ++ show n

instance Device ReflectanceSensorArray where
    usages _ = [CapabilityRequired "3pi reflectance sensor array"]
    setupDevice rsa@(ReflectanceSensorArray n) = 
        do addCDecldef [$cedecl|unsigned int $id:aname[$int:n];|]
           addCFundef [$cedecl|void calibrate_3pi_sensors () {              
                        int counter;
                        for(counter=0;counter<80;counter++) {
                            if(counter < 20 || counter >= 60)
                              set_motors(40,-40);
                            else
                              set_motors(-40,40);
                            calibrate_line_sensors(IR_EMITTERS_ON);
                            delay_ms(20);
                        }
                        set_motors(0,0);
                        }|]

           addCInitStm [$cstm|pololu_3pi_init(2000);|]
           addCInitStm [$cstm|while(!button_is_pressed(BUTTON_B));|]
           addCInitStm [$cstm|calibrate_3pi_sensors ();|]
           addCInitStm [$cstm|while(!button_is_pressed(BUTTON_B));|]
        where aname = sensorarrayname rsa
    uniqueId _ = "reflectance_sensor_array" -- only one should be needed

calibrateSensors :: forall a. (Reify a) => ReflectanceSensorArray -> S a -> S ()
calibrateSensors d@(ReflectanceSensorArray n) = modDev "calibrateSensors" d $
                  \_ c_v_in _ (params, _) ->
                         [$cedecl|void $id:c_v_in($params:params)
                          { 
                            calibrate_3pi_sensors ();
                          }|]


data LineReader = LineReader ReflectanceSensorArray
                  deriving (Eq, Show)

instance Device LineReader where
    usages _ = [CapabilityRequired "3pi reflectance sensor array"]
    uniqueId _ = "line_reader"

instance AnalogInputDevice LineReader where
    genReadCode (LineReader rsa@(ReflectanceSensorArray n)) resultvar = 
        [[$cstm|$id:resultvar = read_line($id:aname, IR_EMITTERS_ON);|]]
        where aname = sensorarrayname rsa

data Motors = Motors
              deriving (Eq, Show)

instance Device Motors where
    usages _ = [CapabilityRequired "3pi motors"]
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
-- transitional speed.  The first element of the pair is rotational
-- speed, the last is speed.
set_motors :: Motors -> S (Float, Float) -> S ()
set_motors Motors from = from >>> f >>> set_motors_native Motors
    where
      f :: S (Float, Float) -> S (Integer, Integer)
      f = smap computeSpeeds
      computeSpeeds :: N ((Float, Float) -> (Integer, Integer))
      computeSpeeds = liftN [$decls|
compute (v, omega) = let motorLvel = v * (if omega <= 0.0 then 1.0 else 1.0 - omega)
                         motorRvel = v * (if omega >= 0.0 then 1.0 else 1.0 + omega)
                     in (round (motorLvel * 255.0), round (motorRvel * 255.0))
                       |]
