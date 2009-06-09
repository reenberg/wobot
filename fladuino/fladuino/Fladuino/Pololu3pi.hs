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


-- Device for reading and calibrating 3pi reflection sensors --
data ReflectanceSensors = ReflectanceSensors
                              deriving (Eq, Show)

sensorArrayName :: ReflectanceSensors -> String
sensorArrayName _ = "sensors_3pi"

sensorArraySize :: ReflectanceSensors -> Integer
sensorArraySize _ = 5

instance Device ReflectanceSensors where
    usages _ = [CapabilityRequired "3pi reflectance sensors"]
    setupDevice sens = 
        do addCDecldef [$cedecl|unsigned int $id:arrayName[$int:arraySize];|]
           addCFundef [$cedecl|
                        void calibrate_3pi_sensors () {              
                          int counter;
                          while(!button_is_pressed(BUTTON_B));
                          for(counter=0;counter<80;counter++) {
                              if(counter < 20 || counter >= 60)
                                set_motors(40,-40);
                              else
                                set_motors(-40,40);
                              calibrate_line_sensors(IR_EMITTERS_ON);
                              delay_ms(20);
                          }
                          set_motors(0,0);
                          while(!button_is_pressed(BUTTON_B));
                        }|]

           addCInitStm [$cstm|pololu_3pi_init(2000);|]
           addCInitStm [$cstm|calibrate_3pi_sensors ();|]
        where arrayName = sensorArrayName sens
              arraySize = sensorArraySize sens

    uniqueId _ = "reflectance_sensor_array" -- only one should be needed

calibrateSensors :: forall a. (Reify a) => ReflectanceSensors -> S a -> S ()
calibrateSensors d = modDev "calibrateSensors" d $
                  \_ c_v_in _ (params, _) ->
                         [$cedecl|void $id:c_v_in($params:params)
                          { 
                            calibrate_3pi_sensors ();
                          }|]

instance AnalogInputDevice ReflectanceSensors where
    genReadCode sens resultvar = 
        [[$cstm|$id:resultvar = read_line($id:arrayName, IR_EMITTERS_ON);|]]
        where 
          arrayName = sensorArrayName sens

data Motors = Motors
              deriving (Eq, Show)

instance Device Motors where
    setupDevice _ = do addCInclude "math.h"
                       addCImport "round" [$ty|Float -> Integer|] [$cexp|round|]
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

-- This interprets the two floats on the input as rotational and
-- transitional velocity, respectively.
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
