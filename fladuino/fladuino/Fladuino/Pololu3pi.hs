{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Fladuino.Pololu3pi where

import Fladuino
import Fladuino.Devices
import Language.C.Quote
import Language.C.Syntax
import Data.Loc
import Control.Monad.CGen

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
    uniqueId _ = "motors"

setMotors :: Motors -> S (Integer, Integer) -> S ()
setMotors Motors = modDev "motors" Motors $
             \Motors c_v_in _ (params, e) -> 
                 let motor0Value = e !! 0
                     motor1Value = e !! 1
                 in [$cedecl|void $id:c_v_in($params:params)
                             { 
                               set_motors($exp:motor0Value, $exp:motor1Value);
                             }|]
{-

data ReflectanceSensor = ReflectanceSensor ReflectanceSensorArray 
                                           Integer -- The index of the sensor in the array

instance Device ReflectanceSensor where
    setupDevice _ = return ()
    deviceClass _ = "reflectance_sensor"
    uniqueId d@(ReflectanceSensor _ i) = deviceClass d ++ "_" ++ show i

instance AnalogInputDevice ReflectanceSensor where
    -}
