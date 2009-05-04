{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Prelude hiding (exp)

import Fladuino
import Fladuino.Device
import Fladuino.Devices


main :: IO ()
main =
    defaultMain $ 
    genStreams (map unS [onEvent (PushButtonPressEvent $ PushButton 2) >>> turnOn (DigitalOutputPin 8 False),
                         onEvent (PushButtonReleaseEvent $ PushButton 2) >>> turnOff (DigitalOutputPin 8 False)])
