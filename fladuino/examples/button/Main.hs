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
    genStreams (map unS [onEvent (PushButtonPressEvent $ PushButton 2) >>> toggle (DigitalOutputPin 8 False),
                         onEvent (PushButtonReleaseEvent $ PushButton 2) >>> toggle (DigitalOutputPin 9 True)])
