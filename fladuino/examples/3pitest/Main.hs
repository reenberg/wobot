{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Prelude hiding (exp)

import Data.Loc
import Data.Name
import qualified Language.Hs.Syntax
import Language.Hs as H
import Language.Hs.Quote
import Language.C.Syntax
import Language.C.Quote
import Control.Monad.CGen
import Text.PrettyPrint.Mainland

import Fladuino
import Fladuino.Devices
import Fladuino.Pololu3pi


red_led = diode 1 False
green_led = diode 7 False

main :: IO ()
main =
    defaultMain $ do
      genStreams $ map unS [motor_control, 
                            clock 500 >>> (toggle red_led), 
                            clock 500 >>> (toggle green_led), 
                            clock 100 >>> (turnOff green_led)]
        where
          motor_control = clock 1000 >>> salternate [$exp|(255, 255)|]
                                                    [$exp|(-255, -255)|]
                                     >>> setMotors Motors


