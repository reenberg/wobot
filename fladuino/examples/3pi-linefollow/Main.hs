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
import Fladuino.Device
import Fladuino.Devices
import Fladuino.Pololu3pi


speed = 0.5

main :: IO ()
main =
    defaultMain $ do
      addCImport "intToFloat" [$ty|Integer -> Float|] [$cexp|intToFloat|]
      addCFundef [$cedecl|float intToFloat(int x) {
                                  return ((float)x);
                          }|]
      genStream $ idle 
                  >>> valueOf ReflectanceSensors
                  >>> smap velocity
                  >>> set_motors_rt Motors
          where 
            velocity :: N (Integer -> (Float, Float))
            velocity = liftN [$exp|\linepos -> ($flo:speed, 
                                                (intToFloat (2000 - linepos)) / 2000.0)|]

