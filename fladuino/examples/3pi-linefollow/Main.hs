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

sensors = ReflectanceSensorArray 5
linereader = LineReader sensors
speed = 100

main :: IO ()
main =
    defaultMain $ do
      addDevice sensors
      genStream $ idle 
                  >>> valueOf linereader 
                  >>> smap velocity 
                  >>> set_motors_native Motors
          where 
            velocity :: N (Integer -> (Integer, Integer))
            velocity = liftN [$decls|f linepos = if linepos < 1000 then (0, $int:speed)
                                                 else if linepos > 3000 then ($int:speed, $int:speed)
                                                 else ($int:speed, 0)|]
