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
      genStream $ idle 
                  >>> valueOf ReflectanceSensors
                  >>> smap velocity 
                  >>> set_motors Motors
          where 
            velocity :: N (Integer -> (Float, Float))
            velocity = liftN [$decls|f linepos = if linepos < 1000 then (0.0-1.0, $flo:speed)
                                                 else if linepos > 3000 then (0.0, $flo:speed)
                                                 else (1.0, 0.0)|]
