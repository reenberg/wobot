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

main :: IO ()
main =
    defaultMain $ do
    addCInclude "math.h"
    addCImport "isBitSet" [$ty|(Integer, Integer) -> Bool|] [$cexp|isBitSet|]
    addCFundef [$cedecl|int isBitSet(int x, int bit) {
                                  return (x & (1 << bit));
                                }|]
    genStreams ((map unS . concat) $ map (\n -> [s >>> maybeTurnOn n, s >>> maybeTurnOff n]) [0..3])
        where
          modNum :: S Integer -> S Integer
          modNum = sintegrate zero int
              where
                zero :: N Integer
                zero = liftN [$exp|0|]

                int :: N ((Integer, Integer) -> (Integer, Integer))
                int = liftN [$decls|f (n, x) = (x + n, x + n)|]

          ifBitOn :: Integer -> S Integer -> S Integer
          ifBitOn n = sfilter [$exp|\p -> isBitSet(p, $int:n)|]

          ifBitOff :: Integer -> S Integer -> S Integer
          ifBitOff n = sfilter [$exp|\p -> isBitSet(p, $int:n)|]

          maybeTurnOn :: Integer -> S Integer -> S ()
          maybeTurnOn n from = from >>> ifBitOn n >>> turnOn (diode (5+n) False)

          maybeTurnOff :: Integer -> S Integer -> S ()
          maybeTurnOff n from = from >>> ifBitOff n >>> turnOff (diode (5+n) False)

          displayBit :: Integer -> S Integer -> S ()
          displayBit n from = from >>> (maybeTurnOn n)

          s1 = onEvent (PushButtonPressEvent $ PushButton 2) >>> sconst [$exp|1|]
          s2 = onEvent (PushButtonReleaseEvent $ PushButton 3) >>> sconst [$exp|-1|]
          s = smerge s1 s2 >>> modNum
