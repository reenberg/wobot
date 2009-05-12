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

secondsPerUnit = 15*60 -- One quarter of an hour.
increaseButton = PushButton 2
decreaseButton = PushButton 3
increase       = onPress increaseButton >>> sconst [$exp|$int:secondsPerUnit|]
decrease       = onPress decreaseButton >>> sconst [$exp|-1 * $int:secondsPerUnit|]
clockInts      = clock 1000 >>> sconst [$exp|-1|]
alarm          = AnalogOutputPin 9 0

main :: IO ()
main =
    defaultMain $ do
    addCImport "isBitSet" [$ty|(Integer, Integer) -> Bool|] [$cexp|isBitSet|]
    addCFundef [$cedecl|int isBitSet(int x, int bit) {
                                  return (x & _BV(bit));
                                }|]
    genStreams $ map unS (streamsForCounter 4 4)
        
          
streamsForCounter :: Integer -> Integer -> [S ()]
streamsForCounter n sp = (interaction >>> maybeBeep)
                         : [ interaction >>> smap [$exp|(/ $int:secondsPerUnit)|] >>> s n |
                             s <- [maybeTurnOn, maybeTurnOff], n <- [0..n] ]
    where
      interaction = smerge (smerge increase decrease) clockInts >>> modNum
      
      modNum :: S Integer -> S Integer
      modNum = sintegrate zero int
          where
            zero :: N Integer
            zero = liftN [$exp|0|]

            int  :: N ((Integer, Integer) -> (Integer, Integer))
            int  = liftN [$decls|f (n, x) = (bounded(0, x + n, $int:maxNum), bounded(0, x + n, $int:maxNum))|]
            
            maxNum = (2^n-1)*secondsPerUnit


      maybeBeep :: S Integer -> S ()
      maybeBeep from = from >>> smap [$exp|(\x -> if x==0 then 128 else 0)|] >>> setValue alarm

      bitStatus :: Integer -> S Integer -> S Bool
      bitStatus i = smap [$exp|\p -> isBitSet(p, $int:i)|]

      maybeTurnOn :: Integer -> S Integer -> S ()
      maybeTurnOn i from = from >>> bitStatus i >>> sfilter [$exp|id|] 
                           >>> turnOn (diode (sp+i) False)

      maybeTurnOff :: Integer -> S Integer -> S ()
      maybeTurnOff i from = from >>> bitStatus i 
                            >>> sfilter [$exp|(\v -> not v)|] 
                            >>> turnOff (diode (sp+i) False)
