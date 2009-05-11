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
    addCImport "isBitSet" [$ty|(Integer, Integer) -> Bool|] [$cexp|isBitSet|]
    addCFundef [$cedecl|int isBitSet(int x, int bit) {
                                  return (x & _BV(bit));
                                }|]
    genStreams $ map unS (streamsForCounter 8 2)
        
          
streamsForCounter :: Integer -> Integer -> [S ()]
streamsForCounter n sp = [ clocked >>> s n | s <- [maybeTurnOn, maybeTurnOff], n <- [0..n-1]]
    where
      modNum :: S Integer -> S Integer
      modNum = sintegrate zero int
          where
            zero :: N Integer
            zero = liftN [$exp|0|]

            int :: N ((Integer, Integer) -> (Integer, Integer))
            int = liftN [$decls|f (n, x) = (x + n, x + n)|]

      bitStatus :: Integer -> S Integer -> S Bool
      bitStatus i = smap [$exp|\p -> isBitSet(p, $int:i)|]

      maybeTurnOn :: Integer -> S Integer -> S ()
      maybeTurnOn i from = from >>> bitStatus i >>> sfilter [$exp|id|] 
                           >>> turnOn (diode (sp+i) False)

      maybeTurnOff :: Integer -> S Integer -> S ()
      maybeTurnOff i from = from >>> bitStatus i 
                            >>> sfilter [$exp|(\v -> not v)|] 
                            >>> turnOff (diode (sp+i) False)

      s1 = onPress $ PushButton 10 >>> sconst [$exp|1|]
      s2 = onPress $ PushButton 11 >>> sconst [$exp|-1|]
      s = smerge s1 s2
      clocked = smerge s (clock 1 
                          >>> (valueOf $ Potentiometer 0) 
                          >>> smap [$exp|(/5)|] 
                          >>> clockSkip 
                          >>> sconst [$exp|1|]) 
                >>> modNum


-- | Slows down the clock based on the input stream. The input stream
-- | indicates how many clock cycles to skip.
clockSkip :: S Integer -> S Integer
clockSkip from = from 
                 >>> sintegrate start count
                 >>> sfilter predicate
    where
      start :: N Integer
      start = liftN [$exp|0|]
      count :: N ((Integer, Integer) -> (Integer, Integer))
      count = liftN [$exp|\(x, s) -> if s > x then (0, 0) 
                                     else (s+1, s+1)|]
      predicate :: N (Integer -> Bool)
      predicate = liftN [$exp|\s -> s == 0|]
