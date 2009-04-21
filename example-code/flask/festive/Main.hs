{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (exp)

import Data.Loc
import Data.Name
import qualified Language.Hs.Syntax
import Language.Hs.Quote
import Language.C.Syntax
import Language.C.Quote
import Control.Monad.CGen
import Text.PrettyPrint.Mainland

import Flask
import Flask.Device

main :: IO ()
main =
    defaultMain $ do
    genStreams $ map unS [s1, s2, s3]
    where
      output = diode 10 False
      s1 = clock 500 >>> (toggle $ diode 8 True)
      s2 = clock 500 >>> (toggle $ diode 9 False)
      s3 = clock 1 >>> vary >>> (setValue $ AnalogOutputPin 10 0)

      vary :: forall a . Reify a => S a -> S Integer
      vary =  sintegrate zero int
          where
            zero :: N Integer
            zero = liftN [$exp|0|]

            int :: N ((a, Integer) -> (Integer, Integer))
            int = liftN [$decls|f (x, 512) = (0, 0); f (x, state) = if (state >= 256) then (256-(state-256), state + 1) else (state, state + 1)|]

      altPair :: Integer -> S Integer -> S (Integer, Integer)
      altPair pin = sintegrate zero int
        where
          zero :: N (Integer, Integer)
          zero = liftN [$exp|(1,0)|]
