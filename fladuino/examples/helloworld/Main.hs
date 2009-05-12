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

import Fladuino
import Fladuino.Devices

main :: IO ()
main =
    defaultMain $ do
    genStream s
    --genStreams $ map unS [clock 500 >>> (toggle $ diode 9 True), clock 500 >>> (toggle $ diode 10 False), clock 100 >>> (turnOff $ diode 10 True)]
    where
      output = diode 10 False
      --s = smerge (clock 4 >>> sconst [$exp|0|]) (clock 1000 >>> sconst [$exp|1|]) >>> altPair 10 >>> digitalWrite
      s = clock 1 >>> vary >>> (setValue $ AnalogOutputPin 10 0)

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

          int :: N ((Integer, (Integer, Integer)) -> ((Integer, Integer), (Integer, Integer)))
          -- The version below would work if pattern-matching wasn't broken.
          --int = liftN [$decls|f (1, (1,x)) = (($int:pin,0), (0,x)); f (1, (0,x)) = (($int:pin,0), (1,x)); f (0, (1,1)) = (($int:pin,0), (1,0)); f (0, (1,0)) = (($int:pin,1), (1,1)); f (0, (0,x)) = (($int:pin,0), (0,x))|]
          -- But we have to make do with this shit.
          int = liftN [$decls|f (change, (enabled, state)) = if (change == 1) then if (enabled == 1) then (($int:pin,0),(0,state)) else (($int:pin,0),(1,state)) else if (enabled == 1) then if (state == 1) then (($int:pin,0),(1,0)) else (($int:pin,1),(1,1)) else (($int:pin,0),(0,state))|]
          -- Also, newlines cause parse errors.  Fun fun!
