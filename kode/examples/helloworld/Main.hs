{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (exp)

import Data.Loc
import Data.Name
import Text.PrettyPrint.Mainland
import qualified Language.Hs.Syntax
import Language.Hs.Quote

import Flask

main :: IO ()
main =
    defaultMain $ do
    genStreams $ map unS [s, s2]
  where
    s :: S ()
    s = clock 500 >>> altPair 10 >>> digitalWrite
    s2 = clock 300 >>> altPair 10 >>> digitalWrite

    onezero :: forall a . Reify a => S a -> S Float
    onezero =  sintegrate zero int
      where
        zero :: N Integer
        zero = liftN [$exp|0|]

        int :: N ((a, Integer) -> (Float, Integer))
        int = liftN [$decls|f (x, 0) = (1.0, 1); f (x, 1) = (0.0, 0)|]

    altPair :: forall a . Reify a => Integer -> S a -> S (Integer, Integer)
    altPair pin = sintegrate zero int
        where
          zero :: N Integer
          zero = liftN [$exp|0|]

          int :: N ((a, Integer) -> ((Integer, Integer), Integer))
          int = liftN [$decls|f (x, 0) = (($int:pin,1), 1); f (x, 1) = (($int:pin,0), 0)|]

    ewma :: Double -> S Float -> S Float
    ewma alpha = sintegrate zero int
      where
        one_minus_alpha :: Double
        one_minus_alpha = 1.0 - alpha

        zero :: N Float
        zero = liftN [$exp|0.0|]

        int :: N ((Float, Float) -> (Float, Float))
        int = liftN [$exp|\(x, prev) -> let cur = ($flo:alpha*x) + ($flo:one_minus_alpha*prev)
                                        in (cur, cur)|]

    clk :: S ()
    clk = clock 500
