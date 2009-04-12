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
    modifyFlaskEnv $ \s -> s { f_output_pin = 14 }
    genStream s
  where
    s :: S ()
    s = clk >>> onezero >>> ewma 0.5 >>> sink

    onezero :: forall a . Reify a => S a -> S Float
    onezero =  sintegrate zero int
      where
        zero :: N Integer
        zero = liftN [$exp|0|]

        int :: N ((a, Integer) -> (Float, Integer))
        int = liftN [$decls|f (x, 0) = (1.0, 1); f (x, 1) = (0.0, 0)|]

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
    clk = clock 10
