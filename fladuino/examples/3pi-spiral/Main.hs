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
import Fladuino.Devices
import Fladuino.Pololu3pi


stepSize = 0.01
speed = 0.5
rotationLimit = 0.5


main :: IO ()
main =
    defaultMain $ do
      addCInclude "math.h"
      addCImport "round" [$ty|Float -> Integer|] [$cexp|round|]
      genStream $ clock 1 >>> smooth
                             >>> (smap [$decls|f x = ($flo:speed, x)|] :: S Float -> S (Float, Float))
                             >>> set_motors Motors
          where 
            smooth  :: S () -> S Float
            smooth = sintegrate startingState update
            startingState :: N (Float, Bool)
            startingState = liftN [$exp|(0.0, True)|] -- A True state means going right.
            update :: N (((), (Float, Bool)) -> (Float, (Float, Bool)))
            update = liftN [$decls|
f (_, (x, True)) | x <= 0.0 - $flo:rotationLimit =   (x1, (x1, False)) where x1 = x - $flo:stepSize
f (_, (x, True)) =              (x1, (x1, True))  where x1 = x - $flo:stepSize
f (_, (x, False)) | x >= $flo:rotationLimit = (x1, (x1, True))  where x1 = x + $flo:stepSize
f (_, (x, False)) =             (x1, (x1, False)) where x1 = x + $flo:stepSize|]


{-

main :: IO ()
main =
    defaultMain $ do
      genStream $ clock 1000 >>> (salternate ([$exp|(255, -255)|]) ([$exp|(-255, 255)|]))
                          >>> set_motors_native Motors
-}
