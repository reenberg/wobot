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
    defaultMain $
    genStreams ((map unS . concat . map (\s -> map (\d -> s >>> d) (map displayBit [1..4]))) [s1, s2])
        where
          modNum :: S Integer -> S Integer
          modNum = sintegrate zero int
              where
                zero :: N Integer
                zero = liftN [$exp|0|]

                int :: N ((Integer, Integer) -> (Integer, Integer))
                int = liftN [$decls|f (n, x) = (x + n, x + n)|]

          ifBitOn :: Integer -> S Integer -> S Integer
          ifBitOn n = sfilter [$decls|p (x) = True|]

          displayBit :: Integer -> S Integer -> S ()
          displayBit n from = from >>> ifBitOn n >>> toUnit >>> turnOn (diode (4+n) False)

          s1 = onEvent (PushButtonPressEvent $ PushButton 2) >>> sconst [$exp|1|] >>> modNum
          s2 = onEvent (PushButtonReleaseEvent $ PushButton 3) >>> sconst [$exp|-1|] >>> modNum
