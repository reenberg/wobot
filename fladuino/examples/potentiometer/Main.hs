{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (exp)

import Data.Loc
import Data.Name
import qualified Language.Hs.Syntax
import Language.Hs.Quote

import Fladuino
import Fladuino.Devices

main :: IO ()
main =
    defaultMain $ do
      genStream $ clock 1 >>> (valueOf $ Potentiometer 0) >>> smap [$exp|(/4)|] >>> (setValue $ AnalogOutputPin 10 0)
