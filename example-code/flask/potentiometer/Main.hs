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
      genStream $ clock 1 >>> adc 0 >>> smap scale >>> (setValue $ AnalogOutputPin 10 0)
    where
      scale :: N (Integer -> Integer)
      scale = liftN [$decls| f i = i/4|]