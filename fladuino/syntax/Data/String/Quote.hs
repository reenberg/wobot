{-# LANGUAGE QuasiQuotes #-}

module Data.String.Quote where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

literal :: QuasiQuoter
literal = QuasiQuoter (litE . stringL) (litP . stringL) 


