{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}


-- Copyright (c) 2008
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

--------------------------------------------------------------------------------
-- |
-- Module      :  Flask.Device
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  athas@sigkill.dk
--
--------------------------------------------------------------------------------

module Flask.Device where

import Prelude hiding (exp)

import Control.Monad.State
import Control.Monad.Trans
import Data.IORef
import Data.List (intersperse, concat)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO

import qualified Check.F
import qualified Check.Hs
import Compiler
import Control.Monad.CGen
import Data.Loc
import Data.Name
import Data.List
import Data.String.Quote
import qualified Language.Hs.Syntax
import qualified Language.Hs as H
import Language.Hs.Quote
import Language.C.Syntax
import Language.C.Quote
import Text.PrettyPrint.Mainland
import qualified Transform.F.ToC as ToC
import qualified Transform.Hs.Desugar as Desugar
import qualified Transform.Hs.Rename as Rename

import Flask.Driver
import Flask.Monad
import Flask.Signals
import Flask.Reify
import Flask.LiftN

statevar :: Device d => MonadFlask m => d -> String -> m String
statevar d s = do
  devices <- getsFlaskEnv f_devices
  case findIndex (\d2 -> d_id d2 == unique_id d) devices of
    Just n -> return $ "device_" ++ (show n) ++ s
    Nothing -> return $ error "Unknown device encountered."


data Pin = Pin Integer Bool
           deriving Eq

instance Device Pin where
    setup d@(Pin pin initstate) = 
        do 
          sv <- statevar d "state"
          addCDecldef [$cedecl|unsigned char $id:sv;|]
          addCInitStm [$cstm|pinMode($int:pin, OUTPUT);|]
          addCInitStm [$cstm|$id:sv = $id:val;|]
          addCInitStm [$cstm|digitalWrite($int:pin, $id:val);|]
          where
            val = if initstate then "HIGH" else "LOW"
    unique_id (Pin pin initstate) = "pin_" ++ (show pin)

toggle :: forall a . Reify a => Pin -> S a -> S ()
toggle d@(Pin pin startstate) a = S $ do
  sa <- unS a
  addDevice d
  unS (a >>> onezero >>> digitalWrite)
    where
      onezero =  sintegrate zero int
          where
            startval = if startstate then 1 else 0
            zero :: N Integer
            zero = liftN [$exp|$int:startval|]
            int :: N ((a, Integer) -> ((Integer, Integer), Integer))
            int = liftN [$decls|f (x, 0) = (($int:pin, 1), 1); f (x, 1) = (($int:pin, 0), 0)|]
