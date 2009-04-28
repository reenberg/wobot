{-# LANGUAGE CPP #-}
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
-- Module      :  Fladuino.LiftN
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Fladuino.LiftN where

import Prelude hiding (exp)

import Control.Monad.State

import Control.Monad.CGen
import Control.Monad.ContextException
import Control.Monad.Exception
import Data.Loc
import Data.Name
import qualified Language.Hs.Syntax
import qualified Language.Hs as H
import Language.Hs.Quote
import Language.C.Syntax
import Language.C.Quote
import Text.PrettyPrint.Mainland
import qualified Transform.Hs.Desugar as Desugar
import qualified Transform.Hs.Rename as Rename

import Fladuino.Driver
import Fladuino.Exceptions
import Fladuino.Monad
import Fladuino.Reify

-- |A value of type @N a@ represents node-level code of type @a@. The @LiftN@
-- type class allows us to lift either C or "Red" code to node-level code.

newtype N a =  N { unN :: FladuinoM NCode }

class LiftN n a where
    liftN :: n -> N a

instance LiftN (N a) a where
    liftN = id

instance forall a . (Reify a) => LiftN [H.Decl] a where
    liftN decls = N $ addCode ty (HsDecls ty decls) $ do
        decls' <- Rename.rename decls >>= Desugar.desugar
        v <- case binders decls' of
               [v]  ->  return v
               vs   ->  withLocContext (getLoc decls') (text "declaration") $
                        fail $ pretty 80 $
                        text "cannot choose which declaration to lift:"
                        <+> commasep (map ppr vs)
        liftDecls decls' v ty
      where
        ty :: H.Type
        ty = reify (undefined :: a)

instance forall a . (Reify a) => LiftN H.Exp a where
    liftN e = N $ addCode ty (HsExp ty e) $
        liftDecls [decl] v ty
      where
        v :: H.Var
        v = H.var "v"

        ty :: H.Type
        ty = reify (undefined :: a)

        decl :: H.Decl
        decl =  H.patD (H.varP v) (H.rhsD [H.sigE e ty])

instance Reify a => LiftN Func a where
    liftN f = N $ addCode ty (CFun ty f) $ do
        cty  <- toC ty
        when (funTy f /= cty) $
             throwException $ CTypeError cty (funTy f)
        liftCFun f ty
      where
        ty :: H.Type
        ty = reify (undefined :: a)

        funTy :: Func -> Type
        funTy (Func decl_spec _ decl args _) =
            Type decl_spec (Ptr [] (Proto decl (stripParams args)))
        funTy (OldFunc decl_spec _ decl args _ _) =
            Type decl_spec (Ptr [] (OldProto decl args))

        stripParams :: Params -> Params
        stripParams (Params ps var) = Params (map stripParam ps) var

        stripParam :: Param -> Param
        stripParam (Param _ decl_spec decl) = Param Nothing decl_spec decl
        stripParam _                        = error "internal error"

liftDecls :: MonadFladuino m
          => [H.Decl]
          -> H.Var
          -> H.Type
          -> m H.Var
liftDecls decls v@(H.Var n) ty = do
    v' <-  return H.var `ap` gensym (nameString n)
    let decl = H.patD (H.varP v') (H.rhsD [H.letE decls (H.varE v)])
    modifyFladuinoEnv $ \s ->
        s { f_hsdecls = f_hsdecls s ++ [H.sigD [v'] ty, decl] }
    return v'

liftDecls _ _ _ = fail "Cannot lift"

liftCFun :: MonadFladuino m
            => Func
            -> H.Type
            -> m H.Var
liftCFun f ty = do
    addCImport fid ty [$cexp|$id:fid|]
    addDecls [$decls|$var:v :: $ty:ty|]
    addCFundef $ FuncDef f internalLoc
    return v
  where
    fid :: String
    fid = idString (funName f)

    v :: H.Var
    v = H.var fid

    funName :: Func -> Id
    funName (Func _ ident _ _ _)       = ident
    funName (OldFunc _ ident _ _ _ _)  = ident

    idString :: Id -> String
    idString (Id s)              = s
    idString _                   = error "internal error"

nzip :: forall a b . (Reify a, Reify b) => N a -> N b -> N (a, b)
nzip na nb = N $ do
    n_1 <- unN na
    n_2 <- unN nb
    let n1 = H.varE (n_var n_1)
    let n2 = H.varE (n_var n_2)
    nzip <- unN $ (liftN [$exp|($exp:n1, $exp:n2)|] :: N (a, b))
    return nzip

nexp :: N a -> FladuinoM H.Exp
nexp n = do
    ncode <- unN n
    return $ H.varE (n_var ncode)
