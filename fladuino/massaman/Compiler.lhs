%if False
\begin{code}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Copyright (c) 2006-2008
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
--
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
-- Module      :  Compiler
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Compiler (
    module Compiler.Opt,
    MonadCompiler(..)
  ) where

import Control.Monad.Error
import Control.Monad.Exception
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.Maybe (fromMaybe)
import IO
import System.FilePath (splitExtension)

import Check.F
import Check.Hs
import Compiler.Opt
import Data.Loc
import qualified Language.F.Parser as F.P
import qualified Language.F as F
import qualified Language.Hs as H
import qualified Language.Hs.Parser as H.P
import Text.PrettyPrint.Mainland
import qualified Transform.F.Simplify as Simplify
import qualified Transform.Hs.Desugar as Desugar
import qualified Transform.Hs.Rename as Rename
\end{code}
%endif

\begin{code}
class (MonadIO m,
       Check.F.MonadTc m,
       Check.Hs.MonadTc IORef m,
       Simplify.MonadSimpl m) => MonadCompiler m where
    forPrelude :: a -> (String -> m a) -> m a
    forPrelude a m = do
        closures         <-  optVal (isFlagSet Opt_Closures)
        implicitPrelude  <-  optVal (isFlagSet Opt_ImplicitPrelude)
        preludeFile      <-  return (fromMaybe (thePrelude closures)) `ap`
                             optVal prelude
        if implicitPrelude
          then m preludeFile
          else return a
      where
        thePrelude :: Bool -> String
        thePrelude True   = "massaman.hs"
        thePrelude False  = "massaman_mini.hs"

    compileHs :: String -> m [F.Decl]
    compileHs filename = do
        s        <-  catchIO $ B.readFile filename
        opts     <-  getOpts
        let pos  =   Pos filename 1 1 1
        (H.Body _ topdecls) <-  liftM unLoc $
            case H.P.parse s pos True opts H.P.ParseDirect H.P.parseBody of
              Left e   ->  throwError e
              Right a  ->  return a
        topdecls' <- Rename.rename topdecls >>= Desugar.desugar
        fdecls    <- Check.Hs.checkTopDecls topdecls'
        return fdecls

    compileF :: String -> m [F.Decl]
    compileF filename = do
        s        <-  catchIO $ B.readFile filename
        opts     <-  getOpts
        let pos  =   Pos filename 1 1 1
        decls <-  liftM (map unLoc) $
            case F.P.parse s pos opts F.P.parseBody of
              Left e   ->  throwError e
              Right a  ->  return a
        return decls

    checkF :: [F.Decl] -> m ()
    checkF fdecls = do
        check_f <- optVal (isFlagSet Opt_d_check_f)
        when check_f $
            Check.F.savingTcEnv $
            Check.F.checkDecls fdecls

    dump :: Pretty a => String -> Flag -> FilePath -> String -> [a] -> m ()
    dump ext flag filename suffix as = do
        dump <- optVal (isFlagSet flag)
        when dump $ do
            h <- liftIO $ openFile filepath WriteMode
            liftIO $ hPutStr h $ pretty 80 (stack $ map ppr as)
            liftIO $ hClose h
      where
        filepath = fst (splitExtension filename) ++
                   ".dump." ++ case suffix of
                                 "" -> ext
                                 _ ->  suffix ++ "." ++ ext

    dumpAndCheckF :: Flag -> FilePath -> String -> [F.Decl] -> m ()
    dumpAndCheckF flag filename suffix fdecls = do
        dump "f" flag filename suffix fdecls
        checkF fdecls

    optimizeF :: String -> [F.Var] -> [F.Decl] -> m [F.Decl]
    optimizeF filename live decls = do
        simplify  <-  optVal (isFlagSet Opt_simpl)
        dumpAndCheckF Opt_d_dump_f filename "raw" decls
        if simplify
            then  Check.F.savingTcEnv $
                  Simplify.savingSimplEnv $
                  Simplify.simplify
                      live
                      (dumpAndCheckF Opt_d_dump_simpl filename)
                      decls
            else return decls
\end{code}
