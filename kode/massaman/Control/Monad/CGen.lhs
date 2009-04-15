%if False
\begin{code}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

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
-- Module      :  Control.Monad.CGen
-- Copyright   :  (c) Harvard University 2006-2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Control.Monad.CGen where

import Control.Monad (ap)
import qualified Data.Set as Set

import Data.Loc

import Language.C.Syntax
import Language.C.Quote
\end{code}
%endif

\subsection{C Code Generation Monad}

\begin{code}
data CGenEnv = CGenEnv
    {  symbols :: Set.Set String

    ,  ctypedefs  :: [Definition]
    ,  cvardefs   :: [Definition]
    ,  cinitstms  :: [Stm]
    ,  cdecls     :: [InitGroup]
    ,  cstms      :: [Stm]

    ,  topContext   :: Bool
    ,  cscopeDepth  :: Int
    ,  cscopeTemps  :: Int
    }

emptyCGenEnv :: CGenEnv
emptyCGenEnv = CGenEnv
    {  symbols = Set.empty

    ,  ctypedefs  = []
    ,  cvardefs   = []
    ,  cinitstms  = []
    ,  cdecls     = []
    ,  cstms      = []

    ,  topContext   = True
    ,  cscopeDepth  = 0
    ,  cscopeTemps  = 0
    }

class Monad m => MonadCGen m where
    getCGenEnv   :: m CGenEnv
    putCGenEnv   :: CGenEnv -> m ()

    getsCGenEnv :: (CGenEnv -> a) -> m a
    getsCGenEnv f = getCGenEnv >>= \s -> return (f s)

    modifyCGenEnv :: (CGenEnv -> CGenEnv) -> m ()
    modifyCGenEnv f = getCGenEnv >>= \s -> putCGenEnv (f s)

    gensym :: String -> m String
    gensym s = do
        syms <- getsCGenEnv symbols
        let s' =  head [s | s <- ss, not (Set.member s syms)]
        modifyCGenEnv $ \s -> s { symbols = Set.insert s' (symbols s) }
        return s'
      where
        ss = [s ++ show i | i <- [0 :: Integer ..]]

    addCDecldef :: Definition -> m ()
    addCDecldef def =
        modifyCGenEnv (\s -> s { ctypedefs = def : ctypedefs s } )

    addCVardef :: Definition -> m ()
    addCVardef def =
        modifyCGenEnv (\s -> s { cvardefs = def : cvardefs s } )

    addCInitStm :: Stm -> m ()
    addCInitStm (Block [] [] _) =
        return ()

    addCInitStm stm =
        modifyCGenEnv (\s -> s { cinitstms = stm : cinitstms s })

    addCDecl :: InitGroup -> m ()
    addCDecl cdecl =
        modifyCGenEnv (\s -> s { cdecls = cdecl : cdecls s })

    addCStm :: Stm -> m ()
    addCStm (Block [] [] _) =
        return ()

    addCStm [$cstm|;|] =
        return ()

    addCStm [$cstm|NULL;|] =
        return ()

    addCStm stm =
        modifyCGenEnv (\s -> s { cstms = stm : cstms s })

    getCDefs :: m [Definition]
    getCDefs = do
        typedefs  <- getsCGenEnv ctypedefs
        vardefs   <- getsCGenEnv cvardefs
        return $ reverse typedefs ++ reverse vardefs

    getCInitStms :: m [Stm]
    getCInitStms =
        return reverse `ap` getsCGenEnv cinitstms

    isTopContext  :: m Bool
    isTopContext =
        getsCGenEnv topContext

    newScope :: Bool -> m a -> m (Stm, a)
    newScope top m = do
        s_old <- getCGenEnv
        modifyCGenEnv $ \s ->
            s  {  topContext = top
               ,  cscopeDepth = if top then 0 else cscopeDepth s + 1
               ,  cscopeTemps = 0
               ,  cdecls = []
               ,  cstms = []
               }
        a <- m
        decls  <- return reverse `ap` getsCGenEnv cdecls
        stms   <- return reverse `ap` getsCGenEnv cstms
        modifyCGenEnv $ \s ->
            s  {  cscopeDepth = cscopeDepth s_old
               ,  cscopeTemps = cscopeTemps s_old
               ,  cdecls = cdecls s_old
               ,  cstms = cstms s_old
               }
        case (decls, stms) of
          ([], [stm])  -> return (stm, a)
          _            -> return (Block decls stms internalLoc, a)

    ctemp :: Type -> m String
    ctemp cty = do
        depth <- getsCGenEnv cscopeDepth
        temps <- getsCGenEnv cscopeTemps
        modifyCGenEnv $ \s -> s { cscopeTemps = cscopeTemps s + 1 }
        let ident = "temp"  ++ (if depth > 0 then show depth else "")
                            ++ (if temps > 0 then "_" ++ show temps else "")
        addCDecl [$cdecl|$ty:cty $id:ident;|]
        return ident
\end{code}

\begin{code}
addCInclude :: MonadCGen m
            => String
            -> m ()
addCInclude def@('"' : _) =
    addCDecldef $ EscDef ("#include " ++ def) internalLoc
addCInclude def@('<' : _) =
    addCDecldef $ EscDef ("#include " ++ def) internalLoc
addCInclude def =
    addCDecldef $ EscDef ("#include <" ++ def ++ ">") internalLoc
\end{code}

\begin{code}
addCFundef :: MonadCGen m
           => Definition
           -> m ()
addCFundef def@(FuncDef f l) = do
    addCDecldef $ DecDef (funcProto f) l
    addCVardef def

addCFundef def = addCVardef def
\end{code}
