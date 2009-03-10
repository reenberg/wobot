{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

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
-- Module      :  Control.Monad.NesCGen
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Control.Monad.NesCGen where

import Control.Monad (ap,
                      forM_,
                      when)
import qualified Data.Set as Set

import Language.NesC.Syntax

data NesCGenEnv = NesCGenEnv
    {  config_name          :: String
    ,  config_usesprovides  :: [UsesProvides]
    ,  config_components    :: [Components]
    ,  config_connections   :: [Connection]

    ,  module_name          :: String
    ,  module_usesprovides  :: [UsesProvides]

    ,  components           :: Set.Set String
    }

emptyNesCGenEnv :: NesCGenEnv
emptyNesCGenEnv = NesCGenEnv
    {  config_name          = error "no configuration name set"
    ,  config_usesprovides  = []
    ,  config_components    = []
    ,  config_connections   = []

    ,  module_name          = error "no module name set"
    ,  module_usesprovides  = []

    ,  components = Set.empty
    }

class Monad m => MonadNesCGen m where
    getNesCGenEnv   :: m NesCGenEnv
    putNesCGenEnv   :: NesCGenEnv -> m ()

    getsNesCGenEnv :: (NesCGenEnv -> a) -> m a
    getsNesCGenEnv f = getNesCGenEnv >>= \s -> return (f s)

    modifyNesCGenEnv :: (NesCGenEnv -> NesCGenEnv) -> m ()
    modifyNesCGenEnv f = getNesCGenEnv >>= \s -> putNesCGenEnv (f s)

    configName :: m String
    configName = getsNesCGenEnv config_name

    setConfigName :: String -> m ()
    setConfigName name =
        modifyNesCGenEnv $ \s -> s { config_name = name }

    moduleName :: m String
    moduleName = getsNesCGenEnv module_name

    setModuleName :: String -> m ()
    setModuleName name =
        modifyNesCGenEnv $ \s -> s { module_name = name }

    lookupComponent :: String -> m () -> m String
    lookupComponent ident add = do
        is_member <- getsNesCGenEnv $ \s -> Set.member ident (components s)
        when (not is_member) $ do
            add
            modifyNesCGenEnv $ \s ->
                s { components = Set.insert ident (components s) }
        return ident

    usesProvides :: Bool -> UsesProvides -> m ()
    usesProvides export up = do
        modifyNesCGenEnv $ \s ->
            s { module_usesprovides = up : module_usesprovides s }
        when export $
             case up of
               Uses elems      -> configUsesProvides Uses elems
               Provides elems  -> configUsesProvides Provides elems
               _               -> error "internal error: antiquoted uses/provides"
      where
        configUsesProvides :: MonadNesCGen m
                           => ([SpecElem] -> UsesProvides)
                           -> [SpecElem]
                           -> m ()
        configUsesProvides con spec_elems = do
            mname <- moduleName
            modifyNesCGenEnv $ \s ->
                s { config_usesprovides = con spec_elems : config_usesprovides s }
            forM_ (concatMap specElemId spec_elems) $ \ident ->
                modifyNesCGenEnv $ \s ->
                    s { config_connections = ConnEqual (Endpoint [ident] [])
                                                       (Endpoint [Id mname, ident] [])
                                             : config_connections s }

        specElemId :: SpecElem -> [Id]
        specElemId (BareSpec (InitGroup _ _ inits)) =
            [ident | Init ident _ _ _ <- inits]
        specElemId (InterfaceSpec _ _ (Just ident) _) =
            [ident]
        specElemId (InterfaceSpec ident _ _ _) =
            [ident]
        specElemId  _ =
            error "internal error: antiquoted SpecElem"

    addComponents :: Components ->  m ()
    addComponents comp =
        modifyNesCGenEnv $ \s ->
            s { config_components = comp : config_components s }

    getComponents :: m [Components]
    getComponents =
        return reverse `ap` getsNesCGenEnv config_components

    addConnection :: Connection ->  m ()
    addConnection conn =
        modifyNesCGenEnv $ \s ->
            s { config_connections = conn : config_connections s }

    getConnections :: m [Connection]
    getConnections =
        return reverse `ap` getsNesCGenEnv config_connections

    getConfigUsesProvides :: m [UsesProvides]
    getConfigUsesProvides =
        return reverse `ap` getsNesCGenEnv config_usesprovides

    getModuleUsesProvides :: m [UsesProvides]
    getModuleUsesProvides =
        return reverse `ap` getsNesCGenEnv module_usesprovides
