{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

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
-- Module      :  Fladuino.Monad
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Fladuino.Monad where

import Prelude hiding (exp)

import Control.Monad (when)
import Data.IORef
import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Check.F
import qualified Check.Hs
import Compiler
import Control.Monad.CGen
import Control.Monad.Ref
import Data.List
import Data.Loc
import Data.Name
import qualified Language.C.Syntax
import Language.C.Syntax
import Language.C.Quote
import qualified Language.Hs.Syntax
import qualified Language.Hs as H
import Language.Hs.Quote
import qualified Language.F as F
import Text.PrettyPrint.Mainland
import qualified Transform.F.ToC as ToC

import Fladuino.Reify

data FladuinoEnv m = FladuinoEnv
    {  f_fdecls      :: [F.Decl]
    ,  f_hstopdecls  :: [H.Decl]
    ,  f_hsdecls     :: [H.Decl]
    ,  f_cvars       :: [(F.Var, F.Type, ToC.CExp)]
    ,  f_live_vars   :: Set.Set F.Var

    ,  f_ncode_id   :: NCodeID
    ,  f_ncode_hash :: Map.Map NRep NCode

    ,  f_stream_id          :: SCodeID
    ,  f_stream_hash        :: Map.Map (SRep m) (SCode m)
    ,  f_stream_connections :: [(SCode m, SCode m, H.Var)]

    ,  f_timers            :: Map.Map Int String
    ,  f_timer_connections :: Map.Map Int [(SCode m, H.Var)]

    , f_interrupts :: Map.Map Int String
    , f_interrupt_connections :: Map.Map Int [(SCode m, H.Var)]

    ,  f_devices :: [DRef m]

    ,  f_events            :: [ERep m]
    ,  f_event_connections :: [(ERep m, [(SCode m, H.Var)])]

    ,  f_adcs         :: Int
    ,  f_adc_getdata  :: Map.Map Int Exp

    ,  f_channel_types       :: Map.Map FlowChannel F.Type
    ,  f_channel_receiving   :: Map.Map FlowChannel FlowActivity
    ,  f_channel_sending     :: Map.Map FlowChannel FlowActivity
    ,  f_channel_connections :: Map.Map FlowChannel [(SCode m, H.Var)]
    }

emptyFladuinoEnv :: FladuinoEnv m
emptyFladuinoEnv = FladuinoEnv
    {  f_fdecls      = []
    ,  f_hsdecls     = []
    ,  f_hstopdecls  = []
    ,  f_cvars       = []
    ,  f_live_vars   = Set.empty

    ,  f_ncode_id = 0
    ,  f_ncode_hash = Map.empty

    ,  f_stream_id = 0
    ,  f_stream_hash = Map.empty
    ,  f_stream_connections = []

    ,  f_timers = Map.empty
    ,  f_timer_connections = Map.empty

    ,  f_interrupts = Map.empty
    ,  f_interrupt_connections = Map.empty

    ,  f_devices = []

    ,  f_events = []
    ,  f_event_connections = []

    ,  f_adcs = 0
    ,  f_adc_getdata = Map.empty

    ,  f_channel_types = Map.empty
    ,  f_channel_receiving = Map.empty
    ,  f_channel_sending = Map.empty
    ,  f_channel_connections = Map.empty
    }

type NCodeID = Int

data NCode = NCode
    { n_id   :: NCodeID
    , n_type :: H.Type
    , n_var  :: H.Var
    , n_rep  :: NRep
    }
  deriving (Eq, Ord)

instance Show NCode where
    show (NCode { n_rep = nrep }) = show nrep

data NRep  =  HsDecls H.Type [H.Decl]
           |  HsExp H.Type H.Exp
           |  CFun H.Type Func
  deriving (Eq, Ord, Show)

type SCodeID = Int

data SCode m = SCode
    { s_id       :: SCodeID          -- ^ Unique identifier
    , s_name     :: String           -- ^ Operator's name
    , s_vout     :: H.Var            -- ^ Red function to which to send output
    , s_type     :: H.Type           -- ^ Type of output
    , s_rep      :: SRep m           -- ^ Stream representation
    , s_gen_hs   :: SCode m -> m ()  -- ^ Red code generator
    , s_gen_c :: SCode m -> m ()  -- ^ C code generator
    }

instance Eq (SCode m) where
    SCode { s_rep = srep1 } == SCode { s_rep = srep2 } = srep1 == srep2

instance Ord (SCode m) where
    compare (SCode { s_rep = srep1 }) (SCode { s_rep = srep2 }) =
        compare srep1 srep2

instance Show (SCode m) where
    show (SCode { s_id = sid }) = show sid

-- A bit of indirection for convenience...
data DRef m = forall a. (Device a) => DRef a

instance Eq (DRef m) where
    DRef d1 == DRef d2 = uniqueId d1 == uniqueId d2

data ERef m = forall e t. (Event e t) => ERef (e, H.Var)

instance Eq (ERef m) where
    ERef (e1, _) == ERef (e2, _) = show e1 == show e2

data ERep m = ERep
    { e_value      :: Maybe H.Var
    , e_predicate  :: Maybe H.Var
    , e_type       :: H.Type
    , e_interrupts :: [Integer]
    , e_id         :: String
    }

instance Eq (ERep m) where
    e1 == e2 = e_id e1 == e_id e2

data SRep m  =  SConst NCode (SCode m)
             |  SMap NCode (SCode m)
             |  SFilter NCode (SCode m)
             |  SMerge (SCode m) (SCode m)
             |  SZip (SCode m) (SCode m)
             |  SIntegrate NCode NCode (SCode m)
             |  SJust (SCode m)
             |  SClock Int
             |  SExternalInterrupt Int
             |  SADC String
             |  SSend FlowChannel FlowActivity (SCode m)
             |  SRecv FlowChannel FlowActivity
             |  SLoop Int NCode NCode (SCode m)
             |  SBlackbox String
             |  DeviceWrite (SCode m) String
             |  DeviceRead (SCode m) String
             |  OnEvent String
             |  GenericSRep (SCode m) String -- For random new operators.
  deriving (Eq, Ord, Show)

type FlowChannel = Integer

data FlowActivity = Passive | Active
  deriving (Eq, Ord, Show)

class (MonadCompiler m,
       MonadCGen m,
       MonadRef IORef m,
       ToC.MonadToC IORef m) => MonadFladuino m where
    getFladuinoEnv   :: m (FladuinoEnv m)
    putFladuinoEnv   :: FladuinoEnv m -> m ()

    getsFladuinoEnv :: (FladuinoEnv m -> a) -> m a
    getsFladuinoEnv f = getFladuinoEnv >>= \s -> return (f s)

    modifyFladuinoEnv :: (FladuinoEnv m -> FladuinoEnv m) -> m ()
    modifyFladuinoEnv f = getFladuinoEnv >>= \s -> putFladuinoEnv (f s)

    liveVar :: H.Var -> m ()
    liveVar (H.Var v) =
        modifyFladuinoEnv $ \s ->
            s { f_live_vars = Set.insert (F.Var v) (f_live_vars s) }

    addTopDecls :: [H.Decl] -> m ()
    addTopDecls decls =
        modifyFladuinoEnv $ \s ->
            s { f_hstopdecls = f_hstopdecls s ++ decls }

    addDecls :: [H.Decl] -> m ()
    addDecls decls =
        modifyFladuinoEnv $ \s -> s { f_hsdecls = f_hsdecls s ++ decls }

    addAbstractType :: H.Type -> Type -> m ()
    addAbstractType tau cty = do
        tau_f <- toF tau
        ToC.insertCType tau_f cty

    addCImport :: String -> H.Type -> Exp -> m ()
    addCImport v tau e = do
        tau_f <- toF tau
        let ce = ToC.CLowered tau_f e
        addDecls [$decls|$id:v :: $ty:tau|]
        modifyFladuinoEnv $ \s ->
            s { f_cvars = (F.var v, tau_f, ce) : f_cvars s }

    addCExpImport :: String -> H.Type -> ToC.CExp -> m ()
    addCExpImport v tau ce = do
        tau_f <- toF tau
        addDecls [$decls|$id:v :: $ty:tau|]
        modifyFladuinoEnv $ \s ->
            s { f_cvars = (F.var v, tau_f, ce) : f_cvars s }

    addCode  ::  H.Type
             ->  NRep
             ->  m H.Var
             ->  m NCode
    addCode tau nrep m = do
        maybe_ncode <- getsFladuinoEnv $ \s -> Map.lookup nrep (f_ncode_hash s)
        case maybe_ncode of
          Just ncode -> return ncode
          Nothing -> do
              v <- m
              nid <- getsFladuinoEnv f_ncode_id
              modifyFladuinoEnv $ \s -> s { f_ncode_id = (f_ncode_id s) + 1 }
              let ncode = NCode { n_id   = nid
                                , n_type = tau
                                , n_var  = v
                                , n_rep  = nrep
                                }
              modifyFladuinoEnv $ \s -> s { f_ncode_hash = Map.insert nrep ncode
                                                       (f_ncode_hash s) }
              return ncode

    addStream  ::  String             -- ^ Operator's name
               ->  H.Type             -- ^ Type of output
               ->  SRep m             -- ^ Stream representation
               ->  (SCode m -> m ())  -- ^ Red code generator
               ->  (SCode m -> m ())  -- ^ C code generator
               ->  (SCode m -> m ())  -- ^ Stream constructor
               ->  m (SCode m)
    addStream name ty srep gen_hs gen_c m = do
        maybe_scode <- getsFladuinoEnv $ \s -> Map.lookup srep (f_stream_hash s)
        case maybe_scode of
          Just scode -> return scode
          Nothing -> do
              sid <- getsFladuinoEnv f_stream_id
              modifyFladuinoEnv $ \s -> s { f_stream_id = (f_stream_id s) + 1 }
              let scode = SCode { s_id       = sid
                                , s_name     = name
                                , s_vout     = H.var $
                                               name ++ show sid ++ "_out"
                                , s_type     = ty
                                , s_rep      = srep
                                , s_gen_hs   = gen_hs
                                , s_gen_c = gen_c
                                }
              modifyFladuinoEnv $ \s -> s { f_stream_hash = Map.insert srep scode
                                                         (f_stream_hash s) }
              m scode
              return scode

    connect :: SCode m -> SCode m -> H.Type -> H.Var -> m ()
    connect sfrom sto tau v =  do
        when (tau_from /= tau) $
            fail $ pretty 80 $ text "type" <+> squotes (ppr tau_from) <+> text "and" <+>
                squotes (ppr tau) <+> text "don't match when connecting stream" <+>
                squotes (ppr (s_name sfrom)) <+> text "to" <+> squotes (ppr (s_name sto))
        modifyFladuinoEnv $ \s ->
            s { f_stream_connections = (sfrom, sto, v) : f_stream_connections s }
      where
        tau_from = s_type sfrom

    addTimer :: Int -> m String
    addTimer period = once $ do
        modifyFladuinoEnv $ \s ->
            s { f_timers = Map.insert period timerCP (f_timers s) }
        return timerCP
      where
        timerCP :: String
        timerCP = "Timer" ++ show period

        once :: MonadFladuino m => m String -> m String
        once m = do
            maybe_timerc <- getsFladuinoEnv $ \s -> Map.lookup period (f_timers s)
            case maybe_timerc of
              Just timerc -> return timerc
              Nothing ->     m

    connectTimer :: Int -> SCode m -> H.Var -> m ()
    connectTimer period stream v = do
        liveVar v
        modifyFladuinoEnv $ \s ->
            s { f_timer_connections =
                    let connections = Map.findWithDefault []
                                      period (f_timer_connections s)
                    in
                      Map.insert period ((stream, v) : connections)
                                 (f_timer_connections s)
              }

    addInterrupt :: Int -> m String 
    addInterrupt pin = once $ do
        modifyFladuinoEnv $ \s ->
            s { f_interrupts = Map.insert pin interruptID (f_interrupts s) }
        return interruptID
      where
        interruptID = "Interrupt" ++ show pin
          
        once :: MonadFladuino m => m String -> m String
        once m = do
            maybe_interrupt <- getsFladuinoEnv $ \s -> Map.lookup pin (f_interrupts s)
            case maybe_interrupt of
              Just interrupt -> return interrupt
              Nothing ->     m


    connectInterrupt :: Int -> SCode m -> H.Var -> m ()
    connectInterrupt pin stream v = do
        liveVar v
        modifyFladuinoEnv $ \s ->
            s { f_interrupt_connections =
                    let connections = Map.findWithDefault []
                                      pin (f_interrupt_connections s)
                    in
                      Map.insert pin ((stream, v) : connections)
                                 (f_interrupt_connections s)
              }
    

    addDevice :: Device d => d -> m ()
    addDevice d = once $ do
                    modifyFladuinoEnv $ \s -> 
                        s { f_devices = (DRef d) : (f_devices s) }
                    return ()
        where

        once :: MonadFladuino m => m () -> m ()
        once m = do
          devices <- getsFladuinoEnv f_devices
          case find (\(DRef d2) -> uniqueId d2 == uniqueId d) devices of
            Just device -> return ()
            Nothing ->     m

    lookupEvent :: forall t e. (Event e t) => e -> m (Maybe (ERep m))
    lookupEvent event = do 
      events <- getsFladuinoEnv f_events 
      case find (\(ERep { e_id = id}) -> show event == id) events of
        Just erep  -> return $ Just erep
        Nothing -> return Nothing

    useChannel :: FlowChannel -> F.Type -> m ()
    useChannel chan tau = do
        tau_chan <- getsFladuinoEnv $ \s ->
            Map.findWithDefault tau chan (f_channel_types s)
        when (tau /= tau_chan) $
            fail $ pretty 80 $
            text "channel" <+> ppr chan <+> text "has type" <+> ppr tau_chan
            <+> text "but requested type" <+> ppr tau
        modifyFladuinoEnv $ \s ->
            s { f_channel_types = Map.insert chan tau (f_channel_types s)
              }

    connectFlow :: FlowChannel -> FlowActivity -> F.Type -> SCode m -> H.Var -> m ()
    connectFlow chan act tau stream v = do
        liveVar v
        useChannel chan tau
        modifyFladuinoEnv $ \s ->
            s { f_channel_receiving = Map.adjust (activityLUB act) chan
                                                 (f_channel_receiving s)
              , f_channel_connections =
                    let connections = Map.findWithDefault []
                                      chan (f_channel_connections s)
                    in
                      Map.insert chan ((stream, v) : connections)
                                 (f_channel_connections s)
              }

    sendOnFlow :: FlowChannel -> FlowActivity -> F.Type -> m ()
    sendOnFlow chan act tau = do
        useChannel chan tau
        modifyFladuinoEnv $ \s ->
            s { f_channel_sending = Map.adjust (activityLUB act) chan
                                               (f_channel_sending s)
              }

activityLUB :: FlowActivity -> FlowActivity -> FlowActivity
activityLUB Active _      = Active
activityLUB _      Active = Active
activityLUB _      _      = Passive

class ToF a b where
    toF :: MonadFladuino m => a -> m b

instance ToF H.Type F.Type where
    toF tau = do
        tau_f <- Check.Hs.toSigma tau
        Check.Hs.coerceAst tau_f

class ToC a b where
    toC :: MonadFladuino m => a -> m b

instance ToC H.Type Type where
    toC tau = do
        tau_f <- Check.Hs.toSigma tau >>= Check.Hs.coerceAst
        ToC.transType tau_f

instance ToC F.Type Type where
    toC tau = ToC.transType tau

instance ToC H.Var Exp where
    toC (H.Var v) = do
        v_cexp <- ToC.transExp Nothing (F.varE (F.Var v))
        ToC.concrete v_cexp

    toC _ = fail "bad H.Var"

unitTy :: H.Type
unitTy = H.TyConTy (H.TupleTyCon 0)

unitGTy :: F.Type
unitGTy = F.TyConTy (F.TupleTyCon 0) internalLoc

floatTy :: H.Type
floatTy = H.TyConTy (H.TyCon prelFloat)

integerTy :: H.Type
integerTy = H.TyConTy (H.TyCon prelInteger)

boolTy :: H.Type
boolTy = H.TyConTy (H.TyCon prelBool)

floatGTy :: F.Type
floatGTy = F.TyConTy (F.TyCon prelFloat) internalLoc

hcall :: MonadFladuino m => H.Var -> ToC.CExp -> m Exp
hcall (H.Var n_v) ce_arg = do
    ce <- Check.F.extendVars [(x, ToC.cexpTy ce_arg)] $
          ToC.extendCVars [(x, return ce_arg)] $
          ToC.transExp Nothing (F.appE (F.varE v) (F.varE x))
    ToC.concrete ce
  where
    v = F.Var n_v
    x = F.var "x"

hcall _ _ = fail "bad H.Var"

hfst :: MonadFladuino m => ToC.CExp -> m Exp
hfst = hcall (H.var "fst")

hsnd :: MonadFladuino m => ToC.CExp -> m Exp
hsnd = hcall (H.var "snd")

varIn :: SCode m -> String -> H.Var
varIn s "" = H.var $ s_name s ++ show (s_id s) ++ "_in"
varIn s v  = H.var $ s_name s ++ show (s_id s) ++ "_in_" ++ v

ident :: SCode m -> String -> String
ident s ""     = s_name s ++ show (s_id s) ++ "_c"
ident s suffix = s_name s ++ show (s_id s) ++ "_c" ++ suffix

class (Eq a) => Device a where
    setup :: MonadFladuino m => a -> m ()
    deviceClass :: a -> String
    uniqueId :: a -> String
    uniqueId = deviceClass

class (Eq e, Show e, Reify t) => Event e t | e -> t where
    interruptPins :: e -> [Integer]

eventValueType :: forall e t. (Event e t) => e -> H.Type
eventValueType _ = reify (undefined :: t)
