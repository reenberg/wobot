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
-- Module      :  Fladuino.Generate
-- Copyright   :  (c) Harvard University 2008
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Fladuino.Generate where

import Prelude hiding (exp)

import Control.Monad.State
import Control.Monad.Trans
import Data.IORef
import Data.List (intersperse, concat)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.IO

import qualified Check.F
import qualified Check.Hs
import Compiler
import Control.Monad.CGen
import Data.Loc
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

import Fladuino.Driver
import Fladuino.Monad
import Fladuino.Signals
import Fladuino.Device

defaultMain :: FladuinoM a -> IO a
defaultMain m = do
    args       <- getArgs
    (opts, _)  <- parseOpts defaultOpts args
    writeIORef Language.Hs.Quote.quasiOpts defaultOpts
    env        <- emptyFladuinoState opts
    result     <- (flip evalFladuinoM) env $ do
                    addCInclude "util/delay.h"
                    addCInclude "avr/interrupt.h"
                    addCInclude "avr/io.h"
                    addCInclude "common/Fladuino.h"
                    addCInclude "common/event_dispatch.h"
                    fdecls <- forPrelude [] compileHs
                    modifyFladuinoEnv $ \s ->
                        s { f_fdecls = f_fdecls s ++ fdecls }
                    m
    case result of
      Left err  -> fail $ show err
      Right a   -> return a
  where
    defaultOpts :: Opts
    defaultOpts = Opts  {  output    = Just "Fladuino"
                        ,  prelude   = Nothing
                        ,  flags     = [Opt_OptC, Opt_ImplicitPrelude]
                        }

putDoc :: MonadFladuino m => FilePath -> Doc -> m ()
putDoc filepath doc = do
    h <- liftIO $ openFile filepath WriteMode
    liftIO $ hPutStr h $ pretty 80 doc
    liftIO $ hClose h

genStream :: S a -> FladuinoM ()
genStream s = genStreams [unS s]

genStreams :: [FladuinoM (SCode FladuinoM)]
           -> FladuinoM ()
genStreams ss = do
    basename <- return (maybe "Fladuino" id) `ap` optVal output
    scodes <- sequence ss
    timer_connections   <- getsFladuinoEnv f_timer_connections
    channel_connections <- getsFladuinoEnv f_channel_connections
    let scodes' = scodes ++ [scode | (_, conns) <- Map.toList timer_connections,
                                     (scode, _) <- conns]
                         ++ [scode | (_, conns) <- Map.toList channel_connections,
                                     (scode, _) <- conns]
    genHs Set.empty scodes'
    env <- getFladuinoEnv
    forM_ (f_cvars env) $ \(gv, gty, ce) -> do
        Check.F.insertVar gv gty
        ToC.insertCVar gv (return ce)
    topdecls  <-  Rename.rename (f_hstopdecls env ++ f_hsdecls env) >>=
                  Desugar.desugar
    dump "hs" Opt_d_dump_hs basename "" topdecls
    live    <- getsFladuinoEnv f_live_vars
    fdecls  <- Check.Hs.checkTopDecls topdecls
    fdecls' <- optimizeF basename (Set.toList live) (f_fdecls env ++ fdecls)
    ToC.transDecls fdecls'
    genC Set.empty scodes'
    finalizeDevices
    finalizeTimers
    --finalizeInterrupts
    finalizeEvents
--    finalizeADCs
    finalizeFlows
    cdefs_toc            <- getCDefs
    cstms_toc            <- getCInitStms
    --fladuinom               <- moduleName
    --fladuinom_usesprovides  <- getModuleUsesProvides
    --fladuinoc_usesprovides  <- getConfigUsesProvides
    --fladuinoc_components    <- getComponents
    --fladuinoc_connections   <- getConnections
    filepath <- return (maybe "Fladuino" id) `ap` optVal output
    putDoc (filepath ++ ".pde") $ ppr [$cunit|
$edecls:cdefs_toc

void setup()
{
  $stms:cstms_toc
}

void loop()
{
  if (event_available()) 
         {
           struct event event = pop_event();
           switch (event.type)
                      {
                        case FCALL_EVENT:
                             (*event.data.fcall_event_data.func)();
                             break;
                        case FARGCALL_EVENT:
                             (*event.data.fargcall_event_data.func)(event.data.fargcall_event_data.data);
                             break;
                        default:
                        break;
                      }
         }
}
|]
  where
    genHs :: Set.Set SCodeID -> [SCode FladuinoM] -> FladuinoM ()
    genHs _       []      = return ()
    genHs visited (scode : scodes)
        | s_id scode `Set.member` visited = genHs visited scodes
        | otherwise = do
            --liftIO $ print $ s_name scode
            connections <- getsFladuinoEnv f_stream_connections
            let sconnections = [(to, v) |  (from, to, v) <- connections,
                                           s_id from == s_id scode]
            {-
            liftIO $ print $ [s_name from |  (from, to, _) <- connections,
                                             s_id to == s_id scode]
            liftIO $ print $ [s_name to |  (from, to, _) <- connections,
                                            s_id from == s_id scode]
            -}
            let es       = [H.appE (H.varE v) (H.varE x) |
                                (_, v) <- sconnections]
            let e_call   = seqsE es
            let decls    = [H.sigD [v_out] (tau H.--> unitTy),
                            H.varD  v_out [H.varP x] (H.rhsD [e_call])]
            modifyFladuinoEnv $ \s -> s { f_hsdecls = f_hsdecls s ++ decls }
            (s_gen_hs scode) scode
            genHs visited' ([from |  (from, to, _) <- connections,
                                     s_id to == s_id scode] ++
                            [to |  (from, to, _) <- connections,
                                   s_id from == s_id scode] ++
                            scodes)
      where
        tau      = s_type scode
        v_out    = s_vout scode
        x        = H.var "x"
        visited' = Set.insert (s_id scode) visited

    genC :: Set.Set SCodeID -> [SCode FladuinoM] -> FladuinoM ()
    genC _       []      = return ()
    genC visited (scode : scodes)
        | s_id scode `Set.member` visited = genC visited scodes
        | otherwise = do
            connections <- getsFladuinoEnv f_stream_connections
            (s_gen_c scode) scode
            genC visited' ([from |  (from, to, _) <- connections,
                                       s_id to == s_id scode] ++
                              [to |  (from, to, _) <- connections,
                                     s_id from == s_id scode] ++
                              scodes)
      where
        visited' = Set.insert (s_id scode) visited

    seqE :: H.Exp -> H.Exp -> H.Exp
    seqE e1 e2 = H.appE (H.appE (H.varE seqV) e1) e2

    seqsE :: [H.Exp] -> H.Exp
    seqsE es  = foldr seqE unitE es

    seqV :: H.Var
    seqV = H.var "seq"

    unitE :: H.Exp
    unitE = H.conE (H.TupleCon 0)

finalizeDevices :: forall m . MonadFladuino m
               => m ()
finalizeDevices = do
  devices <- getsFladuinoEnv f_devices
  forM_ devices $ \(DRef d) -> do
                    setup d
  return ()

finalizeTimers :: forall m . MonadFladuino m
               => m ()
finalizeTimers = do
    timersMap <- getsFladuinoEnv $ \s -> f_timers s
    let timers = Map.toList timersMap
    -- We must at least have 62 interrupts each second
    let interruptsPerSecond = max 62 (1000 / (fromIntegral . fst $ Map.findMin timersMap))
    when (timers /= []) $ do 
                   addCInclude "common/timersupport.h"
                   addCInitStm [$cstm|SetupTimer2($float:interruptsPerSecond);|]
    counterCode <- forM timers $ \(period, timerC) ->
        finalizeTimer period timerC interruptsPerSecond
    let (funcs, counterDefs, counterStms) = unzip3 counterCode
    let stms = concat counterStms
    mapM_ addCFundef funcs
    addCFundef [$cedecl|
void timer2_interrupt_handler (void)
{
  $decls:counterDefs
  $stms:stms
}
|]
  where
    finalizeTimer :: Int -> String -> Double -> m (Definition, InitGroup, [Stm])
    finalizeTimer period _ interruptsPerSecond = do
        let c_period = toInteger period
        vs <- getsFladuinoEnv $ \s ->
            Map.findWithDefault [] period (f_timer_connections s)
        stms <- forM vs $ \(_, v) -> do
                e <- hcall v $ ToC.CLowered unitGTy [$cexp|NULL|]
                return $ Exp (Just e) internalLoc
        return ([$cedecl|void $id:timerCP (void) { $stms:stms } |],
                [$cdecl|static int $id:counterCP = 0;|], 
                [[$cstm|$id:counterCP = $id:counterCP + 1;|],
                 [$cstm|if ( $id:counterCP >= ($float:interruptsPerSecond/1000* $int:c_period ))
                           {
                             queue_funcall(&$id:timerCP);
                             $id:counterCP = 0;
                           }|]])
      where
        timerCP = "timer" ++ show period
        counterCP = timerCP ++ "_counter"


finalizeInterrupts :: forall m . MonadFladuino m
               => m ()
finalizeInterrupts = do
    interrupts <- getsFladuinoEnv $ Map.toList . f_interrupts
    when (interrupts /= []) $ do 
                   addCInclude "common/PCINT.h"
    
    counterCode <- forM interrupts $ \(pin, interruptID) ->
        finalizeInterrupt pin interruptID
    let (funcs, initStms) = unzip counterCode
    let funcs' = concat funcs
    let stms = concat initStms
    mapM_ addCFundef funcs'
    mapM_ addCInitStm stms

--     addCFundef [$cedecl|
-- void interrupt2_interrupt_handler (void)
-- {
--   $decls:counterDefs
--   $stms:stms
-- }
-- |]
  where
    finalizeInterrupt :: Int -> String -> m ([Definition], [Stm])
    finalizeInterrupt pin _ = do
        let c_pin = toInteger pin
        vs <- getsFladuinoEnv $ \s ->
            Map.findWithDefault [] pin (f_interrupt_connections s)
        stms <- forM vs $ \(_, v) -> do
                e <- hcall v $ ToC.CLowered unitGTy [$cexp|NULL|]
                return $ Exp (Just e) internalLoc
        return ([[$cedecl|
                  void $id:interruptCP_switch (void) {
                      static int $id:interruptCP_on = 0;
                      $id:interruptCP_on = 1 - $id:interruptCP_on;
                      queue_funcall(&$id:interruptCP);
                  }|],

                 [$cedecl|
                  void $id:interruptCP (void) {
                      $stms:stms
                  }|]], 
                [[$cstm|pinMode($int:c_pin, INPUT);|],
                 [$cstm|PCattachInterrupt($int:c_pin, $id:interruptCP_switch, CHANGE);|]])
            where
              interruptCP = "interrupt" ++ show pin
              interruptCP_switch = interruptCP ++ "_switch"
              interruptCP_on = interruptCP ++ "_on"
              counterCP = interruptCP ++ "_counter"

finalizeEvents :: forall m. MonadFladuino m
               => m ()
finalizeEvents = do
  connections <- getsFladuinoEnv f_event_connections
  when (connections /= []) $ do 
                   addCInclude "common/PCINT.h"
  bindings <- forM connections $ \(erep, binding) ->
      do tauf_v <- toF $ e_type erep
         ty <- ToC.transType tauf_v
         (params, ce_params) <- ToC.flattenParams tauf_v
         e_params <- ToC.concrete ce_params
         stms <- forM binding $ \(_, v) -> do
                   e <- hcall v $ ToC.CLowered (tauf_v) [$cexp|$exp:e_params|]
                   return $ Exp (Just e) internalLoc
         let eventCP = map (\c -> if (c == ' ') then '_' else c) $ e_id erep
         let eventCheckCP = eventCP ++ "_check"
         
         predcall <- case e_predicate erep of
                       Just pred -> hcall pred $ ToC.CLowered unitGTy [$cexp|NULL|]
                       Nothing -> return [$cexp|true|]
         queueStms <- case e_value erep of
                        Just valfn -> do
                          eventcall <- hcall valfn $ ToC.CLowered unitGTy [$cexp|NULL|]
                          addCFundef [$cedecl|void $id:eventCP (void *data) {
                                                $ty:ty temp = *($ty:ty*) data;
                                                $ty:ty arg1 = temp;
                                                free(data);
                                                $stms:stms
                                              }|]
                          addCFundef [$cedecl|void $id:eventCheckCP () {
                                                if ($exp:predcall) {
                                                  $ty:ty *vp = ($ty:ty*) malloc(sizeof($ty:ty));
                                                  *vp = $exp:eventcall;
                                                  queue_fargcall(&$id:eventCP, (void*) vp);
                                                }
                                              }|]
                        Nothing -> do
                          addCFundef [$cedecl|void $id:eventCP () {
                                                $stms:stms
                                              }|]
                          addCFundef [$cedecl|void $id:eventCheckCP () {
                                                if ($exp:predcall) {
                                                  queue_funcall(&$id:eventCP);
                                                }
                                              }|]
         return (eventCheckCP, e_interrupts erep)
  let bindings' = foldl (\map (f, ints) -> 
                             foldl (\map int -> 
                                        Map.insert int (f : Map.findWithDefault [] int map) map)
                             map ints)
                  Map.empty bindings
  forM_ (Map.toList bindings') $ \(interrupt, fs) -> do
                   let c_pin = interrupt
                   let stms = map (\f -> [$cstm|$id:f();|]) fs
                   let interruptCP = "interrupt" ++ show interrupt
                   addCFundef [$cedecl|
                               void $id:interruptCP (void) {
                                 $stms:stms
                               }|]
                   addCInitStm [$cstm|pinMode($int:c_pin, INPUT);|]
                   addCInitStm [$cstm|PCattachInterrupt($int:c_pin, $id:interruptCP, CHANGE);|]

finalizeADCs :: MonadFladuino m => m ()
finalizeADCs = do
    adcs <- getsFladuinoEnv f_adcs
    when (adcs > 0) $ do
        adc_stms <-
            forM [0..adcs - 1] $ \i -> do
                let c_i = toInteger i
                e <- getsFladuinoEnv $ \s -> f_adc_getdata s Map.! i
                return [$cstm|
if ((adc_pending & 1 << $int:c_i) != 0) {
    $exp:e;
    return;
}
|]
        addCDecldef [$cedecl|typename uint16_t adc_val;|]
        addCDecldef [$cedecl|typename uint32_t adc_pending;|]
        addCInitStm [$cstm|adc_pending = 0;|]
        addCFundef [$cedecl|
void adc_process_pending()
{
  $stms:adc_stms
}
|]

finalizeFlows ::  FladuinoM ()
finalizeFlows = do
    receiving <- getsFladuinoEnv $ \s -> Map.toList (f_channel_receiving s)
    {-forM receiving $ \(chan, activity) ->
        case activity of
          Active   -> addCInitStm [$cstm|call Flow.subscribe($int:chan, TRUE);|]
          Passive  -> addCInitStm [$cstm|call Flow.subscribe($int:chan, FALSE);|]-}

    sending <- getsFladuinoEnv $ \s -> Map.toList (f_channel_sending s)
    {-forM sending $ \(chan, activity) ->
        case activity of
          Active   -> addCInitStm [$cstm|call Flow.publish($int:chan, TRUE);|]
          Passive  -> addCInitStm [$cstm|call Flow.publish($int:chan, FALSE);|]-}

    connections <- getsFladuinoEnv $ \s -> Map.toList (f_channel_connections s)
    return ()
    --handlers    <- forM connections $ \(chan, vs) -> finalizeChannel chan vs
    --let switch_stms = intersperse [$cstm|break;|] handlers
    --usesProvides True [$ncusesprovides|uses interface Flow;|]
    {-addCVardef [$cedecl|
event void Flow.receive(typename flowid_t flow_id, void *data, typename size_t size)
{
    switch (flow_id) {
        $stms:switch_stms
    }
}
|]-}
  where
    {-
    finalizeChannel :: Integer -> [(SCode FladuinoM, H.Var)] -> FladuinoM Stm
    finalizeChannel chan vs = do
        tau  <- getsFladuinoEnv $ \s -> f_channel_types s Map.! chan
        cty  <- toC tau
        stms <- forM vs $ \(_, v) -> do
                e <- hcall v $ ToC.CLowered tau [$cexp|temp|]
                return $ Exp (Just e) internalLoc
        return [$cstm|
case $int:chan:
    if (size == sizeof($ty:cty)) {
        $ty:cty temp;
        memcpy(&temp, data, sizeof(temp));
        $stms:stms;
    }
|]
-}
