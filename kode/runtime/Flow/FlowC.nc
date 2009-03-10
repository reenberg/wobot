/*
 * Copyright (c) 2005-2007
 *      The President and Fellows of Harvard College.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/* Author: Geoffrey Mainland <mainland@eecs.harvard.edu> */

#include "Flow.h"

generic configuration FlowC(int CHAN,
                            int NSUB, int NPUB,
                            int NSEQNO,
                            int NNEIGHBORS,
                            int NMSG) {
    provides interface StdControl;
    provides interface Flow;
    provides interface FlowControl;
    provides interface FlowDebug;
    provides interface LinkEstimator;
    provides interface NeighborTable;

    uses interface SendMsg[uint8_t];
    uses interface SendMsg as UrgentSendMsg[uint8_t];
    uses interface ReceiveMsg[uint8_t];
} implementation {
    components FlowM,
        FlowDebugPrinter,
        new FlowInterestM(NSUB) as SubscribeInterest,
        new FlowInterestM(NPUB) as PublishInterest,
        new FlowSeqnoM(NSEQNO) as FlowSeqno,
        new NeighborTableM(NNEIGHBORS) as NeighborTableM,
        new PoolM(struct flowaction_t, NMSG) as Pool,
        new QueueM(struct flowaction_t*, NMSG) as SendQueue,
        new QueueM(struct flowaction_t*, NMSG) as ReceiveQueue,
        LQILinkEstimatorM as TheLinkEstimator,
#if defined(TOSMSG_HAS_ACK)
        CC2420RadioM,
#endif /* defined(TOSMSG_HAS_ACK) */
        RandomLFSR as Random,
        TimerC,
        LedsC;

#if defined(PLATFORM_MICA2) || defined(PLATFORM_MICAZ)
    components SysTimeC as SysTime;
#elif defined(PLATFORM_TELOSB)
    components LocalTimeToSysTimeM as SysTime;
#elif defined(PLATFORM_PC)
    components SysTimeM as SysTime;
#endif /* !defined(PLATFORM_MICA2) &&  
          !defined(PLATFORM_MICAZ) &&  
          !defined(PLATFORM_TELOSB) */

    Flow = FlowM;
    FlowControl = FlowM;
    FlowDebug = FlowM;
    LinkEstimator = TheLinkEstimator;
    NeighborTable = NeighborTableM;

    FlowDebugPrinter.FlowDebug -> FlowM.FlowDebug;

    SubscribeInterest.NetTime -> FlowM.NetTime;

    PublishInterest.NetTime -> FlowM.NetTime;

#if defined(PLATFORM_TELOSB)
    SysTime.LocalTime -> TimerC;
#endif /* defined(PLATFORM_TELOSB) */

    TimerC.StdControl = StdControl;

    TheLinkEstimator.StdControl = StdControl;
    TheLinkEstimator.BeaconSendMsg = SendMsg[CHAN + 1];
    TheLinkEstimator.BeaconReceiveMsg = ReceiveMsg[CHAN + 1];
    TheLinkEstimator.NeighborTable -> NeighborTableM;
    TheLinkEstimator.Timer -> TimerC.Timer[unique("Timer")];
    TheLinkEstimator.Random -> Random;

    FlowSeqno.Random -> Random;
    FlowSeqno.NetTime -> FlowM.NetTime;

    NeighborTableM.NetTime -> FlowM.NetTime;

    FlowM.StdControl = StdControl;

    FlowM.SendMsg = SendMsg[CHAN];
    FlowM.UrgentSendMsg = UrgentSendMsg[CHAN];
    FlowM.ReceiveMsg = ReceiveMsg[CHAN];

    FlowM.SubscribeInterest -> SubscribeInterest;
    FlowM.PublishInterest -> PublishInterest;

    FlowM.FlowSeqno -> FlowSeqno;

    FlowM.NeighborTable -> NeighborTableM;

    FlowM.Pool -> Pool;
    FlowM.SendQueue -> SendQueue;
    FlowM.ReceiveQueue -> ReceiveQueue;

    FlowM.LinkEstimator -> TheLinkEstimator;

    FlowM.SysTime -> SysTime;
    FlowM.RefreshTimer -> TimerC.Timer[unique("Timer")];

#if defined(FLOW_APP_ACKS)
    FlowM.AckTimer -> TimerC.Timer[unique("Timer")];
#endif /* defined(FLOW_APP_ACKS) */

#if defined(TOSMSG_HAS_ACK)
    FlowM.MacControl -> CC2420RadioM.MacControl;
#endif /* defined(TOSMSG_HAS_ACK) */
}
