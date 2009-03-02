/*
 * Copyright (c) 2007-2008
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

#define FLOW_CHAN 128
#define MAX_SUB 1
#define MAX_PUB 1
#define MAX_SEQNO_CACHE 10
#define MAX_NEIGHBORS 10

#define MAX_BUFFERS 10
#define MAX_FLOW_ACTIONS 10
#define MAX_MESSAGES MAX_BUFFERS + MAX_FLOW_ACTIONS

configuration EWMA {
} implementation {
    components Main;
    components LedsC as Leds;
    components NoLeds as NoLeds;
    components TimerC;

    components GenericComm as GenericComm;
#if defined(LEAN)
    components new QueuedComm(MAX_MESSAGES) as Comm;
#else /* !defined(LEAN) */
    components new BufferedComm(MAX_BUFFERS, MAX_MESSAGES) as Comm;
#endif /* !defined(LEAN) */
    components new FlowC(FLOW_CHAN,
                         MAX_SUB,
                         MAX_PUB,
                         MAX_SEQNO_CACHE,
                         MAX_NEIGHBORS,
                         MAX_FLOW_ACTIONS) as Flow;
    components EWMAC;

    Main.StdControl -> TimerC;
#if !defined(LEAN)
    Main.StdControl -> GenericComm;
#endif /* !defined(LEAN) */
    Main.StdControl -> Comm;
    Main.StdControl -> Flow;
    Main.StdControl -> EWMAC;

    Comm.BaseSendMsg -> GenericComm;
    Comm.BaseReceiveMsg -> GenericComm;

    Flow.SendMsg -> Comm.SendMsg;
    Flow.UrgentSendMsg -> Comm.UrgentSendMsg;
    Flow.ReceiveMsg -> Comm.ReceiveMsg;

    EWMAC.Flow -> Flow.Flow;
}
