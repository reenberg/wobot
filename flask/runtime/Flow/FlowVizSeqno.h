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

#ifndef __FLOWVIZSEQNO_H__
#define __FLOWVIZSEQNO_H__

#include "Flow.h"

#if !defined(_AM_FLOWVIZSEQNOMSG)
#define _AM_FLOWVIZSEQNOMSG 17
#endif /* !defined(_AM_FLOWVIZSEQNOMSG) */

enum {
    AM_FLOWVIZSEQNOMSG = _AM_FLOWVIZSEQNOMSG
};

enum {
    FLOWVIZSEQNO_RESET = 0,

    FLOWVIZSEQNO_SET_SEQNO,
    FLOWVIZSEQNO_GET_SEQNO,
    FLOWVIZSEQNO_SEQNO,

    FLOWVIZSEQNO_SET_INTERVAL,
    FLOWVIZSEQNO_GET_INTERVAL,
    FLOWVIZSEQNO_INTERVAL,

    FLOWVIZSEQNO_ANYCAST,
    FLOWVIZSEQNO_MULTICAST,
    FLOWVIZSEQNO_FLOOD,

    FLOWVIZSEQNO_SUBSCRIBE,
    FLOWVIZSEQNO_UNSUBSCRIBE,
    FLOWVIZSEQNO_PUBLISH,
    FLOWVIZSEQNO_UNPUBLISH,

    FLOWVIZSEQNO_NOTIFY_SEND,
    FLOWVIZSEQNO_NOTIFY_RECV
};

struct FlowVizSeqnoSeqno {
    flowseqno_t seqno;
};

struct FlowVizSeqnoInterval {
    uint32_t interval;
};

struct FlowVizSeqnoSend {
    uint16_t flow_id;
};

struct FlowVizSeqnoBcast {
    uint16_t flow_id;
    uint16_t ttl;
    uint16_t repeat;
};

struct FlowVizSeqnoPubsub {
    uint16_t flow_id;
    uint16_t active;
};

struct FlowVizSeqnoNotifySend {
    uint16_t flow_id;
    uint16_t src;
    flowseqno_t seqno;
};

struct FlowVizSeqnoNotifyRecv {
    uint16_t flow_id;
    uint16_t src;
    flowseqno_t seqno;
};

struct FlowVizSeqnoMsg {
    uint16_t type;
    union {
        struct FlowVizSeqnoSeqno seqno;
        struct FlowVizSeqnoInterval interval;
        struct FlowVizSeqnoSend send;
        struct FlowVizSeqnoBcast bcast;
        struct FlowVizSeqnoPubsub pubsub;
        struct FlowVizSeqnoNotifySend notifySend;
        struct FlowVizSeqnoNotifyRecv notifyRecv;
    } u;
};

struct seqnomsg_t {
    uint16_t    src;
    flowseqno_t seqno;
};

#endif /* __FLOWVIZSEQNO_H__ */
