/*
 * Copyright (c) 2005, 2007
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

#ifndef __FLOWVIZSNOOP_H__
#define __FLOWVIZSNOOP_H__

#if !defined(_AM_FLOWVIZSNOOPMSG)
#define _AM_FLOWVIZSNOOPMSG 16
#endif /* !defined(_AM_FLOWVIZSNOOPMSG) */

#include "metric.h"
#include "Flow.h"

#define TOSH_STATS
#define FLOWVIZSNOOP_MAX_NEIGHBORS 4

enum {
    AM_FLOWVIZSNOOPMSG = _AM_FLOWVIZSNOOPMSG
};

enum {
    FLOWVIZSNOOP_RESET = 0,

    FLOWVIZSNOOP_SET_BEACON_INTERVAL,
    FLOWVIZSNOOP_GET_BEACON_INTERVAL,
    FLOWVIZSNOOP_BEACON_INTERVAL,

    FLOWVIZSNOOP_SET_PUBSUB_REPEAT,
    FLOWVIZSNOOP_GET_PUBSUB_REPEAT,
    FLOWVIZSNOOP_PUBSUB_REPEAT,

    FLOWVIZSNOOP_SET_REFRESH_TIME,
    FLOWVIZSNOOP_GET_REFRESH_TIME,
    FLOWVIZSNOOP_REFRESH_TIME,

    FLOWVIZSNOOP_SET_PURGE_FACTOR,
    FLOWVIZSNOOP_GET_PURGE_FACTOR,
    FLOWVIZSNOOP_PURGE_FACTOR,

    FLOWVIZSNOOP_SET_MAX_ACK_RETRANS,
    FLOWVIZSNOOP_GET_MAX_ACK_RETRANS,
    FLOWVIZSNOOP_MAX_ACK_RETRANS,

    FLOWVIZSNOOP_PING,
    FLOWVIZSNOOP_PONG,

    FLOWVIZSNOOP_GET_STATS,
    FLOWVIZSNOOP_STATS,

    FLOWVIZSNOOP_GET_NEIGHBORS,
    FLOWVIZSNOOP_NEIGHBORS,

    FLOWVIZSNOOP_NOTIFY_SEND_DATA,
    FLOWVIZSNOOP_NOTIFY_FWD_DATA,
    FLOWVIZSNOOP_NOTIFY_RESEND_DATA,
    FLOWVIZSNOOP_NOTIFY_DROP_DATA,
    FLOWVIZSNOOP_NOTIFY_SENDDONE_DATA,
    FLOWVIZSNOOP_NOTIFY_RECV_DATA,
    FLOWVIZSNOOP_NOTIFY_SEND_BCAST,
    FLOWVIZSNOOP_NOTIFY_FWD_BCAST,
    FLOWVIZSNOOP_NOTIFY_RECV_BCAST,
    FLOWVIZSNOOP_NOTIFY_DROP_SEQNO,
    FLOWVIZSNOOP_NOTIFY_ACK,
    FLOWVIZSNOOP_NOTIFY_RECV_ACK,
    FLOWVIZSNOOP_NOTIFY_ADD_SUB,
    FLOWVIZSNOOP_NOTIFY_REM_SUB,
    FLOWVIZSNOOP_NOTIFY_REFRESH_SUB,
    FLOWVIZSNOOP_NOTIFY_PURGE_SUB,
    FLOWVIZSNOOP_NOTIFY_ADD_PUB,
    FLOWVIZSNOOP_NOTIFY_REM_PUB,
    FLOWVIZSNOOP_NOTIFY_REFRESH_PUB,
    FLOWVIZSNOOP_NOTIFY_PURGE_PUB
};

struct FlowVizSnoopBeaconInterval {
    uint32_t interval;
};

struct FlowVizSnoopPubsubRepeat {
    uint16_t repeat;
};

struct FlowVizSnoopRefreshTime {
    uint16_t refresh;
};

struct FlowVizSnoopPurgeFactor {
    uint16_t factor;
};

struct FlowVizSnoopMaxAckRetrans {
    uint16_t maxRetrans;
};

struct FlowVizSnoopPing {
    uint16_t addr;
};

struct FlowVizSnoopStats {
#if defined(TOSH_STATS)
    uint16_t tosh_pending;
#endif /* defined(TOSH_STATS) */
    struct flowstats_t flow_stats;
};

struct neighbor {
    uint16_t addr;
    metric_t metric;
};

struct FlowVizSnoopNeighbors {
    uint16_t count;
    struct neighbor neighbors[FLOWVIZSNOOP_MAX_NEIGHBORS];
};

struct FlowVizSnoopNotifySendData {
    uint16_t    flow_id;
    uint16_t    dest_id;
    uint16_t    src;
    flowseqno_t seqno;
    uint16_t    next_hop;
    uint16_t    multicast;
};

struct FlowVizSnoopNotifyFwdData {
    uint16_t    flow_id;
    uint16_t    src;
    flowseqno_t seqno;
    uint16_t    next_hop;
};

struct FlowVizSnoopNotifyResendData {
    uint16_t    flow_id;
    uint16_t    src;
    flowseqno_t seqno;
    uint16_t    next_hop;
    uint16_t    ack_failure;
};

struct FlowVizSnoopNotifyDropData {
    uint16_t    flow_id;
    uint16_t    src;
    flowseqno_t seqno;
    uint16_t    next_hop;
    uint16_t    ack_failure;
};

struct FlowVizSnoopNotifySendDoneData {
    uint16_t    flow_id;
    uint16_t    src;
    flowseqno_t seqno;
    uint16_t    next_hop;
};

struct FlowVizSnoopNotifyRecvData {
    uint16_t    flow_id;
    uint16_t    src;
    flowseqno_t seqno;
};

struct FlowVizSnoopNotifySendBcast {
    uint16_t    flow_id;
    uint16_t    src;
    flowseqno_t seqno;
    uint16_t    multicast;
};

struct FlowVizSnoopNotifyFwdBcast {
    uint16_t    flow_id;
    uint16_t    src;
    flowseqno_t seqno;
};

struct FlowVizSnoopNotifyRecvBcast {
    uint16_t    flow_id;
    uint16_t    src;
    flowseqno_t seqno;
};

struct FlowVizSnoopNotifyDropSeqno {
    uint16_t    flow_id;
    uint16_t    src;
    flowseqno_t seqno;
};

struct FlowVizSnoopNotifyAck {
    uint16_t    sender;
    uint16_t    src;
    flowseqno_t seqno;
};

struct FlowVizSnoopNotifyRecvAck {
    uint16_t    acker;
    uint16_t    src;
    flowseqno_t seqno;
};

struct FlowVizSnoopNotifyAddSub {
    uint16_t    flow_id;
    uint16_t    subscriber;
    flowseqno_t seqno;
    uint16_t    next_hop;
    metric_t    metric;
};

struct FlowVizSnoopNotifyRemSub {
    uint16_t flow_id;
    uint16_t subscriber;
};

struct FlowVizSnoopNotifyRefreshSub {
    uint16_t    flow_id;
    uint16_t    subscriber;
    flowseqno_t seqno;
};

struct FlowVizSnoopNotifyPurgeSub {
    uint16_t flow_id;
    uint16_t subscriber;
};

struct FlowVizSnoopNotifyAddPub {
    uint16_t    flow_id;
    uint16_t    publisher;
    flowseqno_t seqno;
    uint16_t    next_hop;
    metric_t    metric;
};

struct FlowVizSnoopNotifyRemPub {
    uint16_t flow_id;
    uint16_t publisher;
};

struct FlowVizSnoopNotifyRefreshPub {
    uint16_t    flow_id;
    uint16_t    publisher;
    flowseqno_t seqno;
};

struct FlowVizSnoopNotifyPurgePub {
    uint16_t flow_id;
    uint16_t publisher;
};

struct FlowVizSnoopMsg {
    uint16_t type;
    uint32_t timestamp;
    union {
        struct FlowVizSnoopBeaconInterval interval;
        struct FlowVizSnoopPubsubRepeat repeat;
        struct FlowVizSnoopRefreshTime refresh;
        struct FlowVizSnoopPurgeFactor factor;
        struct FlowVizSnoopMaxAckRetrans maxRetrans;
        struct FlowVizSnoopPing ping;
        struct FlowVizSnoopStats stats;
        struct FlowVizSnoopNeighbors neighbors;
        struct FlowVizSnoopNotifySendData notifySendData;
        struct FlowVizSnoopNotifyFwdData notifyFwdData;
        struct FlowVizSnoopNotifyResendData notifyResendData;
        struct FlowVizSnoopNotifyDropData notifyDropData;
        struct FlowVizSnoopNotifySendDoneData notifySendDoneData;
        struct FlowVizSnoopNotifyRecvData notifyRecvData;
        struct FlowVizSnoopNotifySendBcast notifySendBcast;
        struct FlowVizSnoopNotifyFwdBcast notifyFwdBcast;
        struct FlowVizSnoopNotifyRecvBcast notifyRecvBcast;
        struct FlowVizSnoopNotifyDropSeqno notifyDropSeqno;
        struct FlowVizSnoopNotifyAck notifyAck;
        struct FlowVizSnoopNotifyRecvAck notifyRecvAck;
        struct FlowVizSnoopNotifyAddSub notifyAddSub;
        struct FlowVizSnoopNotifyRemSub notifyRemSub;
        struct FlowVizSnoopNotifyRefreshSub notifyRefreshSub;
        struct FlowVizSnoopNotifyPurgeSub notifyPurgeSub;
        struct FlowVizSnoopNotifyAddPub notifyAddPub;
        struct FlowVizSnoopNotifyRemPub notifyRemPub;
        struct FlowVizSnoopNotifyRefreshPub notifyRefreshPub;
        struct FlowVizSnoopNotifyPurgePub notifyPurgePub;
    } u;
};

#endif /* __FLOWVIZSNOOP_H__ */
