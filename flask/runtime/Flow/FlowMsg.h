/*
 * Copyright (c) 2005
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

#ifndef __FLOWMSG_H__
#define __FLOWMSG_H__

#include "Flow.h"

enum {
    AM_FLOWMSG = 128
};

#define FLOW_VERSION 0x15

struct MsgHeader {
    uint16_t type;
    uint16_t version;
};

struct AckMsg {
    uint16_t    type;
    uint16_t    version;
    uint16_t    src;   /* Originating source of the data being acked */
    flowseqno_t seqno; /* Sequence number */
    uint16_t    acker; /* Node sending the ACK */
};

struct SubscriptionMsg
{
    uint16_t    type;
    uint16_t    version;
    flowid_t    flow_id; /* Flow to subscribe to */
    uint16_t    src;     /* Subscriber/publisher */
    flowseqno_t seqno;   /* Sequence number */
    uint16_t    flags;
    uint16_t    ttl;
    uint16_t    repeat;
    uint16_t    next_hop;
    metric_t    metric;  /* Outgoing link metric seen by next_hop */
};

enum {
    SUBSCRIBE_SUBSCRIBE = (1 << 0),
    SUBSCRIBE_UNSUBSCRIBE = (1 << 1),
    SUBSCRIBE_PUBLISH = (1 << 2),
    SUBSCRIBE_UNPUBLISH = (1 << 3)
};

struct SendMsg
{
    uint16_t    type;
    uint16_t    version;
    flowid_t    flow_id; /* Flow to send to */
    uint16_t    src;     /* Originating sender of the data */
    flowseqno_t seqno;
    uint16_t    flags;
    uint16_t    prev_hop;
};

enum {
    SEND_MULTICAST = (1 << 0)
};

struct BcastMsg
{
    uint16_t    type;
    uint16_t    version;
    flowid_t    flow_id; /* Flow to broadcast to */
    uint16_t    src;     /* Originating sender of the data */
    flowseqno_t seqno;
    uint16_t    ttl;
    uint16_t    repeat;
    uint16_t    prev_hop;
};

enum {
    FLOW_MSG_ACK = 0,
    FLOW_MSG_SUB,
    FLOW_MSG_SEND,
    FLOW_MSG_BCAST
};

union FlowMsg
{
    struct MsgHeader header;
    struct AckMsg ack;
    struct SubscriptionMsg sub;
    struct SendMsg send;
    struct BcastMsg bcast;
};

#endif /* __FLOWMSG_H__ */
