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

#ifndef __FLOW_H__
#define __FLOW_H__

#include "metric.h"
#include "NetTime.h"

/*
 * Default number of times to retransmit broadcast pub/sub requests.
 */
#ifndef FLOW_DEFAULT_PUBSUB_REPEAT
#define FLOW_DEFAULT_PUBSUB_REPEAT 1
#endif /* FLOW_DEFAULT_PUBSUB_REPEAT */

/*
 * Default number of seconds between subscription/publication refreshes.
 */
#ifndef FLOW_DEFAULT_REFRESH_TIME
#define FLOW_DEFAULT_REFRESH_TIME 30
#endif /* FLOW_DEFAULT_REFRESH_TIME */

/* Default interval between link estimation beacons */
#ifndef FLOW_DEFAULT_BEACON_INTERVAL
#define FLOW_DEFAULT_BEACON_INTERVAL 1UL*1024UL /* 1 second */
#endif /* FLOW_DEFAULT_BEACON_INTERVAL */

/*
 * Timeout subscriptions/publications if they haven't been refreshed within
 * FLOW_DEFAULT_PURGE_FACTOR multiples of the refresh time.
 */
#if !defined(FLOW_DEFAULT_PURGE_FACTOR)
#define FLOW_DEFAULT_PURGE_FACTOR 3U
#endif /* !defined(FLOW_DEFAULT_PURGE_FACTORMULTIPLE) */

/* Max number of times to retransmit if ACK fails */
#if !defined(FLOW_DEFAULT_MAX_ACK_RETRANS)
#define FLOW_DEFAULT_MAX_ACK_RETRANS 5
#endif /* !defined(FLOW_DEFAULT_MAX_ACK_RETRANS) */

/* Sequence numbers only conflict if within this window */
#if !defined(FLOW_SEQ_WINDOW)
#define FLOW_SEQ_WINDOW 15
#endif /* !defined(FLOW_SEQ_WINDOW) */

/* Broadcasts heard within FLOW_BCAST_RECENT_TIME seconds are considered
 * "recent." When we eject sequence numbers from the sequence number table we do
 * not eject recent broadcasts.
 */
#if !defined(FLOW_BCAST_RECENT_TIME)
#define FLOW_BCAST_RECENT_TIME 30U
#endif /* !defined(FLOW_BCAST_RECENT_TIME) */

/* We squelch broadcast sequence numbers for this number of seconds if they
 * don't fall within our window. This prevents broadcast storms resulting from
 * resetting sequence numbers.
 */
#if !defined(FLOW_SEQ_SQUELCH_TIME)
#define FLOW_SEQ_SQUELCH_TIME 30U
#endif /* !defined(FLOW_SEQ_SQUELCH_TIME) */

/* Length of an ACK tick milliseconds (1/1024th of a second) */
#if !defined(FLOW_APP_ACK_TICK)
#define FLOW_APP_ACK_TICK 64U
#endif /* !defined(FLOW_APP_ACK_TICK) */

/* Number of ACK ticks to wait for an ACK */
#if !defined(FLOW_APP_ACK_WAIT)
#define FLOW_APP_ACK_WAIT 4
#endif /* !defined(FLOW_APP_ACK_WAIT) */

#if defined(PLATFORM_MICAZ) || defined(PLATFORM_TELOSB)
#define TOSMSG_HAS_LQI
#endif /* defined(PLATFORM_MICAZ) || defined(PLATFORM_TELOSB) */

#if defined(PLATFORM_MICAZ) && !defined(FLOW_APP_ACKS)
#define TOSMSG_HAS_ACK
#endif /* defined(PLATFORM_MICAZ) && !defined(FLOW_APP_ACKS) */

#if defined(PLATFORM_TELOSB) && !defined(FLOW_APP_ACKS)
#define TOSMSG_HAS_ACK
#endif /* defined(PLATFORM_TELOSB) && !defined(FLOW_APP_ACKS) */

#if defined(TOSMSG_HAS_ACK) || defined(FLOW_APP_ACKS)
#define FLOW_APP_USE_ACK
#endif /* (TOSMSG_HAS_ACK) || defined(FLOW_APP_ACKS) */

#define MAX_FLOW 0xFFFF

#define MAX_FLOW_TTL 255

typedef uint16_t flowid_t;

typedef uint16_t flowseqno_t;

struct flowint_t {
    flowid_t  flow_id;
    uint16_t  node_id;
    uint16_t  next_hop;
    metric_t  metric;
    uint16_t  flags;
    nettime_t timestamp;
};

struct flowstats_t {
    uint16_t radio_send_unicast_cnt;
    uint16_t radio_send_bcast_cnt;
    uint16_t radio_recv_unicast_cnt;
    uint16_t radio_recv_bcast_cnt;
    uint16_t send_cnt;
    uint16_t bcast_cnt;
    uint16_t recv_cnt;
    uint16_t send_fwd_cnt;
    uint16_t bcast_fwd_cnt;
    uint16_t sub_fwd_cnt;
    uint16_t unsub_fwd_cnt;
    uint16_t pub_fwd_cnt;
    uint16_t unpub_fwd_cnt;
    uint16_t alloc_failure_cnt;
    uint16_t send_failure_cnt;
    uint16_t sendDone_failure_cnt;
    uint16_t radio_ack_failure_cnt;
    uint16_t app_ack_failure_cnt;
    uint16_t drop_cnt;
    uint16_t seqno_suppression_cnt;
};

enum {
    FLOWINT_ACTIVE = 1 << 0 /* If subscription is active */
};

#endif /* __FLOW_H__ */
