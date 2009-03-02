/*
 * Copyright (c) 2005-2006
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

#ifndef __FLOWACTION_H__
#define __FLOWACTION_H__

#include "Flow.h"

enum {
    FLOW_ACTION_NONE = -1,
    FLOW_ACTION_SEND_FIRST = 0,
    FLOW_ACTION_SEND_SUBSCRIBE = FLOW_ACTION_SEND_FIRST,
    FLOW_ACTION_SEND_UNSUBSCRIBE,
    FLOW_ACTION_SEND_PUBLISH,
    FLOW_ACTION_SEND_UNPUBLISH,
    FLOW_ACTION_SEND_DATA,
    FLOW_ACTION_SEND_BCAST,
#if defined(FLOW_APP_ACKS)
    FLOW_ACTION_SEND_ACK,
    FLOW_ACTION_SEND_LAST = FLOW_ACTION_SEND_ACK,
#else /* !defined(FLOW_APP_ACKS) */
    FLOW_ACTION_SEND_LAST = FLOW_ACTION_SEND_BCAST,
#endif /* !defined(FLOW_APP_ACKS) */
    FLOW_ACTION_RECEIVE_FIRST,
    FLOW_ACTION_RECEIVE_SUBSCRIPTION = FLOW_ACTION_RECEIVE_FIRST,
    FLOW_ACTION_RECEIVE_DATA,
    FLOW_ACTION_RECEIVE_BCAST,
#if defined(FLOW_APP_ACKS)
    FLOW_ACTION_RECEIVE_ACK,
    FLOW_ACTION_RECEIVE_LAST = FLOW_ACTION_RECEIVE_ACK,
#else /* !defined(FLOW_APP_ACKS) */
    FLOW_ACTION_RECEIVE_LAST = FLOW_ACTION_RECEIVE_BCAST,
#endif /* !defined(FLOW_APP_ACKS) */
};

struct flowaction_t;

struct flow_send_data_t {
    struct flowint_t* interest; /* interest struct for the current destination,
                                 * NULL if none */
    int idx; /* Index of the current destination, undefined if none */
#if defined(FLOW_APP_USE_ACK)
    uint16_t ack_retran_count;
#endif /* (FLOW_APP_USE_ACK) */
#if defined(FLOW_APP_ACKS)
    struct flowaction_t* next;
    uint16_t ack_ticks_remaining;
#endif /* defined(FLOW_APP_ACKS) */
};

#if defined(FLOW_APP_ACKS)
struct flow_send_ack_t {
    uint16_t dest;
};
#endif /* defined(FLOW_APP_ACKS) */

struct flow_send_bcast_t {
    uint16_t remaining; /* Remaining number of broadcasts */
};

struct flow_send_subscribe_t {
    uint16_t remaining; /* Remaining number of broadcasts */
    struct flowint_t* pubinterest; /* interest struct for the current active
                                    * publisher destination, NULL if none */
    int pubidx; /* Index of the current active publisher destination, undefined
                 * if none */
    struct flowint_t* subinterest; /* interest struct for the current active
                                    * subscription we're resending, NULL if
                                    * none */
    int subidx; /* Index of the current active subscription we're resending,
                 * undefined if none */
#if defined(FLOW_APP_USE_ACK)
    uint16_t ack_retran_count;
#endif /* (FLOW_APP_USE_ACK) */
};

struct flow_send_publish_t {
    uint16_t remaining; /* Remaining number of broadcasts */
    struct flowint_t* pubinterest; /* interest struct for the current active
                                    * publication, NULL if none*/
    int pubidx; /* Index of the current destination, undefined if none */
};

struct flowaction_t {
    TOS_Msg msg;
    int16_t type;
    uint16_t urgent;
    union {
        struct flow_send_data_t send_data;
#if defined(FLOW_APP_ACKS)
        struct flow_send_ack_t send_ack;
#endif /* defined(FLOW_APP_ACKS) */
        struct flow_send_bcast_t send_bcast;
        struct flow_send_subscribe_t send_subscribe;
        struct flow_send_publish_t send_publish;
    } u;
};

#endif /* __FLOWACTION_H__ */
