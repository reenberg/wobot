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

#include "fassert.h"
#include "Flow.h"
#include "FlowAction.h"

#define MAX_PUBSUB 5

#define PUBSUB_REPEAT 1

module FlowVizSeqnoM {
    provides interface StdControl;
    provides interface FlowVizSeqno;

    uses interface Timer;
    uses interface BufferedSend;
    uses interface ReceiveMsg;
    uses interface Flow;
    uses interface FlowControl;
} implementation {
    struct seqnomsg_t mSeqnoMsg;

    uint32_t mInterval = 0;
    flowid_t mSubscriptions[MAX_PUBSUB];
    flowid_t mPublications[MAX_PUBSUB];

    void
    setInterval(uint32_t interval)
    {
        mInterval = interval;

        call Timer.stop();
        if (mInterval != 0)
            call Timer.start(TIMER_REPEAT, mInterval);
    }

    result_t
    subscribe(flowid_t flow_id, bool active)
    {
        int i;
        int unused = -1;

        for (i = 0; i < MAX_PUBSUB; ++i) {
            if (unused == -1 && mSubscriptions[i] == MAX_FLOW)
                unused = i;
            else if (mSubscriptions[i] == flow_id)
                return SUCCESS;
        }

        if (unused != -1) {
            mSubscriptions[unused] = flow_id;
            return call Flow.subscribe(flow_id, active);
        } else
            return FAIL;
    }

    void
    unsubscribe(flowid_t flow_id)
    {
        int i;

        for (i = 0; i < MAX_PUBSUB; ++i) {
            if (mSubscriptions[i] == flow_id) {
                mSubscriptions[i] = MAX_FLOW;
                return call Flow.unsubscribe(flow_id);
            }
        }
    }

    result_t
    publish(flowid_t flow_id, bool active)
    {
        int i;
        int unused = -1;

        for (i = 0; i < MAX_PUBSUB; ++i) {
            if (unused == -1 && mPublications[i] == MAX_FLOW)
                unused = i;
            else if (mPublications[i] == flow_id)
                return SUCCESS;
        }

        if (unused != -1) {
            mPublications[unused] = flow_id;
            return call Flow.publish(flow_id, active);
        } else
            return FAIL;
    }

    void
    unpublish(flowid_t flow_id)
    {
        int i;

        for (i = 0; i < MAX_PUBSUB; ++i) {
            if (mPublications[i] == flow_id) {
                mPublications[i] = MAX_FLOW;
                return call Flow.unpublish(flow_id);
            }
        }
    }

    result_t
    anycast(flowid_t flow_id)
    {
        result_t res;

        res = call Flow.anycast(flow_id, &mSeqnoMsg, sizeof(mSeqnoMsg));
        if (res == SUCCESS)
            ++mSeqnoMsg.seqno;
        return res;
    }

    result_t
    multicast(flowid_t flow_id)
    {
        result_t res;

        res = call Flow.multicast(flow_id, &mSeqnoMsg, sizeof(mSeqnoMsg));
        if (res == SUCCESS)
            ++mSeqnoMsg.seqno;
        return res;
    }

    result_t
    flood(flowid_t flow_id,
          uint8_t ttl, uint8_t repeat)
    {
        result_t res;

        res = call Flow.flood(flow_id, &mSeqnoMsg, sizeof(mSeqnoMsg),
                              ttl, repeat);
        if (res == SUCCESS)
            ++mSeqnoMsg.seqno;
        return res;
    }

    /**********************************************************************
     *
     * FlowVizSeqno interface
     *
     **********************************************************************/

    command void
    FlowVizSeqno.init()
    {
        int i;

        mInterval = 0;

        mSeqnoMsg.src = TOS_LOCAL_ADDRESS;
        mSeqnoMsg.seqno = 0;

        for (i = 0; i < MAX_PUBSUB; ++i) {
            mSubscriptions[i] = MAX_FLOW;
            mPublications[i] = MAX_FLOW;
        }
    }

    command result_t
    FlowVizSeqno.subscribe(flowid_t flow_id, bool active)
    {
        return subscribe(flow_id, active);
    }

    command void
    FlowVizSeqno.unsubscribe(flowid_t flow_id)
    {
        unsubscribe(flow_id);
    }

    command result_t
    FlowVizSeqno.publish(flowid_t flow_id, bool active)
    {
        return publish(flow_id, active);
    }

    command void
    FlowVizSeqno.unpublish(flowid_t flow_id)
    {
        unpublish(flow_id);
    }

    command result_t
    FlowVizSeqno.anycast(flowid_t flow_id)
    {
        return anycast(flow_id);
    }

    command result_t
    FlowVizSeqno.multicast(flowid_t flow_id)
    {
        return multicast(flow_id);
    }

    command result_t
    FlowVizSeqno.flood(flowid_t flow_id,
                       uint8_t ttl, uint8_t repeat)
    {
        return flood(flow_id, ttl, repeat);
    }

    command uint32_t
    FlowVizSeqno.getInterval()
    {
        return mInterval;
    }

    command void
    FlowVizSeqno.setInterval(uint32_t interval)
    {
        setInterval(interval);
    }

    /**********************************************************************
     *
     * Timer interface
     *
     **********************************************************************/

    event result_t
    Timer.fired()
    {
        int      i;
        bool     sent = FALSE;
        flowid_t flow_id = 0;

        for (i = 0; i < MAX_PUBSUB; ++i) {
            if (mPublications[i] != MAX_FLOW) {
                flow_id = mPublications[i];

                if (call Flow.anycast(mPublications[i],
                                      &mSeqnoMsg, sizeof(mSeqnoMsg)) == SUCCESS)
                    sent = TRUE;
            }
        }

        if (sent) {
            struct FlowVizSeqnoMsg msg;

            msg.type = FLOWVIZSEQNO_NOTIFY_SEND;
            msg.u.notifyRecv.flow_id = flow_id;
            msg.u.notifyRecv.src = mSeqnoMsg.src;
            msg.u.notifyRecv.seqno = mSeqnoMsg.seqno;

            call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));

            ++mSeqnoMsg.seqno;
        }

        return SUCCESS;
    }

    /**********************************************************************
     *
     * ReceiveMsg interface
     *
     **********************************************************************/

    void sendSeqno()
    {
        struct FlowVizSeqnoMsg msg;

        msg.type = FLOWVIZSEQNO_SEQNO;
        msg.u.seqno.seqno = mSeqnoMsg.seqno;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    void sendInterval()
    {
        struct FlowVizSeqnoMsg msg;

        msg.type = FLOWVIZSEQNO_INTERVAL;
        msg.u.interval.interval = mInterval;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event TOS_MsgPtr
    ReceiveMsg.receive(TOS_MsgPtr m)
    {
        struct FlowVizSeqnoMsg* in = (struct FlowVizSeqnoMsg*) m->data;

        if (m->length != sizeof(*in))
            return m;

        switch (in->type) {
            case FLOWVIZSEQNO_RESET:
                call FlowVizSeqno.init();
                break;

            case FLOWVIZSEQNO_SET_SEQNO:
                mSeqnoMsg.seqno = in->u.seqno.seqno;
                break;

            case FLOWVIZSEQNO_GET_SEQNO:
                sendSeqno();
                break;

            case FLOWVIZSEQNO_SET_INTERVAL:
                setInterval(in->u.interval.interval);
                break;

            case FLOWVIZSEQNO_GET_INTERVAL:
                sendInterval();
                break;

            case FLOWVIZSEQNO_ANYCAST:
                anycast(in->u.send.flow_id);
                break;

            case FLOWVIZSEQNO_MULTICAST:
                multicast(in->u.send.flow_id);
                break;

            case FLOWVIZSEQNO_FLOOD:
                flood(in->u.bcast.flow_id,
                      in->u.bcast.ttl, in->u.bcast.repeat);
                break;

            case FLOWVIZSEQNO_SUBSCRIBE:
                subscribe(in->u.pubsub.flow_id, in->u.pubsub.active);
                break;

            case FLOWVIZSEQNO_UNSUBSCRIBE:
                unsubscribe(in->u.pubsub.flow_id);
                break;

            case FLOWVIZSEQNO_PUBLISH:
                publish(in->u.pubsub.flow_id, in->u.pubsub.active);
                break;

            case FLOWVIZSEQNO_UNPUBLISH:
                unpublish(in->u.pubsub.flow_id);
                break;
        }

        return m;
    }

    /**********************************************************************
     *
     * Flow interface
     *
     **********************************************************************/

    event void
    Flow.receive(flowid_t flow_id, void* data, size_t size)
    {
        struct FlowVizSeqnoMsg msg;
        struct seqnomsg_t      seqnoMsg;

        memcpy(&seqnoMsg, data, sizeof(seqnoMsg));

        signal FlowVizSeqno.receive(flow_id, seqnoMsg.src, seqnoMsg.seqno);

        msg.type = FLOWVIZSEQNO_NOTIFY_RECV;
        msg.u.notifyRecv.flow_id = flow_id;
        msg.u.notifyRecv.src = seqnoMsg.src;
        msg.u.notifyRecv.seqno = seqnoMsg.seqno;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    /**********************************************************************
     *
     * StdControl interface
     *
     **********************************************************************/
    command result_t
    StdControl.init()
    {
        call FlowVizSeqno.init();
        return SUCCESS;
    }

    command result_t
    StdControl.start()
    {
        call FlowControl.setPubsubRepeat(PUBSUB_REPEAT);
        return SUCCESS;
    }

    command result_t
    StdControl.stop()
    {
        return SUCCESS;
    }
}
