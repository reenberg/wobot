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

module FlowDebugPrinter {
    uses interface FlowDebug;
} implementation {
    event void
    FlowDebug.forwardNeighborTable(uint16_t node_id, metric_t metric)
    {
        dbg(DBG_USR1, "flow: forward link metric to %d = %d\n",
            node_id, metric);
    }

    event void
    FlowDebug.dropSeqno(flowid_t flow_id, uint16_t src, flowseqno_t seqno)
    {
        dbg(DBG_USR1, "flow: drop seqno flow id %d src %d seqno %d\n",
            flow_id, src, seqno);
    }

    event void
    FlowDebug.sendData(flowid_t flow_id, uint16_t dest_id,
                       uint16_t src, flowseqno_t seqno,
                       uint16_t next_hop, bool multicast)
    {
        dbg(DBG_USR1, "flow: %s flow id %d src %d seqno %d dest %d via %d\n",
            multicast ? "send multicast" : "send anycast",
            flow_id, src, seqno, dest_id, next_hop);
    }

    event void
    FlowDebug.fwdData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                      uint16_t next_hop)
    {
        dbg(DBG_USR1, "flow: forward data flow id %d src %d seqno %d via %d\n",
            flow_id, src, seqno, next_hop);
    }

    event void
    FlowDebug.resendData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                         uint16_t next_hop, bool ack_failure)
    {
        dbg(DBG_USR1, "flow: resend %s flow id %d src %d seqno %d via %d\n",
            ack_failure ? "ack failure" : "sendDone failure",
            flow_id, src, seqno, next_hop);
    }

    event void
    FlowDebug.dropData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                       uint16_t next_hop, bool ack_failure)
    {
        dbg(DBG_USR1, "flow: drop %s flow id %d src %d seqno %d via %d\n",
            ack_failure ? "ack failure" : "sendDone failure",
            flow_id, src, seqno, next_hop);
    }

    event void
    FlowDebug.sendDoneData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                           uint16_t next_hop)
    {
        dbg(DBG_USR1, "flow: sendDone flow id %d src %d seqno %d via %d\n",
            flow_id, src, seqno, next_hop);
    }

    event void
    FlowDebug.receiveData(flowid_t flow_id, uint16_t src, flowseqno_t seqno)
    {
        dbg(DBG_USR1, "flow: receive data flow id %d from %d seqno %d\n",
            flow_id, src, seqno);
    }

    event void
    FlowDebug.bcast(flowid_t flow_id, uint16_t src, flowseqno_t seqno)
    {
        dbg(DBG_USR1, "flow: bcast flow id %d src %d seqno %d\n",
            flow_id, src, seqno);
    }

    event void
    FlowDebug.fwdBcast(flowid_t flow_id, uint16_t src, flowseqno_t seqno)
    {
        dbg(DBG_USR1, "flow: forward bcast flow id %d src %d seqno %d\n",
            flow_id, src, seqno);
    }

    event void
    FlowDebug.receiveBcast(flowid_t flow_id, uint16_t src, flowseqno_t seqno)
    {
        dbg(DBG_USR1, "flow: receive bcast flow id %d from %d seqno %d\n",
            flow_id, src, seqno);
    }

    event void
    FlowDebug.ack(uint16_t sender, uint16_t src, flowseqno_t seqno)
    {
        dbg(DBG_USR1, "flow: ack to %d source %d seqno %d\n",
            sender, src, seqno);
    }

    event void
    FlowDebug.receiveAck(uint16_t acker, uint16_t src, flowseqno_t seqno)
    {
        dbg(DBG_USR1, "flow: receive ack from %d source %d seqno %d\n",
            acker, src, seqno);
    }

    event void
    FlowDebug.addSubscription(flowid_t flow_id,
                              uint16_t subscriber, flowseqno_t seqno,
                              uint16_t next_hop, metric_t metric)
    {
        dbg(DBG_USR1, "flow: %d subscribed to flow id %d seqno %d next hop %d metric %f\n",
            subscriber, flow_id, seqno, next_hop, metric);
    }

    event void
    FlowDebug.removeSubscription(flowid_t flow_id, uint16_t subscriber)
    {
        dbg(DBG_USR1, "flow: %d unsubscribed from flow id %d\n",
            subscriber, flow_id);
    }

    event void
    FlowDebug.refreshSubscription(flowid_t flow_id,
                                  uint16_t subscriber, flowseqno_t seqno)
    {
        dbg(DBG_USR1, "flow: %d subscription refreshed to flow id %d seqno %d\n",
            subscriber, flow_id, seqno);
    }

    event void
    FlowDebug.purgeSubscription(flowid_t flow_id, uint16_t subscriber)
    {
        dbg(DBG_USR1, "flow: %d subscription purged to flow id %d\n",
            subscriber, flow_id);
    }

    event void
    FlowDebug.addPublication(flowid_t flow_id,
                             uint16_t publisher, flowseqno_t seqno,
                             uint16_t next_hop, metric_t metric)
    {
        dbg(DBG_USR1, "flow: %d publishing to flow id %d seqno %d next hop %d metric %f\n",
            publisher, flow_id, seqno, next_hop, metric);
    }

    event void
    FlowDebug.removePublication(flowid_t flow_id, uint16_t publisher)
    {
        dbg(DBG_USR1, "flow: %d unpublished to flow id %d\n",
            publisher, flow_id);
    }

    event void
    FlowDebug.refreshPublication(flowid_t flow_id,
                                 uint16_t publisher, flowseqno_t seqno)
    {
        dbg(DBG_USR1, "flow: %d publication refreshed to flow id %d seqno %d\n",
            publisher, flow_id, seqno);
    }

    event void
    FlowDebug.purgePublication(flowid_t flow_id, uint16_t publisher)
    {
        dbg(DBG_USR1, "flow: %d publication purged to flow id %d\n",
            publisher, flow_id);
    }
}
