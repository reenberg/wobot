/*
 * Copyright (c) 2006-2007
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

extern uint8_t TOSH_sched_full;
extern volatile uint8_t TOSH_sched_free;

module FlowVizSnoopM {
    uses interface Leds;
    uses interface SysTime;
    uses interface BufferedSend;
    uses interface ReceiveMsg;
    uses interface Flow;
    uses interface FlowControl;
    uses interface FlowDebug;
    uses interface NeighborTable;
    uses interface LinkEstimator;
} implementation {
    /**********************************************************************
     *
     * ReceiveMsg interface
     *
     **********************************************************************/
    void sendBeaconInterval()
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_BEACON_INTERVAL;
        msg.timestamp = call SysTime.getTime32();
        msg.u.interval.interval = call LinkEstimator.getBeaconInterval();

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    void sendPubsubRepeat()
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_PUBSUB_REPEAT;
        msg.timestamp = call SysTime.getTime32();
        msg.u.repeat.repeat = call FlowControl.getPubsubRepeat();

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    void sendRefreshTime()
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_REFRESH_TIME;
        msg.timestamp = call SysTime.getTime32();
        msg.u.refresh.refresh = call FlowControl.getRefreshTime();

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    void sendPurgeFactor()
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_PURGE_FACTOR;
        msg.timestamp = call SysTime.getTime32();
        msg.u.factor.factor = call FlowControl.getPurgeFactor();

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    void sendMaxAckRetrans()
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_MAX_ACK_RETRANS;
        msg.timestamp = call SysTime.getTime32();
        msg.u.maxRetrans.maxRetrans = call FlowControl.getMaxAckRetrans();

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    void sendPong()
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_PONG;
        msg.timestamp = call SysTime.getTime32();
        msg.u.ping.addr = TOS_LOCAL_ADDRESS;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));

    }

    void sendStats()
    {
        struct FlowVizSnoopMsg    msg;
        const struct flowstats_t* stats;

        stats = call FlowControl.stats();

        msg.type = FLOWVIZSNOOP_STATS;
        msg.timestamp = call SysTime.getTime32();
        memcpy(&(msg.u.stats.flow_stats), stats, sizeof(*stats));

#if defined(TOSH_STATS)
        {
            int temp = ((int) TOSH_sched_free) - TOSH_sched_full;

            if (temp < 0)
                msg.u.stats.tosh_pending = temp + TOSH_MAX_TASKS;
            else
                msg.u.stats.tosh_pending = temp;
        }
#endif /* defined(TOSH_STATS) */

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    void sendNeighbors()
    {
        struct FlowVizSnoopMsg msg;
        int                    count;
        int                    pos;
        uint16_t               addr;
        metric_t               metric;

        pos = call NeighborTable.firstForward(&addr, &metric);

        msg.type = FLOWVIZSNOOP_NEIGHBORS;
        msg.timestamp = call SysTime.getTime32();
        count = 0;

        for (; pos != -1;
             pos = call NeighborTable.nextForward(pos, &addr, &metric)) {
            msg.u.neighbors.neighbors[count].addr = addr;
            msg.u.neighbors.neighbors[count].metric = metric;

            if (++count == FLOWVIZSNOOP_MAX_NEIGHBORS) {
                msg.u.neighbors.count = count;
                call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));

                count = 0;
            }
        }

        if (count > 0) {
            msg.u.neighbors.count = count;
            call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
        }
    }

    event TOS_MsgPtr
    ReceiveMsg.receive(TOS_MsgPtr m)
    {
        struct FlowVizSnoopMsg* in = (struct FlowVizSnoopMsg*) m->data;

        if (m->length != sizeof(*in))
            return m;

        switch (in->type) {
            case FLOWVIZSNOOP_RESET:
                call Flow.init();
                break;

            case FLOWVIZSNOOP_SET_BEACON_INTERVAL:
                call LinkEstimator.setBeaconInterval(in->u.interval.interval);
                break;

            case FLOWVIZSNOOP_GET_BEACON_INTERVAL:
                sendBeaconInterval();
                break;

            case FLOWVIZSNOOP_SET_PUBSUB_REPEAT:
                call FlowControl.setPubsubRepeat(in->u.repeat.repeat);
                break;

            case FLOWVIZSNOOP_GET_PUBSUB_REPEAT:
                sendPubsubRepeat();
                break;

            case FLOWVIZSNOOP_SET_REFRESH_TIME:
                call FlowControl.setRefreshTime(in->u.refresh.refresh);
                break;

            case FLOWVIZSNOOP_GET_REFRESH_TIME:
                sendRefreshTime();
                break;

            case FLOWVIZSNOOP_SET_PURGE_FACTOR:
                call FlowControl.setPurgeFactor(in->u.factor.factor);
                break;

            case FLOWVIZSNOOP_GET_PURGE_FACTOR:
                sendPurgeFactor();
                break;

            case FLOWVIZSNOOP_SET_MAX_ACK_RETRANS:
                call FlowControl.setMaxAckRetrans(in->u.maxRetrans.maxRetrans);
                break;

            case FLOWVIZSNOOP_GET_MAX_ACK_RETRANS:
                sendMaxAckRetrans();
                break;

            case FLOWVIZSNOOP_PING:
                sendPong();
                break;

            case FLOWVIZSNOOP_GET_STATS:
                sendStats();
                break;

            case FLOWVIZSNOOP_GET_NEIGHBORS:
                sendNeighbors();
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
    }

    /**********************************************************************
     *
     * FlowDebug interface
     *
     **********************************************************************/
    event void
    FlowDebug.forwardNeighborTable(uint16_t node_id, metric_t metric)
    {
    }

    event void
    FlowDebug.dropSeqno(flowid_t flow_id, uint16_t src, flowseqno_t seqno)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_DROP_SEQNO;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyDropSeqno.flow_id = flow_id;
        msg.u.notifyDropSeqno.src = src;
        msg.u.notifyDropSeqno.seqno = seqno;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.sendData(flowid_t flow_id, uint16_t dest_id,
                       uint16_t src, flowseqno_t seqno,
                       uint16_t next_hop, bool multicast)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_SEND_DATA;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifySendData.flow_id = flow_id;
        msg.u.notifySendData.dest_id = dest_id;
        msg.u.notifySendData.src = src;
        msg.u.notifySendData.seqno = seqno;
        msg.u.notifySendData.next_hop = next_hop;
        msg.u.notifySendData.multicast = multicast;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.fwdData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                      uint16_t next_hop)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_FWD_DATA;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyFwdData.flow_id = flow_id;
        msg.u.notifyFwdData.src = src;
        msg.u.notifyFwdData.seqno = seqno;
        msg.u.notifyFwdData.next_hop = next_hop;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.resendData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                         uint16_t next_hop, bool ack_failure)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_RESEND_DATA;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyResendData.flow_id = flow_id;
        msg.u.notifyResendData.src = src;
        msg.u.notifyResendData.seqno = seqno;
        msg.u.notifyResendData.next_hop = next_hop;
        msg.u.notifyResendData.ack_failure = ack_failure;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.dropData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                       uint16_t next_hop, bool ack_failure)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_DROP_DATA;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyDropData.flow_id = flow_id;
        msg.u.notifyDropData.src = src;
        msg.u.notifyDropData.seqno = seqno;
        msg.u.notifyDropData.next_hop = next_hop;
        msg.u.notifyDropData.ack_failure = ack_failure;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.sendDoneData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                           uint16_t next_hop)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_SENDDONE_DATA;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyResendData.flow_id = flow_id;
        msg.u.notifyResendData.src = src;
        msg.u.notifyResendData.seqno = seqno;
        msg.u.notifyResendData.next_hop = next_hop;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.receiveData(flowid_t flow_id, uint16_t src, flowseqno_t seqno)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_RECV_DATA;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyRecvData.flow_id = flow_id;
        msg.u.notifyRecvData.src = src;
        msg.u.notifyRecvData.seqno = seqno;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.bcast(flowid_t flow_id, uint16_t src, flowseqno_t seqno)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_SEND_BCAST;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifySendBcast.flow_id = flow_id;
        msg.u.notifySendBcast.src = src;
        msg.u.notifySendBcast.seqno = seqno;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.fwdBcast(flowid_t flow_id, uint16_t src, flowseqno_t seqno)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_FWD_BCAST;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyFwdBcast.flow_id = flow_id;
        msg.u.notifyFwdBcast.src = src;
        msg.u.notifyFwdBcast.seqno = seqno;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.receiveBcast(flowid_t flow_id, uint16_t src, flowseqno_t seqno)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_RECV_BCAST;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyRecvBcast.flow_id = flow_id;
        msg.u.notifyRecvBcast.src = src;
        msg.u.notifyRecvBcast.seqno = seqno;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.ack(uint16_t sender, uint16_t src, flowseqno_t seqno)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_ACK;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyAck.sender = sender;
        msg.u.notifyAck.src = src;
        msg.u.notifyAck.seqno = seqno;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.receiveAck(uint16_t acker, uint16_t src, flowseqno_t seqno)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_RECV_ACK;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyRecvAck.acker = acker;
        msg.u.notifyRecvAck.src = src;
        msg.u.notifyRecvAck.seqno = seqno;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.addSubscription(flowid_t flow_id,
                              uint16_t subscriber, flowseqno_t seqno,
                              uint16_t next_hop, metric_t metric)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_ADD_SUB;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyAddSub.flow_id = flow_id;
        msg.u.notifyAddSub.subscriber = subscriber;
        msg.u.notifyAddSub.seqno = seqno;
        msg.u.notifyAddSub.next_hop = next_hop;
        msg.u.notifyAddSub.metric = metric;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.removeSubscription(flowid_t flow_id, uint16_t subscriber)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_REM_SUB;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyRemSub.flow_id = flow_id;
        msg.u.notifyRemSub.subscriber = subscriber;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.refreshSubscription(flowid_t flow_id,
                                  uint16_t subscriber, flowseqno_t seqno)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_REFRESH_SUB;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyRefreshSub.flow_id = flow_id;
        msg.u.notifyRefreshSub.subscriber = subscriber;
        msg.u.notifyRefreshSub.seqno = seqno;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.purgeSubscription(flowid_t flow_id, uint16_t subscriber)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_PURGE_SUB;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyPurgeSub.flow_id = flow_id;
        msg.u.notifyPurgeSub.subscriber = subscriber;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.addPublication(flowid_t flow_id,
                             uint16_t publisher, flowseqno_t seqno,
                             uint16_t next_hop, metric_t metric)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_ADD_PUB;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyAddPub.flow_id = flow_id;
        msg.u.notifyAddPub.publisher = publisher;
        msg.u.notifyAddPub.seqno = seqno;
        msg.u.notifyAddPub.next_hop = next_hop;
        msg.u.notifyAddPub.metric = metric;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.removePublication(flowid_t flow_id, uint16_t publisher)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_REM_PUB;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyRemPub.flow_id = flow_id;
        msg.u.notifyRemPub.publisher = publisher;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.refreshPublication(flowid_t flow_id,
                                 uint16_t publisher, flowseqno_t seqno)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_REFRESH_PUB;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyRefreshPub.flow_id = flow_id;
        msg.u.notifyRefreshPub.publisher = publisher;
        msg.u.notifyRefreshPub.seqno = seqno;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event void
    FlowDebug.purgePublication(flowid_t flow_id, uint16_t publisher)
    {
        struct FlowVizSnoopMsg msg;

        msg.type = FLOWVIZSNOOP_NOTIFY_PURGE_PUB;
        msg.timestamp = call SysTime.getTime32();
        msg.u.notifyPurgePub.flow_id = flow_id;
        msg.u.notifyPurgePub.publisher = publisher;

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }
}
