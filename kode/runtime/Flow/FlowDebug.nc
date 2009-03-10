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

interface FlowDebug
{
    event void forwardNeighborTable(uint16_t node_id, metric_t metric);

    event void dropSeqno(flowid_t flow_id, uint16_t src, flowseqno_t seqno);

    event void sendData(flowid_t flow_id, uint16_t dest_id,
                        uint16_t src, flowseqno_t seqno,
                        uint16_t next_hop, bool multicast);
    event void fwdData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                       uint16_t next_hop);
    event void resendData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                          uint16_t next_hop, bool ack_failure);
    event void dropData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                        uint16_t next_hop, bool ack_failure);
    event void sendDoneData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                            uint16_t next_hop);
    event void receiveData(flowid_t flow_id, uint16_t src, flowseqno_t seqno);

    event void bcast(flowid_t flow_id, uint16_t src, flowseqno_t seqno);
    event void fwdBcast(flowid_t flow_id, uint16_t src, flowseqno_t seqno);
    event void receiveBcast(flowid_t flow_id, uint16_t src, flowseqno_t seqno);

    event void ack(uint16_t sender, uint16_t src, flowseqno_t seqno);
    event void receiveAck(uint16_t acker, uint16_t src, flowseqno_t seqno);

    event void addSubscription(flowid_t flow_id,
                               uint16_t subscriber, flowseqno_t seqno,
                               uint16_t next_hop, metric_t metric);
    event void removeSubscription(flowid_t flow_id, uint16_t subscriber);
    event void refreshSubscription(flowid_t flow_id,
                                   uint16_t subscriber, flowseqno_t seqno);
    event void purgeSubscription(flowid_t flow_id, uint16_t subscriber);

    event void addPublication(flowid_t flow_id,
                              uint16_t publisher, flowseqno_t seqno,
                              uint16_t next_hop, metric_t metric);
    event void removePublication(flowid_t flow_id, uint16_t publisher);
    event void refreshPublication(flowid_t flow_id,
                                  uint16_t publisher, flowseqno_t seqno);
    event void purgePublication(flowid_t flow_id, uint16_t publisher);
}
