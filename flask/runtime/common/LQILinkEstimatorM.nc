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

#include "metric.h"
#include "LQILinkEstimator.h"

module LQILinkEstimatorM {
    provides interface StdControl;
    provides interface LinkEstimator;

    uses interface NeighborTable;
    uses interface SendMsg as BeaconSendMsg;
    uses interface ReceiveMsg as BeaconReceiveMsg;
    uses interface Timer;
    uses interface Random;
} implementation {
    bool    mInited = FALSE;
    bool    mSending = FALSE;
    TOS_Msg mMsg;

    int      mBeaconPos;
    int      mNextBeaconPos;
    uint32_t mBeaconInterval;

    /*
     * adjustLQI is taken from the MultiHopLQI code
     *
     * - 80-(LQI-50) : takes LQI from range [50 bad ... 110 good]
     *   to val [80 bad ... 20 good]
     * - then takes val^3 / 64.
     * - Net result: Good LQIs are very low numbers, bad LQIs are very
     *   big numbers.
     */
    uint16_t adjustLQI(uint8_t val) {
        uint16_t result = (80 - (val - 50));
        result = (((result * result) >> 3) * result) >> 3;
        return result;
    }

    void
    sendBeacon()
    {
        if (!mSending) {
            struct LQIBeaconMsg* beacon = (struct LQIBeaconMsg*) mMsg.data;
            int i;

            mNextBeaconPos = mBeaconPos;

            for (beacon->count = 0; beacon->count < MAX_BEACON_METRICS;) {
                mNextBeaconPos = call NeighborTable.nextReverse(mNextBeaconPos,
                  &(beacon->metrics[beacon->count].node_id),
                  &(beacon->metrics[beacon->count].metric));

                if (mNextBeaconPos == -1)
                    mNextBeaconPos = 0;

                if (beacon->metrics[beacon->count].metric != WORST_METRIC)
                    ++beacon->count;
            }

            for (i = beacon->count; i < MAX_BEACON_METRICS; ++i) {
                beacon->metrics[i].node_id = UNUSED;
                beacon->metrics[i].node_id = WORST_METRIC;
            }

            if (call BeaconSendMsg.send(TOS_BCAST_ADDR,
                                        sizeof(struct LQIBeaconMsg),
                                        &mMsg) == SUCCESS)
                mSending = TRUE;
        }
    }

    void
    receiveBeacon(TOS_MsgPtr m)
    {
        struct LQIBeaconMsg* beacon = (struct LQIBeaconMsg*) m->data;
        metric_t             metric;
        int                  i;

        if (m->length != sizeof(struct LQIBeaconMsg))
            return;

#if defined(TOSMSG_HAS_LQI)
        metric = adjustLQI(m->lqi);
#else /* !defined(TOSMSG_HAS_LQI) */
        metric = DEFAULT_METRIC;
#endif /* !defined(TOSMSG_HAS_LQI) */

        call NeighborTable.updateReverse(beacon->src, metric);

        /* If the beaconing node heard from us before, record the link metric
         * for the forward path. Beacons contain a list of node/link metric
         * pairs that give the *reverse* link metric from the perspective of
         * the beaconing node. If the current node is in this list of metrics,
         * then we know our forward metric to the beaconing node.
         */
        for (i = 0; i < beacon->count; ++i) {
            if (beacon->metrics[i].node_id == TOS_LOCAL_ADDRESS) {
                call NeighborTable.updateForward(beacon->src,
                                                 beacon->metrics[i].metric);
                break;
            }
        }
    }

    /**********************************************************************
     *
     * StdControl interface
     *
     **********************************************************************/

    command result_t
    StdControl.init()
    {
        struct LQIBeaconMsg* beacon = (struct LQIBeaconMsg*) mMsg.data;

        beacon->src = TOS_LOCAL_ADDRESS;
        beacon->seqno = 0;

        mBeaconPos = -1;
        mBeaconInterval = 0;

        call NeighborTable.init();
        mInited = TRUE;
        return SUCCESS;
    }

    command result_t
    StdControl.start()
    {
        return SUCCESS;
    }

    command result_t
    StdControl.stop()
    {
        return SUCCESS;
    }

    /**********************************************************************
     *
     * BeaconSendMsg interface
     *
     **********************************************************************/

    event result_t
    BeaconSendMsg.sendDone(TOS_MsgPtr msg, result_t success)
    {
        struct LQIBeaconMsg* beacon = (struct LQIBeaconMsg*) msg->data;

        if (success == SUCCESS) {
            ++beacon->seqno;
            mBeaconPos = mNextBeaconPos;
        }

        mSending = FALSE;
        return SUCCESS;
    }

    /**********************************************************************
     *
     * BeaconReceiveMsg interface
     *
     **********************************************************************/

    event TOS_MsgPtr
    BeaconReceiveMsg.receive(TOS_MsgPtr m)
    {
        if (mInited)
            receiveBeacon(m);
        return m;
    }

    /**********************************************************************
     *
     * LinkEstimator interface
     *
     **********************************************************************/

    command uint32_t
    LinkEstimator.getBeaconInterval()
    {
        return mBeaconInterval;
    }

    command void
    LinkEstimator.setBeaconInterval(uint32_t interval)
    {
        mBeaconInterval = interval;

        call Timer.stop();
        if (mBeaconInterval != 0)
            call Timer.start(TIMER_ONE_SHOT,
                             mBeaconInterval + call Random.rand() % 1024);
    }

    command void
    LinkEstimator.updateLink(uint16_t src, TOS_MsgPtr m)
    {
        metric_t metric;

#if defined(TOSMSG_HAS_LQI)
        metric = adjustLQI(m->lqi);
#else /* !defined(TOSMSG_HAS_LQI) */
        metric = DEFAULT_METRIC;
#endif /* !defined(TOSMSG_HAS_LQI) */

        call NeighborTable.updateReverse(src, metric);
    }

    /**********************************************************************
     *
     * Timer interface
     *
     **********************************************************************/

    event result_t
    Timer.fired()
    {
        sendBeacon();
        call Timer.start(TIMER_ONE_SHOT, mBeaconInterval);
        return SUCCESS;
    }
}
