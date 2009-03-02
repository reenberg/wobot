/*
 * Copyright (c) 2006
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

#include "Query.h"

#define FLOW_ID 1

#define INIT_DEADLINE 1

#if defined(PLATFORM_PC)
#define SUBSCRIBE_DEADLINE 30
#define BACKOFF_DEADLINE 30
#else
#if defined(STATIC_ROUTES)
#define SUBSCRIBE_DEADLINE 30
#define BACKOFF_DEADLINE 30
#else /* !defined(STATIC_ROUTES) */
#define SUBSCRIBE_DEADLINE 150
#define BACKOFF_DEADLINE 300
#endif /* !defined(STATIC_ROUTES) */
#endif

module QueryInitM {
    provides interface StdControl;
    uses interface StdControl as QueryStdControl;
#if defined(PLATFORM_TELOSB)
    uses interface SplitControl as HumiditySplitControl;
#endif /* defined(PLATFORM_telosb) */
    uses interface Timer;
    uses interface LinkEstimator;
    uses interface Flow;
    uses interface FlowControl;
#if !defined(LEAN)
    uses interface BufferedSend as LocalSend;
    uses interface BufferedSend as ResultSend;
#endif /* !defined(LEAN) */
    provides event void eval_local(double arg1, double arg2, double arg3);
    provides event void eval(double arg1, double arg2, double arg3);
} implementation {
    int m_secs = 0;

    command result_t StdControl.init()
    {
        call QueryStdControl.init();
        return SUCCESS;
    }

    command result_t StdControl.start()
    {
#if defined(PLATFORM_TELOSB)
        atomic call HumiditySplitControl.init();
#else /* !defined(PLATFORM_telosb) */
        call Timer.start(TIMER_REPEAT, 1024);
#endif /* !defined(PLATFORM_telosb) */
        return SUCCESS;
    }

    command result_t StdControl.stop()
    {
        return call Timer.stop();
    }

#if defined(PLATFORM_TELOSB)
  /* MDW: I found that one needs to initialize the humidity sensor before
   * starting up the application timer - otherwise the app timer stops for some
   * reason.
   */
    event result_t HumiditySplitControl.initDone() {
        call HumiditySplitControl.start();
        return SUCCESS;
    }

    event result_t HumiditySplitControl.startDone() {
        call Timer.start(TIMER_REPEAT, 1024);
        return SUCCESS;
    }

    event result_t HumiditySplitControl.stopDone() {
        return SUCCESS;
    }
#endif /* defined(PLATFORM_telosb) */

    event result_t Timer.fired()
    {
        m_secs++;

        if (m_secs == INIT_DEADLINE) {
            call LinkEstimator.setBeaconInterval(1*1024);
            call FlowControl.setRefreshTime(15);
            //call FlowControl.setPurgeMultiple(0);
            call FlowControl.setMaxAckRetrans(10);
        }

        if (m_secs == BACKOFF_DEADLINE) {
#if defined(STATIC_ROUTES)
            call LinkEstimator.setBeaconInterval(0);
            call FlowControl.setRefreshTime(0);
#endif
        }

        if (   m_secs == SUBSCRIBE_DEADLINE
            && TOS_LOCAL_ADDRESS == BASE_NODE_ID) {
            call Flow.unsubscribe(FLOW_ID);

            call FlowControl.setPubsubRepeat(5);
            call Flow.subscribe(FLOW_ID, TRUE);
            call FlowControl.setPubsubRepeat(1);
        }

        return SUCCESS;
    }

    event void Flow.receive(flowid_t flow_id, void *data, size_t size)
    {
    }

    struct local_t {
        int flag1;
        uint16_t item1;
        int flag2;
        uint16_t item2;
    };

    event void eval_local(double arg1, double arg2, double arg3)
    {
#if !defined(LEAN)
        struct LocalMsg msg;
        const struct flowstats_t* stats =
            call FlowControl.stats();

        dbg(DBG_USR1, "local: count %f temp %f\n",
            (double) arg1, (double) (arg3/arg2));

        msg.id = arg1;
        msg.temp = arg3/arg2;
        msg.radio_send_unicast_cnt = stats->radio_send_unicast_cnt;
        msg.radio_send_bcast_cnt = stats->radio_send_bcast_cnt;
        msg.radio_recv_unicast_cnt = stats->radio_recv_unicast_cnt;
        msg.radio_recv_bcast_cnt = stats->radio_recv_bcast_cnt;

        call LocalSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
#endif /* !defined(LEAN) */
    }

    event void eval(double arg1, double arg2, double arg3)
    {
#if !defined(LEAN)
        if (TOS_LOCAL_ADDRESS == BASE_NODE_ID) {
            struct ResultMsg msg;

            dbg(DBG_USR1, "result: count %f avg temp %f\n",
                arg1, arg3/arg2);

            msg.count = arg1;
            msg.avg_temp = arg3/arg2;

            call ResultSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
        }
#endif /* !defined(LEAN) */
    }
}
