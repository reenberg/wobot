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

#define SEQ_LT(a,b)     (((long) a)-((long) b) < 0)
#define SEQ_LEQ(a,b)    (((long) a)-((long) b) <= 0)
#define SEQ_GT(a,b)     (((long) a)-((long) b) > 0)
#define SEQ_GEQ(a,b)    (((long) a)-((long) b) >= 0)

#define WINDOW_SIZE 16

typedef uint16_t window_t;

/*
 * m_seqnos holds all sequence numbers. An entry is free when its node_id is
 * UNUSED.
 */
#define UNUSED TOS_BCAST_ADDR

generic module FlowSeqnoM(int N) {
    provides interface FlowSeqno;

    uses interface NetTime;
    uses interface Random;
} implementation {
    struct entry_t {
        flowid_t    flow_id;
        uint16_t    node_id;
        uint16_t    bcast;
        flowseqno_t max_seqno;
        window_t    window;
        nettime_t   timestamp;
    };

    struct entry_t m_seqnos[N];

    int
    findFree(bool local)
    {
        int       i;
        int       free_entry = -1;
        nettime_t now = call NetTime.get();

        for (i = 0; i < N; ++i) {
            /* Prefer the first unused entry */
            if (m_seqnos[i].node_id == UNUSED)
                return i;

            /* If there is no free entry, keep track of which entry is the
             * oldest so we can re-use it. We never eject recent broadcasts.
             */
            if (m_seqnos[i].node_id != TOS_LOCAL_ADDRESS
                && (   local
                    || !m_seqnos[i].bcast
                    || NETTIME_LT(m_seqnos[i].timestamp,
                                  now - FLOW_BCAST_RECENT_TIME*NETTIME_RATE))) {
                if (   free_entry == -1
                    || NETTIME_LT(m_seqnos[i].timestamp,
                                  m_seqnos[free_entry].timestamp))
                    free_entry = i;
            }
        }

        return free_entry;
    }

    command void
    FlowSeqno.init()
    {
        int i;

        call Random.init();

        for (i = 0; i < N; ++i)
            m_seqnos[i].node_id = UNUSED;
    }

    command bool
    FlowSeqno.sawSeqno(flowid_t flow_id, uint32_t node_id,
                       bool bcast, flowseqno_t seqno)
    {
        int       i;
        nettime_t now = call NetTime.get();

        fassert(node_id != UNUSED);

        for (i = 0; i < N; ++i) {
            if (   m_seqnos[i].flow_id == flow_id
                && m_seqnos[i].node_id == node_id) {
                uint16_t max = m_seqnos[i].max_seqno;
                window_t window = m_seqnos[i].window;

                if (   SEQ_LEQ(seqno, max - FLOW_SEQ_WINDOW)
                    || SEQ_GEQ(seqno, max + FLOW_SEQ_WINDOW)) {
                    /* The received sequence number is outside of our
                     * window...watch out!
                     */
                    if (   bcast
                        && NETTIME_LT(now,
                                      m_seqnos[i].timestamp + FLOW_SEQ_SQUELCH_TIME*NETTIME_RATE))
                            return TRUE;

                    m_seqnos[i].bcast = bcast;
                    m_seqnos[i].max_seqno = seqno;
                    m_seqnos[i].window = 1;
                    m_seqnos[i].timestamp = now;
                    return FALSE;
                } else {
                    m_seqnos[i].bcast = bcast;
                    m_seqnos[i].timestamp = now;

                    if (SEQ_LEQ(seqno, max - WINDOW_SIZE))
                        return TRUE;
                    else if (SEQ_LEQ(seqno, max)) {
                        if ((window & (1 << (max - seqno))) != 0)
                            return TRUE;
                        else {
                            m_seqnos[i].window |= (1 << (max - seqno));
                            return FALSE;
                        }
                    } else /* SEQ_GT(seqno, max) */ {
                        m_seqnos[i].max_seqno = seqno;
                        m_seqnos[i].window <<= seqno - max;
                        m_seqnos[i].window |= 1;
                        return FALSE;
                    }
                }
            }
        }

        /*
         * If we don't have a sequence number for the specified flow/node, add
         * an entry for it. If we can't find a free entry, be safe and say we've
         * already seen this sequence number if it's a broadcast. This is needed
         * to prevent broadcast storms.
         */
        if ((i = findFree(FALSE)) != -1) {
            m_seqnos[i].flow_id = flow_id;
            m_seqnos[i].node_id = node_id;
            m_seqnos[i].bcast = bcast;
            m_seqnos[i].max_seqno = seqno;
            m_seqnos[i].window = 1;
            m_seqnos[i].timestamp = now;

            return FALSE;
        } else {
            if (bcast)
                return TRUE;
            else
                return FALSE;
        }
    }

    command flowseqno_t
    FlowSeqno.nextSeqno(flowid_t flow_id)
    {
        int         i;
        flowseqno_t seqno = 0xDEAD;
        nettime_t   now = call NetTime.get();

        for (i = 0; i < N; ++i) {
            if (   m_seqnos[i].flow_id == flow_id
                && m_seqnos[i].node_id == TOS_LOCAL_ADDRESS) {
                ++m_seqnos[i].max_seqno;
                m_seqnos[i].window = (m_seqnos[i].window << 1) | 1;
                m_seqnos[i].timestamp = now;

                return m_seqnos[i].max_seqno;
            }
        }

        if ((i = findFree(TRUE)) != -1) {
            seqno = call Random.rand();

            m_seqnos[i].flow_id = flow_id;
            m_seqnos[i].node_id = TOS_LOCAL_ADDRESS;
            m_seqnos[i].bcast = TRUE;
            m_seqnos[i].max_seqno = seqno;
            m_seqnos[i].window = 1;
            m_seqnos[i].timestamp = now;
        }

        return seqno;
    }
}
