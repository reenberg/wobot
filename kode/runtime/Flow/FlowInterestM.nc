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
#include "metric.h"
#include "Flow.h"
#include "NetTime.h"

generic module FlowInterestM(int N) {
    provides interface FlowInterest;

    uses interface NetTime;
} implementation {
    /*
     * m_interest holds all flow interests. An entry is free when its node_id
     * is TOS_BCAST_ADDR.
     */
    #define NONE_IDX -1
    #define UNUSED TOS_BCAST_ADDR
    struct flowint_t m_interest[N];

    command void
    FlowInterest.init()
    {
        int i;

        for (i = 0; i < N; ++i)
            m_interest[i].node_id = UNUSED;
    }

    command void
    FlowInterest.purge(nettime_t t)
    {
        int i;

        for (i = 0; i < N; ++i) {
            if (   m_interest[i].node_id != UNUSED
                && m_interest[i].node_id != TOS_LOCAL_ADDRESS
                && NETTIME_LT(m_interest[i].timestamp, t)) {
                signal FlowInterest.purged(m_interest[i].flow_id,
                                           m_interest[i].node_id);
                m_interest[i].node_id = UNUSED;
            }
        }
    }

    default event void
    FlowInterest.purged(flowid_t flow_id, uint16_t node_id)
    {
    }

    command bool
    FlowInterest.isInterested(flowid_t id)
    {
        int i;

        for (i = 0; i < N; ++i) {
            if (   m_interest[i].flow_id == id
                && m_interest[i].node_id != UNUSED)
                return TRUE;
        }

        return FALSE;
    }

    command struct flowint_t*
    FlowInterest.get(flowid_t flow_id, uint16_t node_id)
    {
        int i;

        fassert(node_id != UNUSED);

        for (i = 0; i < N; ++i) {
            if (   m_interest[i].flow_id == flow_id
                && m_interest[i].node_id == node_id)
                return &(m_interest[i]);
        }

        return NULL;
    }

    command struct flowint_t*
    FlowInterest.first(flowid_t flow_id, int* idx)
    {
        int i;

        fassert(idx != NULL);

        for (i = 0; i < N; ++i) {
            if (   m_interest[i].flow_id == flow_id
                && m_interest[i].node_id != UNUSED
                && m_interest[i].node_id != TOS_LOCAL_ADDRESS) {
                *idx = i;
                return &(m_interest[i]);
            }
        }

        return NULL;
    }

    command struct flowint_t*
    FlowInterest.next(flowid_t flow_id, int* idx)
    {
        int i;

        fassert(idx != NULL);
        fassert(*idx >= 0);
        fassert(*idx < N);

        for (i = *idx + 1; i < N; ++i) {
            if (   m_interest[i].flow_id == flow_id
                && m_interest[i].node_id != UNUSED
                && m_interest[i].node_id != TOS_LOCAL_ADDRESS) {
                *idx = i;
                return &(m_interest[i]);
            }
        }

        *idx = NONE_IDX;
        return NULL;
    }

    command struct flowint_t*
    FlowInterest.firstActiveLocal(int* idx, flowid_t* flow_id)
    {
        int i;

        fassert(idx != NULL);

        for (i = 0; i < N; ++i) {
            if (   m_interest[i].node_id == TOS_LOCAL_ADDRESS
                && (m_interest[i].flags & FLOWINT_ACTIVE) != 0) {
                *idx = i;
                *flow_id = m_interest[i].flow_id;
                return &(m_interest[i]);
            }
        }

        return NULL;
    }

    command struct flowint_t*
    FlowInterest.nextActiveLocal(int* idx, flowid_t* flow_id)
    {
        int i;

        fassert(idx != NULL);
        fassert(*idx >= 0);
        fassert(*idx < N);

        for (i = *idx + 1; i < N; ++i) {
            if (   m_interest[i].node_id == TOS_LOCAL_ADDRESS
                && (m_interest[i].flags & FLOWINT_ACTIVE) != 0) {
                *idx = i;
                *flow_id = m_interest[i].flow_id;
                return &(m_interest[i]);
            }
        }

        *idx = NONE_IDX;
        return NULL;
    }

    command bool
    FlowInterest.add(flowid_t flow_id, uint16_t node_id,
                     struct flowint_t* interest)
    {
        int i;
        int j = NONE_IDX;

        fassert(node_id != UNUSED);

        /* If we already have an interest in this node/flow combination, update
         * it. Otherwise if we've found an unused entry, remember it.
         */
        for (i = 0; i < N; ++i) {
            if (   m_interest[i].flow_id == flow_id
                && m_interest[i].node_id == node_id) {
                m_interest[i] = *interest;
                m_interest[i].timestamp = call NetTime.get();
                return FALSE;
            } else if (m_interest[i].node_id == UNUSED) {
                j = i;
                break;
            }
        }

        /* If the interest table is full we must eject one entry. We iterate
         * through all entries. If an entry in the table has the same flow id as
         * the one we're adding, we prefer the one we're adding only if its
         * metric is better. Otherwise we eject the oldest entry.
         *
         */
        if (j == NONE_IDX) {
            for (i = 0; i < N; ++i) {
                if (flow_id == m_interest[i].flow_id) {
                    if (j == NONE_IDX) {
                        if (is_better(interest->metric,
                                      m_interest[i].metric))
                            j = i;
                    } else if (is_better(m_interest[j].metric,
                                         m_interest[i].metric))
                            j = i;
                } else if (j == NONE_IDX)
                    j = i;
                else if (NETTIME_LT(m_interest[i].timestamp,
                                    m_interest[j].timestamp))
                    j = i;
            }

            if (j != NONE_IDX)
                signal FlowInterest.purged(m_interest[j].flow_id,
                                           m_interest[j].node_id);
        }

        if (j != NONE_IDX) {
            m_interest[j] = *interest;
            m_interest[j].timestamp = call NetTime.get();

            return TRUE;
        } else
            return FALSE;
    }

    command bool
    FlowInterest.remove(flowid_t flow_id, uint16_t node_id)
    {
        int i;

        for (i = 0; i < N; ++i) {
            if (   m_interest[i].flow_id == flow_id
                && m_interest[i].node_id == node_id) {
                m_interest[i].node_id = UNUSED;
                return TRUE;
            }
        }

        return FALSE;
    }

    command void
    FlowInterest.refresh(struct flowint_t* interest)
    {
        fassert(interest->next_hop != UNUSED);

        interest->timestamp = call NetTime.get();
    }
}
