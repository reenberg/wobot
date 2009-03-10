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
#include "NetTime.h"

generic module NeighborTableM(int N) {
    provides interface NeighborTable;

    uses interface NetTime;
} implementation {
    struct entry_t {
        uint16_t  node_id;
        metric_t  forward;
        metric_t  reverse;
        nettime_t timestamp;
    };

    #define OLD 120

    /*
     * m_metrics holds all link metrics. An entry is free when its node_id is
     * UNUSED.
     */
    #define NONE_IDX -1
    #define UNUSED TOS_BCAST_ADDR
    struct entry_t m_metrics[N];

    #define ALPHA           77UL  /* 0.3*256 */
    #define ONE_MINUS_ALPHA 179UL /* (1 - 0.3)*256 */

    metric_t
    update(metric_t old, metric_t v)
    {
        if (old == WORST_METRIC)
            return v;
        else
            return (ALPHA*((uint32_t) v)+ONE_MINUS_ALPHA*((uint32_t) old))/256UL;
    }

    int
    findFree()
    {
        int       i;
        int       old_entry = -1;
        int       bad_metric_entry = -1;
        nettime_t now = call NetTime.get();

        /* If the metric table is full we must eject one entry. Our policy is to
         * eject the oldest entry unless all entries have been seen within the
         * last OLD nettime units, in which case we eject the "worst" entry.
         */
        for (i = 0; i < N; ++i) {
            if (m_metrics[i].timestamp < now - OLD) {
                if (old_entry == -1)
                    old_entry = i;
                else if (NETTIME_LT(m_metrics[i].timestamp, m_metrics[old_entry].timestamp))
                    old_entry = i;
            } else {
                if (bad_metric_entry == -1)
                    bad_metric_entry = i;
                else if (is_better(m_metrics[bad_metric_entry].forward,
                                   m_metrics[i].forward))
                    bad_metric_entry = i;
            }
        }

        if (old_entry != -1)
            return old_entry;
        else
            return bad_metric_entry;
    }

    command void
    NeighborTable.init()
    {
        int i;

        for (i = 0; i < N; ++i) {
            m_metrics[i].node_id = UNUSED;
            m_metrics[i].forward = WORST_METRIC;
            m_metrics[i].reverse = WORST_METRIC;
            m_metrics[i].timestamp = 0;
        }
    }

    command metric_t
    NeighborTable.reverse(uint16_t node_id)
    {
        int i;

        fassert(node_id != UNUSED);

        for (i = 0; i < N; ++i) {
            if (m_metrics[i].node_id == node_id)
                return m_metrics[i].reverse;
        }

        return WORST_METRIC;
    }

    command metric_t
    NeighborTable.forward(uint16_t node_id)
    {
        int i;

        fassert(node_id != UNUSED);

        for (i = 0; i < N; ++i) {
            if (m_metrics[i].node_id == node_id)
                return m_metrics[i].forward;
        }

        return WORST_METRIC;
    }

    command void
    NeighborTable.updateReverse(uint16_t node_id, metric_t metric)
    {
        int i;

        fassert(node_id != UNUSED);

        /* We should *never* see link estimate packets over the UART. And node
         * 126 is evil! */
        if (node_id == TOS_LOCAL_ADDRESS || node_id == TOS_UART_ADDR)
            return;

        for (i = 0; i < N; ++i) {
            if (m_metrics[i].node_id == node_id) {
                m_metrics[i].reverse = update(m_metrics[i].reverse, metric);
                m_metrics[i].timestamp = call NetTime.get();
                return;
            }
        }

        if ((i = findFree()) != -1) {
            m_metrics[i].node_id = node_id;
            m_metrics[i].reverse = metric;
            m_metrics[i].forward = WORST_METRIC;
            m_metrics[i].timestamp = call NetTime.get();
        }
    }

    command void
    NeighborTable.updateForward(uint16_t node_id, metric_t metric)
    {
        int i;

        fassert(node_id != UNUSED);

        /* We should *never* see link estimate packets over the UART. And node
         * 126 is evil! */
        if (node_id == TOS_LOCAL_ADDRESS || node_id == TOS_UART_ADDR)
            return;

        for (i = 0; i < N; ++i) {
            if (m_metrics[i].node_id == node_id) {
                m_metrics[i].forward = update(m_metrics[i].forward, metric);
                m_metrics[i].timestamp = call NetTime.get();
                return;
            }
        }

        if ((i = findFree()) != -1) {
            m_metrics[i].node_id = node_id;
            m_metrics[i].forward = metric;
            m_metrics[i].reverse = WORST_METRIC;
            m_metrics[i].timestamp = call NetTime.get();
        }
    }

    command int
    NeighborTable.firstForward(uint16_t* node_id, metric_t* metric)
    {
        int i;

        for (i = 0; i < N; ++i) {
            if (m_metrics[i].node_id != UNUSED) {
                *node_id = m_metrics[i].node_id;
                *metric = m_metrics[i].forward;

                return i;
            }
        }

        return -1;
    }

    command int
    NeighborTable.nextForward(int idx, uint16_t* node_id, metric_t* metric)
    {
        int i;

        for (i = idx + 1; i < N; ++i) {
            if (m_metrics[i].node_id != UNUSED) {
                *node_id = m_metrics[i].node_id;
                *metric = m_metrics[i].forward;

                return i;
            }
        }

        return -1;
    }

    command int
    NeighborTable.firstReverse(uint16_t* node_id, metric_t* metric)
    {
        int i;

        for (i = 0; i < N; ++i) {
            if (m_metrics[i].node_id != UNUSED) {
                *node_id = m_metrics[i].node_id;
                *metric = m_metrics[i].reverse;

                return i;
            }
        }

        return -1;
    }

    command int
    NeighborTable.nextReverse(int idx, uint16_t* node_id, metric_t* metric)
    {
        int i;

        for (i = idx + 1; i < N; ++i) {
            if (m_metrics[i].node_id != UNUSED) {
                *node_id = m_metrics[i].node_id;
                *metric = m_metrics[i].reverse;

                return i;
            }
        }

        return -1;
    }
}
