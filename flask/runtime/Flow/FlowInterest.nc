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

#include "Flow.h"

interface FlowInterest
{
    command void init();

    /**
     * Purge all flow interest entries with timestamp < t.
     *
     * @param t the oldest interest timestamp we want to keep around
     */
    command void purge(nettime_t t);

    event void purged(flowid_t flow_id, uint16_t node_id);

    /**
     * Determine if there is any interest in the specified flow.
     *
     * @param id the flow id
     *
     * @return true if any node is interested in flow id, false otherwise.
     */
    command bool isInterested(flowid_t id);

    /**
     * Get the interest struct for the specified flow by the specified node.
     *
     * @param flow_id flow id
     * @param node_id node id
     *
     * @return pointer to the interest struct if there is any interest in flow
     * flow_id by node node_id. If there is no such interest, returns NULL.
     */
    command struct flowint_t* get(flowid_t flow_id, uint16_t node_id);

    /**
     * Get the first interest struct for the specified flow.
     *
     * @param flow_id flow id
     * @param idx pointer to an int, may not be NULL.
     *
     * @return pointer to the interest struct for the first node interested in
     * flow flow_id.
     */
    command struct flowint_t* first(flowid_t flow_id, int* idx);

    /**
     * Get the next interest struct for the specified flow.
     *
     * @param flow_id flow id
     * @param idx pointer to an int, may not be NULL.
     *
     * @return pointer to the interest struct for the first node interested in
     * flow flow_id.
     */
    command struct flowint_t* next(flowid_t flow_id, int* idx);

    command struct flowint_t* firstActiveLocal(int* idx, flowid_t* flow_id);
    command struct flowint_t* nextActiveLocal(int* idx, flowid_t* flow_id);

    command bool add(flowid_t flow_id, uint16_t node_id,
                     struct flowint_t* interest);
    command bool remove(flowid_t flow_id, uint16_t node_id);
    command void refresh(struct flowint_t* interest);
}
