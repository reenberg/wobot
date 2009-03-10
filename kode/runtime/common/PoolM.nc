/*
 * Copyright (c) 2005
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

generic module PoolM(typedef t, int N) {
    provides interface Pool<t> as Pool;
} implementation {
    struct elem_t {
        t x;
        struct elem_t* next;
    };

    struct elem_t* m_head = NULL;
    struct elem_t  m_mem[N];

    bool isMember(t* x)
    {
        return x >= (t*) &(m_mem[0]) && x <= (t*) &(m_mem[N - 1]);
    }

    command void Pool.init() {
        int i;

        m_head = NULL;

        for (i = 0; i < N; ++i) {
            m_mem[i].next = m_head;
            m_head = &(m_mem[i]);
        }
    }

    command bool Pool.isMember(t* x)
    {
        return isMember(x);
    }

    command t* Pool.alloc() {
        if (m_head == NULL)
            return NULL;
        else {
            struct elem_t* temp = m_head;

            m_head = temp->next;
            temp->next = NULL;

            return &(temp->x);
        }
    }

    command void Pool.free(t* x) {
        struct elem_t* temp = (struct elem_t*) x;

        fassert(isMember(x));

        if (((struct elem_t*) x)->next == NULL) {
            temp->next = m_head;
            m_head = temp;
        }
    }
}
