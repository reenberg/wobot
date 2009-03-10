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

#include "fassert.h"

generic module QueueM(typedef T, uint8_t N) {
    provides interface Queue<T>;
} implementation {
    T m_q[N];
    uint8_t m_count = 0;
    uint8_t m_head = 0;
    uint8_t m_tail = 0;

    command void Queue.init()
    {
        m_count = 0;
        m_head = 0;
        m_tail = 0;
    }

    command T Queue.head()
    {
        fassert(m_count <= N);

        return m_q[m_head];
    }

    command result_t Queue.push(T x)
    {
        fassert(m_count <= N);

        if (m_count == N)
            return FAIL;

        m_q[m_tail++] = x;
        m_tail %= N;
        ++m_count;

        return SUCCESS;
    }

    command T Queue.pop()
    {
        T ret = m_q[m_head];

        fassert(m_count <= N);

        if (m_count > 0) {
            m_head++;
            m_head %= N;
            --m_count;
        }

        return ret;
    }

    command result_t Queue.prepend(T x)
    {
        fassert(m_count <= N);

        if (m_count == N)
            return FAIL;

        if (m_head == 0)
            m_head = N - 1;
        else
            --m_head;

        m_q[m_head] = x;
        ++m_count;

        return SUCCESS;
    }

    command uint8_t Queue.size()
    {
        return m_count;
    }

    command uint8_t Queue.maxSize()
    {
        return N;
    }

    command bool Queue.isEmpty()
    {
        return m_count == 0;
    }

    command bool Queue.isFull()
    {
        return m_count == N;
    }
}
