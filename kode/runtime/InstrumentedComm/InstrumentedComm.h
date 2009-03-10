/*
 * Copyright (c) 2007
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

#ifndef __INSTRUMENTEDCOMM_H__
#define __INSTRUMENTEDCOMM_H__

#if !defined(_AM_INSTRUMENTEDCOMMMSG)
#define _AM_INSTRUMENTEDCOMMMSG 18
#endif /* !defined(_AM_INSTRUMENTEDCOMMMSG) */

enum {
    AM_INSTRUMENTEDCOMMMSG = _AM_INSTRUMENTEDCOMMMSG
};

enum {
    INSTRUMENTEDCOMMMSG_RESETSTATS = 0,

    INSTRUMENTEDCOMMMSG_GET_STATS,
    INSTRUMENTEDCOMMMSG_STATS,

    INSTRUMENTEDCOMMMSG_DISABLE_COMM,
    INSTRUMENTEDCOMMMSG_ENABLE_COMM
};

struct InstrumentedCommStats {
    uint16_t unicastPacketsSent;
    uint16_t bcastPacketsSent;
    uint16_t unicastPacketsReceived;
    uint16_t bcastPacketsReceived;
};

struct InstrumentedCommMsg {
    uint16_t type;
    union {
        struct InstrumentedCommStats stats;
    } u;
};

#endif /* __INSTRUMENTEDCOMM_H__ */
