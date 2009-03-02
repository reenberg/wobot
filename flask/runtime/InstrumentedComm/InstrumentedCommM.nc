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

#include "InstrumentedComm.h"

module InstrumentedCommM {
    provides interface StdControl;
    provides interface SendMsg[uint8_t id];
    provides interface ReceiveMsg[uint8_t id];
    provides interface InstrumentedComm;

    uses interface SendMsg as OrigSendMsg[uint8_t id];
    uses interface ReceiveMsg as OrigReceiveMsg[uint8_t id];

    uses interface BufferedSend;
    uses interface ReceiveMsg as InstrumentedCommReceiveMsg;

    uses interface Leds;
} implementation {
    bool     mEnableComm = TRUE;
    uint16_t mUnicastpacketsSent = 0;
    uint16_t mBcastpacketsSent = 0;
    uint16_t mUnicastPacketsReceived = 0;
    uint16_t mBcastPacketsReceived = 0;

    command result_t
    StdControl.init()
    {
        mEnableComm = TRUE;
        mUnicastpacketsSent = 0;
        mBcastpacketsSent = 0;
        mUnicastPacketsReceived = 0;
        mBcastPacketsReceived = 0;
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

    command result_t
    SendMsg.send[uint8_t id](uint16_t address, uint8_t length, TOS_MsgPtr msg)
    {
        if (address == TOS_BCAST_ADDR)
            ++mBcastpacketsSent;
        else if (address != TOS_UART_ADDR)
            ++mUnicastpacketsSent;

        if (mEnableComm || address == TOS_UART_ADDR)
            return call OrigSendMsg.send[id](address, length, msg);
        else
            return FAIL;
    }

    event result_t
    OrigSendMsg.sendDone[uint8_t id](TOS_MsgPtr msg, result_t success)
    {
        return signal SendMsg.sendDone[id](msg, success);
    }

    bool
    isFromUART(TOS_MsgPtr m)
    {
        /* We assume the packet came over the UART if these fields are all
         * zeroed out. This is true for packets sent using the Python library,
         * but may not be true in other cases.
         */
#if defined(PLATFORM_TELOS) || defined(PLATFORM_TELOSB)
        return m->fcfhi == 0 && m->fcflo == 0 && m->dsn == 0;
#else
        return FALSE;
#endif
    }

    event TOS_MsgPtr
    OrigReceiveMsg.receive[uint8_t id](TOS_MsgPtr m)
    {
        if (m->addr == TOS_BCAST_ADDR)
            ++mBcastPacketsReceived;
        else if (!isFromUART(m))
            ++mUnicastPacketsReceived;

#if defined(PLATFORM_TELOS) || defined(PLATFORM_TELOSB)
        if (mEnableComm || isFromUART(m))
            return signal ReceiveMsg.receive[id](m);
        else
            return m;
#else
        return signal ReceiveMsg.receive[id](m);
#endif
    }

    default event TOS_MsgPtr
    ReceiveMsg.receive[uint8_t id](TOS_MsgPtr m)
    {
        return m;
    }

    command void
    InstrumentedComm.resetCounts()
    {
        mUnicastpacketsSent = 0;
        mBcastpacketsSent = 0;
        mUnicastPacketsReceived = 0;
        mBcastPacketsReceived = 0;
    }

    command uint16_t
    InstrumentedComm.unicastPacketsSent()
    {
        return mUnicastpacketsSent;
    }

    command uint16_t
    InstrumentedComm.bcastPacketsSent()
    {
        return mBcastpacketsSent;
    }

    command uint16_t
    InstrumentedComm.unicastPacketsReceived()
    {
        return mUnicastPacketsReceived;
    }

    command uint16_t
    InstrumentedComm.bcastPacketsReceived()
    {
        return mBcastPacketsReceived;
    }

    command void
    InstrumentedComm.disableComm()
    {
        mEnableComm = FALSE;
    }

    command void
    InstrumentedComm.enableComm()
    {
        mEnableComm = TRUE;
    }

    void sendStats()
    {
        struct InstrumentedCommMsg msg;

        msg.type = INSTRUMENTEDCOMMMSG_STATS;

        msg.u.stats.unicastPacketsSent =
            call InstrumentedComm.unicastPacketsSent();
        msg.u.stats.bcastPacketsSent =
            call InstrumentedComm.bcastPacketsSent();
        msg.u.stats.unicastPacketsReceived =
            call InstrumentedComm.unicastPacketsReceived();
        msg.u.stats.bcastPacketsReceived =
            call InstrumentedComm.bcastPacketsReceived();

        call BufferedSend.send(TOS_UART_ADDR, &msg, sizeof(msg));
    }

    event TOS_MsgPtr
    InstrumentedCommReceiveMsg.receive(TOS_MsgPtr m)
    {
        struct InstrumentedCommMsg* in = (struct InstrumentedCommMsg*) m->data;

        if (m->length != sizeof(*in))
            return m;

        switch (in->type) {
            case INSTRUMENTEDCOMMMSG_RESETSTATS:
                call InstrumentedComm.resetCounts();
                break;

            case INSTRUMENTEDCOMMMSG_GET_STATS:
                sendStats();
                break;

            case INSTRUMENTEDCOMMMSG_DISABLE_COMM:
                call InstrumentedComm.disableComm();
                break;

            case INSTRUMENTEDCOMMMSG_ENABLE_COMM:
                call InstrumentedComm.enableComm();
                break;
        }

        return m;
    }
}
