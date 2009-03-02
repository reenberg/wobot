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
#include "QueuedSend.h"

module QueuedSendM {
    provides interface StdControl;
    provides interface SendMsg[uint8_t id];
    provides interface SendMsg as UrgentSendMsg[uint8_t id];

    uses interface Timer;
    uses interface Random;
    uses interface SendMsg as BaseSendMsg[uint8_t id];
    uses interface Pool<struct qentry_t> as Pool;
    uses interface Queue<struct qentry_t*> as SendQueue;
} implementation {
    bool mSending = FALSE;
    bool mSendPending = FALSE;

    task void processSendQueue();
    result_t scheduleSendQueue();
    result_t enqueueSendImmed(struct qentry_t* act);
    result_t enqueueSend(struct qentry_t* act);

    task void
    processSendQueue()
    {
        struct qentry_t* entry;

        fassert(!call SendQueue.isEmpty());

        mSendPending = FALSE;

        entry = call SendQueue.head();

#if defined(TOSMSG_HAS_ACK)
        entry->msg->ack = 0;
#endif /* defined(TOSMSG_HAS_ACK) */

        if (call BaseSendMsg.send[entry->id](entry->address,
                                             entry->length,
                                             entry->msg) == SUCCESS) {
            mSending = TRUE;
            call SendQueue.pop();
            call Pool.free(entry);
        } else
            post processSendQueue();
    }

    result_t
    scheduleSendQueue()
    {
        if (!mSendPending && !mSending && !call SendQueue.isEmpty()) {
            if (post processSendQueue()) {
                mSendPending = TRUE;
                return SUCCESS;
            } else
                return FAIL;
        } else
            return SUCCESS;
    }

    result_t
    enqueueSendImmed(struct qentry_t* act)
    {
        if (call SendQueue.prepend(act) == SUCCESS)
            return scheduleSendQueue();
        else {
            call Pool.free(act);
            return FAIL;
        }
    }

    result_t
    enqueueSend(struct qentry_t* act)
    {
        if (call SendQueue.push(act) == SUCCESS)
            return scheduleSendQueue();
        else {
            call Pool.free(act);
            return FAIL;
        }
    }

    command result_t
    SendMsg.send[uint8_t id](uint16_t address, uint8_t length,
                             TOS_MsgPtr msg)
    {
        struct qentry_t* entry = call Pool.alloc();

        if (entry == NULL)
            return FAIL;

        entry->id = id;
        entry->address = address;
        entry->length = length;
        entry->msg = msg;

        return enqueueSend(entry);
    }

    command result_t
    UrgentSendMsg.send[uint8_t id](uint16_t address, uint8_t length,
                                   TOS_MsgPtr msg)
    {
        struct qentry_t* entry = call Pool.alloc();

        if (entry == NULL)
            return FAIL;

        entry->id = id;
        entry->address = address;
        entry->length = length;
        entry->msg = msg;

        return enqueueSendImmed(entry);
    }

    event result_t
    BaseSendMsg.sendDone[uint8_t id](TOS_MsgPtr msg, result_t success)
    {
        result_t result = signal SendMsg.sendDone[id](msg, success);

#if !defined(FLOWDEBUG)
        uint32_t delay = 0;
        uint32_t window = 0;

        if(   msg->addr == TOS_UART_ADDR
           || (   !call SendQueue.isEmpty()
               && ((call SendQueue.head())->msg->addr == TOS_UART_ADDR))) {
            delay = 0;
            window = 0;
        } else if (msg->ack == 0) {
            delay = ACK_FAILURE_DELAY_MIN;
            window = ACK_FAILURE_DELAY_WINDOW;
        } else {
            delay = SUCCESS_DELAY_MIN;
            window = SUCCESS_DELAY_WINDOW;
        }

        if (   delay != 0
            && call Timer.start(TIMER_ONE_SHOT,
                                delay + call Random.rand() % window) == SUCCESS)
                return result;
#endif /* !defined(FLOWDEBUG) */
        mSending = FALSE;
        scheduleSendQueue();

        return result;
    }

    default event result_t
    SendMsg.sendDone[uint8_t id](TOS_MsgPtr msg, result_t success)
    {
        return SUCCESS;
    }

    command result_t
    StdControl.init()
    {
        call Random.init();
        call Pool.init();
        call SendQueue.init();

        mSending = FALSE;
        mSendPending = FALSE;

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

    event result_t
    Timer.fired()
    {
        mSending = FALSE;
        scheduleSendQueue();
        return SUCCESS;
    }
}
