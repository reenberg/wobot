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

module BufferedSendM {
    provides interface StdControl;
    provides interface BufferedSend[uint8_t id];

    uses interface SendMsg as BaseSendMsg[uint8_t id];

    uses interface Pool<struct TOS_Msg> as Pool;
} implementation {
    command result_t
    BufferedSend.send[uint8_t id](uint16_t address, void* data, uint8_t length)
    {
        TOS_MsgPtr msg;

        if (length > TOSH_DATA_LENGTH)
            return FAIL;

        msg = call Pool.alloc();

        if (msg == NULL)
            return FAIL;

        memcpy(msg->data, data, length);

        if (call BaseSendMsg.send[id](address, length, msg) == SUCCESS)
            return SUCCESS;
        else {
            call Pool.free(msg);
            return FAIL;
        }
    }

    event result_t
    BaseSendMsg.sendDone[uint8_t id](TOS_MsgPtr msg, result_t success)
    {
        /*
         * We will get sendDone upcalls for *all* sent messages, even those we
         * didn't send. Therefore we make sure we allocated msg before freeing
         * it and processing the send queue.
         */
        if (call Pool.isMember(msg))
            call Pool.free(msg);

        return SUCCESS;
    }

    command result_t
    StdControl.init()
    {
        call Pool.init();

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
}
