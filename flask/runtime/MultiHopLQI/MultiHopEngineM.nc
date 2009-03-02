// $Id: MultiHopEngineM.nc,v 1.3 2004/09/17 17:40:39 jpolastre Exp $

/*                                                                      tab:4
 * "Copyright (c) 2000-2003 The Regents of the University  of California.
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice, the following
 * two paragraphs and the author appear in all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS."
 *
 * Copyright (c) 2002-2003 Intel Corporation
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached INTEL-LICENSE
 * file. If you do not find these files, copies can be found by writing to
 * Intel Research Berkeley, 2150 Shattuck Avenue, Suite 1300, Berkeley, CA,
 * 94704.  Attention:  Intel License Inquiry.
 */

/*
 * A simple module that handles multihop packet movement.  It accepts
 * messages from both applications and the network and does the necessary
 * interception and forwarding.
 * It interfaces to an algorithmic componenet via RouteSelect. It also acts
 * as a front end for RouteControl
 */


/*
 * Authors:          Philip Buonadonna, Alec Woo, Crossbow Inc.
 *
 */

includes AM;
includes MultiHop;

module MultiHopEngineM {
  provides {
    interface StdControl;
    interface Receive[uint8_t id];
    interface Send[uint8_t id];
    interface Intercept[uint8_t id];
    interface Intercept as Snoop[uint8_t id];
    interface RouteControl;
    interface RouteStats;
  }
  uses {
    interface ReceiveMsg[uint8_t id];
    interface SendMsg[uint8_t id];
    interface RouteControl as RouteSelectCntl;
    interface RouteSelect;
    interface StdControl as SubControl;
    interface StdControl as CommStdControl;
    interface Leds;
  }
}

implementation {

  enum {
    FWD_QUEUE_SIZE = MHOP_QUEUE_SIZE, // Forwarding Queue
    EMPTY = 0xff
  };


  /* Routing status of local node */


  /* Internal storage and scheduling state */
  struct TOS_Msg FwdBuffers[FWD_QUEUE_SIZE];
  struct TOS_Msg *FwdBufList[FWD_QUEUE_SIZE];
  uint8_t FwdBufBusy[FWD_QUEUE_SIZE];

  uint8_t iFwdBufHead, iFwdBufTail;

  uint16_t sendFailures = 0;

  /***********************************************************************
   * Initialization
   ***********************************************************************/


  static void initialize() {
    int n;

    for (n=0; n < FWD_QUEUE_SIZE; n++) {
      FwdBufList[n] = &FwdBuffers[n];
      FwdBufBusy[n] = 0;
    }

    iFwdBufHead = iFwdBufTail = 0;

    sendFailures = 0;
  }

  command result_t StdControl.init() {
    initialize();
    call CommStdControl.init();
    return call SubControl.init();
  }

  command result_t StdControl.start() {
    call CommStdControl.start();
    return call SubControl.start();
  }

  command result_t StdControl.stop() {
    call SubControl.stop();
    // XXX message doesn't get received if we stop then start radio
    return call CommStdControl.stop();
  }


  /***********************************************************************
   * Commands and events
   ***********************************************************************/

  command result_t Send.send[uint8_t id](TOS_MsgPtr pMsg, uint16_t PayloadLen) {

    uint16_t usMHLength = offsetof(TOS_MHopMsg,data) + PayloadLen;

    if (usMHLength > TOSH_DATA_LENGTH) {
      return FAIL;
    }

//    dbg(DBG_ROUTE,"MHop: send\n");

    call RouteSelect.initializeFields(pMsg,id);

    if (call RouteSelect.selectRoute(pMsg,id, 0) != SUCCESS) {
      return FAIL;
    }
/*
    dbg(DBG_ROUTE,"MHop: out dest %d pkt 0x%x\n",
        pMsg->addr,
        ((TOS_MHopMsg *)pMsg->data)->seqno);
*/

    if (call SendMsg.send[id](pMsg->addr, usMHLength, pMsg) != SUCCESS) {
      sendFailures++;
      return FAIL;
    }

    return SUCCESS;
  }

  command void *Send.getBuffer[uint8_t id](TOS_MsgPtr pMsg, uint16_t* length) {

    TOS_MHopMsg *pMHMsg = (TOS_MHopMsg *)pMsg->data;

    *length = TOSH_DATA_LENGTH - offsetof(TOS_MHopMsg,data);

    return (&pMHMsg->data[0]);

  }

  int8_t get_buff(){
    uint8_t n;
    for (n=0; n < FWD_QUEUE_SIZE; n++) {
        uint8_t done = 0;
        atomic{
          if(FwdBufBusy[n] == 0){
            FwdBufBusy[n] = 1;
            done = 1;
          }
        }
        if(done == 1) return n;

    }
    return -1;
  }

  int8_t is_ours(TOS_MsgPtr ptr){
    uint8_t n;
    for (n=0; n < FWD_QUEUE_SIZE; n++) {
       if(FwdBufList[n] == ptr){
                return n;
       }
    }
    return -1;
  }

  static TOS_MsgPtr mForward(TOS_MsgPtr pMsg, uint8_t id) {
    TOS_MHopMsg* pMHMsg;
    int8_t buf;
    TOS_MsgPtr pNewBuf;

    pNewBuf = pMsg;
    buf = get_buff();
    pMHMsg = (TOS_MHopMsg*)pMsg->data;

    if (buf == -1)
      return pNewBuf;

    if ((call RouteSelect.selectRoute(pMsg,id, 0)) != SUCCESS) {
      FwdBufBusy[(uint8_t)buf] = 0;
      return pNewBuf;
    }

    // Failures at the send level do not cause the seq. number space to be
    // rolled back properly.  This is somewhat broken.
    if (call SendMsg.send[id](pMsg->addr,pMsg->length,pMsg) == SUCCESS) {
      call Leds.yellowOn();
      pNewBuf = FwdBufList[(uint8_t)buf];
      FwdBufList[(uint8_t)buf] = pMsg;
    }else{
      FwdBufBusy[(uint8_t)buf] = 0;
      sendFailures++;
    }

    return pNewBuf;

  }

  event TOS_MsgPtr ReceiveMsg.receive[uint8_t id](TOS_MsgPtr pMsg) {
    TOS_MHopMsg         *pMHMsg = (TOS_MHopMsg *)pMsg->data;
    uint16_t            PayloadLen = pMsg->length - offsetof(TOS_MHopMsg,data);

#if 0
    dbg(DBG_ROUTE, "MHop: Msg Rcvd, src 0x%02x, org 0x%02x, parent 0x%02x\n",
        pMHMsg->sourceaddr, pMHMsg->originaddr, 0 /*pMHMsg->parentaddr*/);
#endif

    // Ordinary message requiring forwarding
    if (pMsg->addr == TOS_LOCAL_ADDRESS) { // Addressed to local node
      if ((signal Intercept.intercept[id](pMsg,&pMHMsg->data[0],PayloadLen)) == SUCCESS) {
        pMsg = mForward(pMsg,id);
      }
    }

    // Snoop the packet for permiscuous applications
    signal Snoop.intercept[id](pMsg,&pMHMsg->data[0],PayloadLen);

    return pMsg;
  }
  uint8_t fail_count;

  event result_t SendMsg.sendDone[uint8_t id](TOS_MsgPtr pMsg, result_t success) {
    //dbg(DBG_ROUTE, "MHop: senddone 0x%x 0x%x\n", pMsg, success);
    int8_t buf;
#ifdef PLATFORM_PC
    pMsg->ack = 1;
#endif
    call Leds.yellowOff();
    if(pMsg->ack == 0 &&
       pMsg->addr != TOS_BCAST_ADDR &&
       pMsg->addr != TOS_UART_ADDR &&
       fail_count < 5){
         call RouteSelect.selectRoute(pMsg,id, 1);
         if (call SendMsg.send[id](pMsg->addr,pMsg->length,pMsg) == SUCCESS) {
           fail_count ++;
           return SUCCESS;
         } else {
           sendFailures++;
         }
    }
    fail_count = 0;
    buf = is_ours(pMsg);
    if (buf != -1) { // Msg was from forwarding queue
      FwdBufBusy[(uint8_t)buf] = 0;
    } else {
      signal Send.sendDone[id](pMsg, success);
    }
    return SUCCESS;
  }

  command uint16_t RouteControl.getParent() {
    return call RouteSelectCntl.getParent();
  }

  command uint8_t RouteControl.getQuality() {
    return call RouteSelectCntl.getQuality();
  }

  command uint8_t RouteControl.getDepth() {
    return call RouteSelectCntl.getDepth();
  }

  command uint8_t RouteControl.getOccupancy() {
    uint16_t uiOutstanding = (uint16_t)iFwdBufTail - (uint16_t)iFwdBufHead;
    uiOutstanding %= FWD_QUEUE_SIZE;
    return (uint8_t)uiOutstanding;
  }

  command uint16_t RouteControl.getSender(TOS_MsgPtr msg) {
    TOS_MHopMsg  *pMHMsg = (TOS_MHopMsg *)msg->data;
    return pMHMsg->sourceaddr;
  }

  command result_t RouteControl.setUpdateInterval(uint16_t Interval) {
    return call RouteSelectCntl.setUpdateInterval(Interval);
  }

  command result_t RouteControl.manualUpdate() {
    return call RouteSelectCntl.manualUpdate();
  }

  command uint16_t RouteStats.getSendFailures() {
    return sendFailures;
  }

  default event result_t Send.sendDone[uint8_t id](TOS_MsgPtr pMsg, result_t success) {
    return SUCCESS;
  }

  default event result_t Intercept.intercept[uint8_t id](TOS_MsgPtr pMsg, void* payload,
                                                         uint16_t payloadLen) {
    return SUCCESS;
  }

  default event result_t Snoop.intercept[uint8_t id](TOS_MsgPtr pMsg, void* payload,
                                                     uint16_t payloadLen) {
    return SUCCESS;
  }

}

