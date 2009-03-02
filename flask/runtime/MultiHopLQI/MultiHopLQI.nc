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
 * Authors:          Gilman Tolle
 */


includes MultiHop;

module MultiHopLQI {

  provides {
    interface StdControl;
    interface RouteSelect;
    interface RouteControl;
  }

  uses {
    interface Timer;

    interface SendMsg;
    interface ReceiveMsg;

    interface Random;

    interface RouteStats;

#ifdef TIMESYNC
    interface Time;
    interface TimeUtil;
    interface TimeSet;
    interface RadioCoordinator;
#endif
  }
}

implementation {

  enum {
    BASE_STATION_ADDRESS = BASE_NODE_ID,
    BEACON_PERIOD        = 32,
    BEACON_TIMEOUT       = 8,
  };

  enum {
    ROUTE_INVALID    = 0xff
  };


  TOS_Msg msgBuf;
  bool msgBufBusy;

  uint16_t gbCurrentParent;
  uint16_t gbCurrentParentCost;
  uint16_t gbCurrentLinkEst;
  uint8_t  gbCurrentHopCount;
  uint16_t gbCurrentCost;

  uint8_t gLastHeard;

  int16_t gCurrentSeqNo;

  uint16_t gUpdateInterval;

  uint8_t gRecentIndex;
  uint16_t gRecentPacketSender[MHOP_HISTORY_SIZE];
  int16_t gRecentPacketSeqNo[MHOP_HISTORY_SIZE];

  uint8_t gRecentOriginIndex;
  uint16_t gRecentOriginPacketSender[MHOP_HISTORY_SIZE];
  int16_t gRecentOriginPacketSeqNo[MHOP_HISTORY_SIZE];

  uint16_t adjustLQI(uint8_t val) {
    uint16_t result = (80 - (val - 50));
    result = (((result * result) >> 3) * result) >> 3;
    return result;
  }

  task void SendRouteTask() {
    TOS_MHopMsg *pMHMsg = (TOS_MHopMsg *) &msgBuf.data[0];
    BeaconMsg *pRP = (BeaconMsg *)&pMHMsg->data[0];
    uint8_t length = offsetof(TOS_MHopMsg,data) + sizeof(BeaconMsg);

    dbg(DBG_ROUTE,"MultiHopRSSI Sending route update msg.\n");

    if (gbCurrentParent != TOS_BCAST_ADDR) {
      dbg(DBG_ROUTE,"MultiHopRSSI: Parent = %d\n", gbCurrentParent);
    }

    if (msgBufBusy) {
#ifndef PLATFORM_PC
      post SendRouteTask();
#endif
      return;
    }

    dbg(DBG_ROUTE,"MultiHopRSSI: Current cost: %d.\n",
        gbCurrentParentCost + gbCurrentLinkEst);

    pRP->parent = gbCurrentParent;
    pRP->cost = gbCurrentParentCost + gbCurrentLinkEst;
    pMHMsg->sourceaddr = pMHMsg->originaddr = TOS_LOCAL_ADDRESS;
    pRP->hopcount = gbCurrentHopCount;
    pMHMsg->hopcount = gbCurrentHopCount;
    pMHMsg->originseqno = gCurrentSeqNo;
    pMHMsg->seqno = gCurrentSeqNo++;

    if (call SendMsg.send(TOS_BCAST_ADDR, length, &msgBuf) == SUCCESS) {
      atomic msgBufBusy = TRUE;
    }
  }

  task void TimerTask() {
    uint8_t val;
    atomic val = ++gLastHeard;
    if ((TOS_LOCAL_ADDRESS != BASE_STATION_ADDRESS) && (val > BEACON_TIMEOUT)) {
      gbCurrentParent = TOS_BCAST_ADDR;
      gbCurrentParentCost = 0x7fff;
      gbCurrentLinkEst = 0x7fff;
      gbCurrentHopCount = ROUTE_INVALID;
      gbCurrentCost = 0xfffe;
    }
    post SendRouteTask();
  }

  command result_t StdControl.init() {
    int n;

    gRecentIndex = 0;
    for (n = 0; n < MHOP_HISTORY_SIZE; n++) {
      gRecentPacketSender[n] = TOS_BCAST_ADDR;
      gRecentPacketSeqNo[n] = 0;
    }

    gRecentOriginIndex = 0;
    for (n = 0; n < MHOP_HISTORY_SIZE; n++) {
      gRecentOriginPacketSender[n] = TOS_BCAST_ADDR;
      gRecentOriginPacketSeqNo[n] = 0;
    }

    gbCurrentParent = TOS_BCAST_ADDR;
    gbCurrentParentCost = 0x7fff;
    gbCurrentLinkEst = 0x7fff;
    gbCurrentHopCount = ROUTE_INVALID;
    gbCurrentCost = 0xfffe;

    gCurrentSeqNo = 0;
    gUpdateInterval = BEACON_PERIOD;
    atomic msgBufBusy = FALSE;

    if (TOS_LOCAL_ADDRESS == BASE_STATION_ADDRESS) {
      gbCurrentParent = TOS_UART_ADDR;
      gbCurrentParentCost = 0;
      gbCurrentLinkEst = 0;
      gbCurrentHopCount = 0;
      gbCurrentCost = 0;
    }

    return SUCCESS;
  }

  command result_t StdControl.start() {
    gLastHeard = 0;
    call Timer.start(TIMER_ONE_SHOT,
                     call Random.rand() % (1024 * gUpdateInterval));
    return SUCCESS;
  }

  command result_t StdControl.stop() {
    call Timer.stop();
    return SUCCESS;
  }

  command bool RouteSelect.isActive() {
    return TRUE;
  }

  command result_t RouteSelect.selectRoute(TOS_MsgPtr Msg, uint8_t id,
                                           uint8_t resend) {
    int i;
    TOS_MHopMsg *pMHMsg = (TOS_MHopMsg *)&Msg->data[0];

//    if (gbCurrentParent != TOS_UART_ADDR && resend == 0) {
    if (pMHMsg->originaddr != TOS_LOCAL_ADDRESS && resend == 0) {
      // supress duplicate packets
      for (i = 0; i < MHOP_HISTORY_SIZE; i++) {
        if ((gRecentPacketSender[i] == pMHMsg->sourceaddr) &&
            (gRecentPacketSeqNo[i] == pMHMsg->seqno)) {
          return FAIL;
        }
      }

      gRecentPacketSender[gRecentIndex] = pMHMsg->sourceaddr;
      gRecentPacketSeqNo[gRecentIndex] = pMHMsg->seqno;
      gRecentIndex = (gRecentIndex + 1) % MHOP_HISTORY_SIZE;

      // supress multihop cycles and try to break out of it
      for (i = 0; i < MHOP_HISTORY_SIZE; i++) {
        if (gbCurrentParent != TOS_UART_ADDR &&
            (gRecentOriginPacketSender[i] == pMHMsg->originaddr) &&
            (gRecentOriginPacketSeqNo[i] == pMHMsg->originseqno)) {
          gbCurrentParentCost = 0x7fff;
          gbCurrentLinkEst = 0x7fff;
          gbCurrentParent = TOS_BCAST_ADDR;
          gbCurrentHopCount = ROUTE_INVALID;
          return FAIL;
        }
      }
      gRecentOriginPacketSender[gRecentOriginIndex] = pMHMsg->originaddr;
      gRecentOriginPacketSeqNo[gRecentOriginIndex] = pMHMsg->originseqno;
      gRecentOriginIndex = (gRecentOriginIndex + 1) % MHOP_HISTORY_SIZE;
    }

    if (gbCurrentParent != TOS_UART_ADDR && resend == 0) {
      pMHMsg->seqno = gCurrentSeqNo++;
    }
    pMHMsg->sourceaddr = TOS_LOCAL_ADDRESS;
    Msg->addr = gbCurrentParent;

    return SUCCESS;
  }

  command result_t RouteSelect.initializeFields(TOS_MsgPtr Msg, uint8_t id) {
    TOS_MHopMsg *pMHMsg = (TOS_MHopMsg *)&Msg->data[0];

    pMHMsg->sourceaddr = pMHMsg->originaddr = TOS_LOCAL_ADDRESS;
    pMHMsg->originseqno = gCurrentSeqNo;
    pMHMsg->hopcount = gbCurrentHopCount;

    return SUCCESS;
  }

  command uint8_t* RouteSelect.getBuffer(TOS_MsgPtr Msg, uint16_t* Len) {

  }

  command uint16_t RouteControl.getParent() {
    return gbCurrentParent;
  }

  command uint8_t RouteControl.getQuality() {
    return gbCurrentLinkEst;
  }

  command uint8_t RouteControl.getDepth() {
    return gbCurrentHopCount;
  }

  command uint8_t RouteControl.getOccupancy() {
    return 0;
  }

  command uint16_t RouteControl.getSender(TOS_MsgPtr msg) {
    TOS_MHopMsg         *pMHMsg = (TOS_MHopMsg *)msg->data;
    return pMHMsg->sourceaddr;
  }

  command result_t RouteControl.setUpdateInterval(uint16_t Interval) {

    gUpdateInterval = Interval;
    return SUCCESS;
  }

  command result_t RouteControl.manualUpdate() {
    post SendRouteTask();
    return SUCCESS;
  }


  event result_t Timer.fired() {
    post TimerTask();
    call Timer.start(TIMER_ONE_SHOT, 1024 * gUpdateInterval + 1);
    return SUCCESS;
  }

  event TOS_MsgPtr ReceiveMsg.receive(TOS_MsgPtr Msg) {

    TOS_MHopMsg *pMHMsg = (TOS_MHopMsg *)&Msg->data[0];
    BeaconMsg *pRP = (BeaconMsg *)&pMHMsg->data[0];

//    dbg(DBG_ROUTE, "Received Beacon(source=%d, cost=%d, strength=%d)\n",
//      pMHMsg->sourceaddr, pRP->cost, Msg->strength);

    /* if the message is from my parent
       store the new link estimation */

    if (pMHMsg->sourceaddr == gbCurrentParent) {
      // try to prevent cycles
      if (pRP->parent != TOS_LOCAL_ADDRESS) {
        gLastHeard = 0;
        gbCurrentParentCost = pRP->cost;
#if !defined(PLATFORM_PC)
        gbCurrentLinkEst = adjustLQI(Msg->lqi);
#else
        gbCurrentLinkEst = 1;
#endif
        gbCurrentHopCount = pRP->hopcount + 1;
      }
      else if (gbCurrentParent != TOS_UART_ADDR) {
        gLastHeard = 0;
        gbCurrentParentCost = 0x7fff;
        gbCurrentLinkEst = 0x7fff;
        gbCurrentParent = TOS_BCAST_ADDR;
        gbCurrentHopCount = ROUTE_INVALID;
      }

#ifdef TIMESYNC
      call TimeSet.set(call TimeUtil.create(0, pRP->timestamp));
      dbg(DBG_ROUTE,"TimeSync: Setting Time To: %d\n", pRP->timestamp);
#endif

    } else {

    /* if the message is not from my parent,
       compare the message's cost + link estimate to my current cost,
       switch if necessary */

      // make sure you don't pick a parent that creates a cycle
#if !defined(PLATFORM_PC)
      if (((uint32_t) pRP->cost + (uint32_t) adjustLQI(Msg->lqi)
          <
          ((uint32_t) gbCurrentParentCost + (uint32_t) gbCurrentLinkEst) -
          (((uint32_t) gbCurrentParentCost + (uint32_t) gbCurrentLinkEst) >> 2)
         ) &&
         (pRP->parent != TOS_LOCAL_ADDRESS)) {
#else
      if (((uint32_t) pRP->cost + 1
          <
          ((uint32_t) gbCurrentParentCost + (uint32_t) gbCurrentLinkEst) -
          (((uint32_t) gbCurrentParentCost + (uint32_t) gbCurrentLinkEst) >> 2)
         ) &&
         (pRP->parent != TOS_LOCAL_ADDRESS)) {
#endif
        gLastHeard = 0;
        gbCurrentParent = pMHMsg->sourceaddr;
        gbCurrentParentCost = pRP->cost;
#if !defined(PLATFORM_PC)
        gbCurrentLinkEst = adjustLQI(Msg->lqi);
#else
        gbCurrentLinkEst = 1;
#endif
        gbCurrentHopCount = pRP->hopcount + 1;

      }
    }

    return Msg;
  }

  event result_t SendMsg.sendDone(TOS_MsgPtr pMsg, result_t success) {
    atomic msgBufBusy = FALSE;
    return SUCCESS;
  }

#ifdef TIMESYNC
  async event void RadioCoordinator.startSymbol(uint8_t bitsPerBlock,
                                                uint8_t offset,
                                                TOS_MsgPtr msgBuff) {
    tos_time_t endTime;
    TOS_MHopMsg *pMHMsg = (TOS_MHopMsg *) &msgBuff->data[0];
    BeaconMsg *pRP = (BeaconMsg *) &pMHMsg->data[0];

    atomic {
      if (msgBufBusy == TRUE) {
        endTime = call Time.get();
        pRP->timestamp = endTime.low32;
        dbg(DBG_ROUTE,"TimeSync: End Send RoutePacket Time %d\n", endTime.low32);
      }
    }
  }

  async event void RadioCoordinator.byte(TOS_MsgPtr msg, uint8_t byteCount) {
      /* XXX: do nothing */
  }

  async event void RadioCoordinator.blockTimer() {
      /* XXX: do nothing */
  }
#endif

}

