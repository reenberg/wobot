// $Id: MultiHop.h,v 1.5 2004/11/16 21:09:22 gtolle Exp $

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
 *
 * Authors:             Philip Buonadonna, Crossbow, Gilman Tolle
 * Date last modified:  2/20/03
 *
 */

/**
 * @author Philip Buonadonna
 */


#ifndef _TOS_MULTIHOP_H
#define _TOS_MULTIHOP_H

#ifndef MHOP_QUEUE_SIZE
#define MHOP_QUEUE_SIZE 16
#endif

#ifndef MHOP_HISTORY_SIZE
#define MHOP_HISTORY_SIZE 10
#endif

#include "AM.h"
enum {
  AM_BEACONMSG = 250,
  AM_MULTIHOPMSG = 250,
  AM_DEBUGPACKET = 3
};

/* Fields of neighbor table */
typedef struct TOS_MHopNeighbor {
  uint16_t addr;                     // state provided by nbr
  uint16_t recv_count;               // since last goodness update
  uint16_t fail_count;               // since last goodness, adjusted by TOs
  uint16_t hopcount;
  uint8_t goodness;
  uint8_t timeouts;                  // since last recv
} TOS_MHopNeighbor;

typedef struct MultihopMsg {
  uint16_t sourceaddr;
  uint16_t originaddr;
  int16_t seqno;
  int16_t originseqno;
  uint16_t hopcount;
  uint8_t data[(TOSH_DATA_LENGTH - 10)];
} TOS_MHopMsg;

typedef struct BeaconMsg {
  uint16_t parent;
  uint16_t cost;
  uint16_t hopcount;
  uint32_t timestamp;
} BeaconMsg;

typedef struct DBGEstEntry {
  uint16_t id;
  uint8_t hopcount;
  uint8_t sendEst;
} DBGEstEntry;

typedef struct DebugPacket {
//  uint16_t seqno;
  uint16_t estEntries;
  DBGEstEntry estList[0];
} DebugPacket;

#endif /* _TOS_MULTIHOP_H */

