// $Id: GenericComm.nc,v 1.9 2003/12/12 07:28:23 jpolastre Exp $

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
 * Authors:             Jason Hill, David Gay, Philip Levis
 * Date last modified:  6/25/02
 *
 */

/**
 * @author Jason Hill
 * @author David Gay
 * @author Philip Levis
 */


configuration InstrumentedGenericComm
{
  provides {
    interface StdControl as Control;
#if defined(INSTRUMENTEDCOMMPROMISCUOUS)
    interface CommControl;
#endif /* defined(INSTRUMENTEDCOMMPROMISCUOUS) */
    interface InstrumentedComm;

    // The interface are as parameterised by the active message id
    interface SendMsg[uint8_t id];
    interface ReceiveMsg[uint8_t id];

    // How many packets were received in the past second
    command uint16_t activity();
  }
  uses {
    interface BufferedSend;
    interface ReceiveMsg as InstrumentedCommReceiveMsg;
    interface Leds;
    // signaled after every send completion for components which wish to
    // retry failed sends
    event result_t sendDone();
  }
}
implementation
{
  // CRCPacket should be multiply instantiable. As it is, I have to use
  // RadioCRCPacket for the radio, and UARTNoCRCPacket for the UART to
  // avoid conflicting components of CRCPacket.
#if defined(INSTRUMENTEDCOMMPROMISCUOUS)
  components AMPromiscuous as AM;
#else /* !defined(INSTRUMENTEDCOMMPROMISCUOUS) */
  components AMStandard as AM;
#endif
  components
    RadioCRCPacket as RadioPacket,
    UARTFramedPacket as UARTPacket,
    NoLeds as NoLeds,
    TimerC, HPLPowerManagementM;
  components InstrumentedCommM;

  Control = AM.Control;
  Control = InstrumentedCommM.StdControl;
#if defined(INSTRUMENTEDCOMMPROMISCUOUS)
  CommControl = AM.CommControl;
#endif /* defined(INSTRUMENTEDCOMMPROMISCUOUS) */
  InstrumentedComm = InstrumentedCommM;
  SendMsg = InstrumentedCommM.SendMsg;
  ReceiveMsg = InstrumentedCommM.ReceiveMsg;
  BufferedSend = InstrumentedCommM.BufferedSend;
  InstrumentedCommReceiveMsg = InstrumentedCommM.InstrumentedCommReceiveMsg;
  Leds = InstrumentedCommM.Leds;
  sendDone = AM.sendDone;

  InstrumentedCommM.OrigSendMsg -> AM.SendMsg;
  InstrumentedCommM.OrigReceiveMsg -> AM.ReceiveMsg;

  activity = AM.activity;
  AM.TimerControl -> TimerC.StdControl;
  AM.ActivityTimer -> TimerC.Timer[unique("Timer")];

  AM.UARTControl -> UARTPacket.Control;
  AM.UARTSend -> UARTPacket.Send;
  AM.UARTReceive -> UARTPacket.Receive;

  AM.RadioControl -> RadioPacket.Control;
  AM.RadioSend -> RadioPacket.Send;
  AM.RadioReceive -> RadioPacket.Receive;
  AM.PowerManagement -> HPLPowerManagementM.PowerManagement;

#if defined(INSTRUMENTEDCOMMPROMISCUOUS)
  AM.Leds -> NoLeds;
#endif /* defined(INSTRUMENTEDCOMMPROMISCUOUS) */
}
