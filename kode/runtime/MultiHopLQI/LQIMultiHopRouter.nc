includes MultiHop;

configuration LQIMultiHopRouter {
  
  provides {
    interface StdControl;
    // The interface are as parameterised by the active message id
    // only the 10 active message ids defined MultiHop.h are supported.
    interface Receive[uint8_t id];
    interface Intercept[uint8_t id];
    interface Intercept as Snoop[uint8_t id];
    interface Send[uint8_t id];
    interface RouteControl;
    interface RouteStats;
  }

  uses {
    interface ReceiveMsg[uint8_t id];
  }
}

implementation {

  components 
    MultiHopEngineM, 
    MultiHopLQI,
    SimpleTime,
    GenericComm as Comm, 
    TimerC, 
#ifdef MHOP_LEDS
    LedsC,
#else
    NoLeds as LedsC,
#endif
    RandomLFSR;

  components CC2420RadioC as RadioCoord;

  StdControl = MultiHopEngineM;
  Receive = MultiHopEngineM;
  Send = MultiHopEngineM;
  Intercept = MultiHopEngineM.Intercept;
  Snoop = MultiHopEngineM.Snoop;
  RouteControl = MultiHopEngineM;
  RouteStats = MultiHopEngineM;

  ReceiveMsg = MultiHopEngineM;

#ifdef TIMESYNC
  MultiHopEngineM.SubControl -> SimpleTime.StdControl;
#endif
  MultiHopEngineM.SubControl -> MultiHopLQI.StdControl;
  MultiHopEngineM.CommStdControl -> Comm;
  MultiHopEngineM.RouteSelectCntl -> MultiHopLQI.RouteControl;
  MultiHopEngineM.RouteSelect -> MultiHopLQI;

  MultiHopEngineM.SendMsg -> Comm.SendMsg;
  
  MultiHopLQI.Timer -> TimerC.Timer[unique("Timer")];  

  MultiHopEngineM.Leds -> LedsC;

  MultiHopLQI.SendMsg -> Comm.SendMsg[AM_BEACONMSG];
  MultiHopLQI.ReceiveMsg -> Comm.ReceiveMsg[AM_BEACONMSG];

  MultiHopLQI.Random -> RandomLFSR;

  MultiHopLQI.RouteStats -> MultiHopEngineM;

  // TimeSync-related Components
#ifdef TIMESYNC
  MultiHopLQI.Time -> SimpleTime.Time;
  MultiHopLQI.TimeUtil -> SimpleTime.TimeUtil;
  MultiHopLQI.TimeSet -> SimpleTime.TimeSet;
  MultiHopLQI.RadioCoordinator -> RadioCoord.RadioSendCoordinator;
#endif
}
