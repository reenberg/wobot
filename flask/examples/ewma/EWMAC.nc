configuration EWMAC {
    provides interface StdControl;
    uses interface Flow;
} implementation {
    components EWMAM;
    components TimerC;
    StdControl = EWMAM.StdControl;
    StdControl = TimerC;
    EWMAM.Timer10 -> TimerC.Timer[unique("Timer")];
    Flow = EWMAM.Flow;
}
