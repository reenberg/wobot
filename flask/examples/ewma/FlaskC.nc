configuration FlaskC {
    provides interface StdControl;
    uses interface Flow;
} implementation {
    components FlaskM;
    components TimerC;
    StdControl = FlaskM.StdControl;
    StdControl = TimerC;
    FlaskM.Timer10 -> TimerC.Timer[unique("Timer")];
    Flow = FlaskM.Flow;
}
