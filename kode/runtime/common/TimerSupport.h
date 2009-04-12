#ifndef _TIMERSUPPORT_H
#define _TIMERSUPPORT_H

#include <avr/interrupt.h>  
#include <avr/io.h>

void timer2_called(void);

unsigned int timerLoadValue;

#define TIMER_CLOCK_FREQ 2000000.0 //2MHz for /8 prescale from 16MHz

//Setup Timer2.
//Configures the ATMega168 8-Bit Timer2 to generate an interrupt
//at the specified frequency.
//Returns the timer load value which must be loaded into TCNT2
//inside your ISR routine.
//See the example usage below.
unsigned char SetupTimer2(float timeoutFrequency){
  unsigned char result; //The timer load value.

  //Calculate the timer load value
  result=(int)((257.0-(TIMER_CLOCK_FREQ/timeoutFrequency))+0.5);
  //The 257 really should be 256 but I get better results with 257.

  //Timer2 Settings: Timer Prescaler /8, mode 0
  //Timer clock = 16MHz/8 = 2Mhz or 0.5us
  //The /8 prescale gives us a good range to work with
  //so we just hard code this for now.
  TCCR2A = 0;
  TCCR2B = 0<<CS22 | 1<<CS21 | 0<<CS20;

  //Timer2 Overflow Interrupt Enable
  TIMSK2 = 1<<TOIE2;

  //load the timer for its first cycle
  TCNT2=result;

  return(result);
}

// Aruino runs at 16 Mhz, so we have 1000 Overflows per second...  
// 1/ ((16000000 / 64) / 256) = 1 / 1000
#define OVERFLOWS_PER_SECOND 1000

ISR(TIMER2_OVF_vect) {
  //Toggle the IO pin to the other state.
  timer2_called();

  //Capture the current timer value. This is how much error we
  //have due to interrupt latency and the work in this function
  int latency=TCNT2;

  //Reload the timer and correct for latency.
  TCNT2=latency+timerLoadValue;
}

#endif
