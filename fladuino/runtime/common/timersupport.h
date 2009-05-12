#ifndef _TIMERSUPPORT_H
#define _TIMERSUPPORT_H

#include <avr/interrupt.h>  
#include <avr/io.h>

#define OVERFLOWS_PER_SECOND 1000


void (*timer2_interrupt_handler)(void);
unsigned char timer2LoadValue;

// timeoutFrequency 
void SetupTimer2(float timeoutFrequency, void (*handler)() ){
  timer2_interrupt_handler = handler;
  int prescaler;
  if (timeoutFrequency < 980)
    prescaler = 1024;
  else if (timeoutFrequency < 249000)
    prescaler = 64;
  else
    prescaler = 0;

  int timerClockFreq = 16000000 / prescaler;

  timer2LoadValue = 256 - (timerClockFreq/timeoutFrequency);

  TCCR2A = 0;   // Normal counting mode

  // Set the calculated prescaler
  switch (prescaler) {
    case 0:
      TCCR2B = _BV(CS20);
      break;
    case 64: 
      TCCR2B = _BV(CS22);
      break;
    case 1024:
      TCCR2B = _BV(CS22) | _BV(CS21) | _BV(CS20);
      break;
  }
  //load the timer for its first cycle
  TCNT2 = timer2LoadValue;

  TIMSK2 = _BV(TOIE2); 
  sei();
}


ISR(TIMER2_OVF_vect) {
  //Toggle the IO pin to the other state.
  timer2_interrupt_handler();

  //Capture the current timer value. This is how much error we
  //have due to interrupt latency and the work in this function
  unsigned char latency=TCNT2;

  //Reload the timer and correct for latency.
  TCNT2=latency+timer2LoadValue;
}


void (*timer1_interrupt_handler)(void);
unsigned int timer1LoadValue;

// timeoutFrequency 
void SetupTimer1(float timeoutFrequency, void (*handler)()){
  timer1_interrupt_handler = handler;
  int prescaler;
  if (timeoutFrequency < 4)
    prescaler = 1024;
  else if (timeoutFrequency < 249000)
    prescaler = 64;
  else
    prescaler = 0;

  float timerClockFreq = 16000000 / prescaler;
  unsigned int timerMax = ~0;
  
  timer1LoadValue = timerMax - (timerClockFreq/timeoutFrequency);

  TCCR1A = 0;   // Normal counting mode

  // Set the calculated prescaler
  switch (prescaler) {
    case 0:
      TCCR1B = _BV(CS10);
      break;
    case 64: 
      TCCR1B = _BV(CS11) | _BV(CS10);
      break;
    case 1024:
      TCCR1B = _BV(CS12) | _BV(CS10);
      break;
  }
  //load the timer for its first cycle
  TCNT1 = timer1LoadValue;

  TIMSK1 = _BV(TOIE1);
  sei();
}


ISR(TIMER1_OVF_vect) {
  //Toggle the IO pin to the other state.
  timer1_interrupt_handler();

  //Capture the current timer value. This is how much error we
  //have due to interrupt latency and the work in this function
  unsigned int latency=TCNT1;

  //Reload the timer and correct for latency.
  TCNT1=latency+timer1LoadValue;
}



#endif
