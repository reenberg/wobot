#ifndef _TIMERSUPPORT_H
#define _TIMERSUPPORT_H

#include <avr/interrupt.h>  
#include <avr/io.h>

// This file takes some inspiration from:
//   http://www.uchobby.com/index.php/2007/11/24/arduino-interrupts/

#define OVERFLOWS_PER_SECOND 1000


void (*timer2_interrupt_handler)(void);
unsigned char timer2_load_value;

// Sets up timer2, so it fires with the given frequency.
// Currently it supports a range going from 62 interrupts/second
// to 16000000 interrupts/second.
void SetupTimer2(float timeoutFrequency, void (*handler)() ){
  timer2_interrupt_handler = handler;

  // How high the 8-bit TCNT2 register can count before overflowing
  int timer2_max = 256;

  // Using no prescaler the range is
  // 16000000/256 ~ 62500 to 16000000 interrupts per second
  
  // Using the 64 prescaler we get a frequency range going from:
  // 16000000/64/256 ~ 977 to 16000000/64 = 250000 interrupts per second.

  // Using 1024 as prescaler we get a frequency range going from:
  // 62 to 15625 interrupts per second

  // Select prescaler
  int prescaler;
  if (timeoutFrequency < 980)
    prescaler = 1024;
  else if (timeoutFrequency < 249000)
    prescaler = 64;
  else
    prescaler = 0;

  // The counter is incremented timer_clock_freq times every second.
  int timerClockFreq = 16000000 / prescaler;

  // The following determines how many of these incrementations that
  // has to be made before we hit the wanted interrupt frequency. 
  // And results in an initial value that gives this amount of incrementations
  // before firering.
  timer2_load_value = timer2_max - (timerClockFreq/timeoutFrequency);

  TCCR2A = 0;   // Normal counting mode  (WGM21:0 = 000)

  // Set the calculated prescaler
  switch (prescaler) {
    case 0:    // no prescaler  (CS22:0 = 001)
      TCCR2B = _BV(CS20);
      break;
    case 64:   // prescaler: /64  (CS22:0 = 100)
      TCCR2B = _BV(CS22);
      break;
    case 1024: // prescaler: /1024  (CS22:0 = 111)
      TCCR2B = _BV(CS22) | _BV(CS21) | _BV(CS20);
      break;
  }

  // load the timer for its first cycle
  TCNT2 = timer2_load_value;

  // Start the timer
  TIMSK2 = _BV(TOIE2); 
  sei();
}


ISR(TIMER2_OVF_vect) {
  // Activate the handler.
  timer2_interrupt_handler();

  // This is how much error we have due to interrupt latency and time
  // used in the interrupt handler.
  unsigned char latency = TCNT2;

  // Reload the timer and correct for latency.
  TCNT2 = latency + timer2_load_value;
}


void (*timer1_interrupt_handler)(void);
unsigned char timer1_load_value;

// Sets up timer1, so it fires with the given frequency.
// Currently it supports a range going from 0.4 interrupts/second
// to 16000000 interrupts/second.
void SetupTimer1(float timeoutFrequency, void (*handler)() ){
  timer1_interrupt_handler = handler;

  // How high the 16-bit TCNT1 register can count before overflowing
  int timer1_max = ~0;

  // Using no prescaler the range is
  // 16000000/256 ~ 244 to 16000000 interrupts per second
  
  // Using the 64 prescaler we get a frequency range going from:
  // 16000000/64/256 ~ 3.82 to 16000000/64 = 250000 interrupts per second.

  // Using 1024 as prescaler we get a frequency range going from:
  // ~ 0.24 to 15625 interrupts per second

  // Select prescaler
  int prescaler;
  if (timeoutFrequency < 4)
    prescaler = 1024;
  else if (timeoutFrequency < 249000)
    prescaler = 64;
  else
    prescaler = 0;

  // The counter is incremented timer_clock_freq times every second.
  int timerClockFreq = 16000000 / prescaler;

  // The following determines how many of these incrementations that
  // has to be made before we hit the wanted interrupt frequency. 
  // And results in an initial value that gives this amount of incrementations
  // before firering.
  timer1_load_value = (timer1_max - (timerClockFreq/timeoutFrequency)) + 1;

  TCCR1A = 0;   // Normal counting mode  (WGM21:0 = 000)

  // Set the calculated prescaler
  switch (prescaler) {
    case 0:    // no prescaler  (CS22:0 = 001)
      TCCR1B = _BV(CS20);
      break;
    case 64:   // prescaler: /64  (CS22:0 = 011)
      TCCR2B = _BV(CS22);
      break;
    case 1024: // prescaler: /1024  (CS22:0 = 101)
      TCCR2B = _BV(CS22) | _BV(CS21) | _BV(CS20);
      break;
  }

  // load the timer for its first cycle
  TCNT1 = timer1_load_value;

  // Start the timer
  TIMSK1 = _BV(TOIE1); 
  sei();
}


ISR(TIMER1_OVF_vect) {
  // Activate the handler.
  timer1_interrupt_handler();

  // This is how much error we have due to interrupt latency and time
  // used in the interrupt handler.
  unsigned char latency = TCNT1;

  // Reload the timer and correct for latency.
  TCNT1 = latency + timer1_load_value;
}

#endif
