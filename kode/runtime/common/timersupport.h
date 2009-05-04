#ifndef _TIMERSUPPORT_H
#define _TIMERSUPPORT_H

#include <avr/interrupt.h>  
#include <avr/io.h>

// This file takes some inspiration from:
//   http://www.uchobby.com/index.php/2007/11/24/arduino-interrupts/

#define OVERFLOWS_PER_SECOND 1000

void timer2_interrupt_handler(void);

unsigned char timer2_init_value;

// Sets up timer2, so it fires with the given frequency.
// Currently it supports a range going from 977 interrupts/second
// to 250000 interrupts/second. The prescaler can eventually be made
// dynamic to support wider ranges.
void SetupTimer2(float interrupt_frequency){
  int timer2_max = 256;

  // Using the 64 prescaler we get a frequency range going from:
  // 16000000/64/256 ~ 977 to 16000000/64 = 250000 times per second.
  int prescaler = 64;

  int timer_clock_freq = 16000000 / prescaler;

  // The counter is incremented timer_clock_freq times every second.
  // The following determines how many of these incrementations that
  // has to be made before we hit the wanted interrupt frequency. 
  // And results in an initial that gives this amount of incrementations
  // before firering.
  timer2_init_value = timer2_max - (timer_clock_freq/interrupt_frequency);

  TCCR2A = 0;   // Normal counting mode  (WGM21:0 = 000)
  TCCR2B = _BV(CS22); // prescaler: /64  (CS22:0 = 100)

  // load the timer for its first cycle
  TCNT2 = timer2_init_value;

  // Start the timer
  TIMSK2 = _BV(TOIE2); 
  sei();
}

ISR(TIMER2_OVF_vect) {
  // Activate the handler.
  timer2_interrupt_handler();

  // This is how much error we have due to interrupt latency and time
  // used in the interrupt handler.
  int latency = TCNT2;

  // Reload the timer and correct for latency.
  TCNT2 = timer2_init_value + latency;
}

#endif
