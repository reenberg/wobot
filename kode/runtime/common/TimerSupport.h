#ifndef _TIMERSUPPORT_H
#define _TIMERSUPPORT_H

#include <avr/interrupt.h>  
#include <avr/io.h>

#define INIT_TIMER_COUNT 6  
#define RESET_TIMER2 TCNT2 = INIT_TIMER_COUNT  

void timer2_called(void);

// Aruino runs at 16 Mhz, so we have 1000 Overflows per second...  
// 1/ ((16000000 / 64) / 256) = 1 / 1000
ISR(TIMER2_OVF_vect) {  
  RESET_TIMER2;  
  timer2_called();
};  

#endif
