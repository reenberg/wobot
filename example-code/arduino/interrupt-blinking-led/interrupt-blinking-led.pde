// interrupt based light flashing program for atmel mega8l
// 
// Craig Limber, July 2004
//

#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/signal.h>

//-----------------------------------------------------------------------------
ISR(TIMER1_COMPA_vect)
{
  static uint8_t ledon;

  if (ledon)
  {
    ledon = 0;
    PORTB = 0x00;
  }
  else
  {
    ledon = 1;
    PORTB = 0xFF;
  }
}

//-----------------------------------------------------------------------------
void setup()
{
  DDRB = 0xFF;     // enable output

  //----------------------------------------------------
  // Set up timer 1 to generate an interrupt every 1 ms
  //----------------------------------------------------
  TCCR1A = 0x00;
  TCCR1B = (_BV(CS12) |  // 256 prescale
            _BV(WGM12)); // CTC mode (Clear-on-Terminal-Count), TOP = OCR1A 
  OCR1A  = 15625;        // count up to TOP 
  TIMSK1 = _BV(OCIE1A);
  
  sei(); // Enable global interrupts.
}

void loop()
{
  asm volatile("nop" ::);  // we spin!  Could also put processor to sleep 
}
