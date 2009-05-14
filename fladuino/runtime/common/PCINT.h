#ifndef _PCINT_H
#define _PCINT_H

#include <pins_arduino.h>

/*
 * An extension to the interrupt support for arduino.  add pin change
 * interrupts to the external interrupts, giving a way for users to
 * have interrupts drive off of any pin that supports the PCINT
 * signal.  Refer to avr-gcc header files, arduino source and atmega
 * datasheet.
 */

/*
 * Theory: all IO pins on Atmega168 are covered by Pin Change
 * Interrupts.  The PCINT corresponding to the pin must be enabled and
 * masked, and an ISR routine provided.  Since PCINTs are per port,
 * not per pin, the ISR must use some logic to actually implement a
 * per-pin interrupt service.
 */

/* Pin to interrupt map for Duemilanove, Atmega168/328:
 * D0-D7 = PCINT 16-23 = PCIR2 = PD = PCIE2 = PCMSK2
 * D8-D13 = PCINT 0-5 = PCIR0 = PB = PCIE0 = PCMSK0
 * A0-A5 (D14-D19) = PCINT 8-13 = PCIR1 = PC = PCIE1 = PCMSK1
 */

/* Pin to interrupt map for MEGA, Atmega1280 (pay attention to the
 * occasional declining order, yes, it's this confusing):
 *
 * D0      = PCINT 8     = PE0   = PCICR1 = PCMSK1 <- This one is weird.
 * D10-D13 = PCINT 4-7   = PB4-7 = PCICR0 = PCMSK0
 * D14-D15 = PCINT 10-9  = PJ1-0 = PCICR1 = PCMSK1 <- These don't work.
 * D50-D53 = PCINT 3-0   = PB3-0 = PCICR0 = PCMSK0
 * A8-A15  = PCINT 16-23 = PK0-7 = PCICR2 = PCMSK2
 *
 * Additionally, we have regular INT interrupts on pins 2-3 and 18-21.
 */

/* In conclusion, this library provides you with CHANGE interrupts on
 * the following pins on Duemilanove, Atmega168/328:
 *
 * D0-D13, A0-A5.
 *
 * And the following on MEGA, Atmega1280:
 *
 * D1-D7, D10-D13, D50-D53, A8-A15.
 */

#if defined(__AVR_ATmega1280__)
volatile uint8_t *port_to_pcmask[] = {
  NULL,
  NULL,
  &PCMSK0,
  NULL,
  NULL,
  &PCMSK1,
  NULL,
  NULL,
  NULL,
  NULL,
  &PCMSK1,
  &PCMSK2
};
volatile uint8_t port_to_pcicrN[] = {
  NULL,
  NULL,
  0,
  NULL,
  NULL,
  1,
  NULL,
  NULL,
  NULL,
  NULL,
  1,
  2
};
volatile uint8_t pcint_to_port[] = {
  2,
  5,
  11
};
#define E_NOINT 42
uint8_t pin_to_intnum(uint8_t pin) {
  switch (pin) {
  case 2:
    return 0;
  case 3:
    return 1;
  case 18:
    return 5;
  case 19:
    return 4;
  case 20:
    return 3;
  case 21:
    return 2;
  default:
    return E_NOINT;
  }
}
#else
volatile uint8_t *port_to_pcmask[] = {
  NULL,
  NULL,
  &PCMSK0,
  &PCMSK1,
  &PCMSK2
};
volatile uint8_t port_to_pcicrN[] = {
  NULL,
  NULL,
  0,
  1,
  2
};
volatile uint8_t pcint_to_port[] = {
  2,
  3,
  4
};
#endif

typedef void (*voidFuncPtr)(void);

volatile static voidFuncPtr PCintFunc[24] = { NULL };

volatile static uint8_t PCintLast[3];

/*
 * attach an interrupt to a specific pin using pin change interrupts.
 * First version only supports CHANGE mode.
 */
void PCattachInterrupt(uint8_t pin, voidFuncPtr userFunc, int mode) {
  uint8_t bit = digitalPinToBitMask(pin);
  uint8_t port = digitalPinToPort(pin);
  uint8_t slot;
  volatile uint8_t *pcmask;

  if (mode == CHANGE) {
#if defined(__AVR_ATmega1280__)
    uint8_t intnum;
    if ((intnum = pin_to_intnum(pin)) != E_NOINT)
      return attachInterrupt(intnum, userFunc, mode);
#endif
    // map pin to PCIR register
    if (port != NOT_A_PORT) {
      pcmask = port_to_pcmask[port];
      slot = port_to_pcicrN[port] * 8 + bit;
      PCintFunc[slot] = userFunc;
      if (pin == 14) bit <<= 1;
      // set the mask
      *pcmask |= bit;
      // enable the interrupt
      PCICR |= _BV(port_to_pcicrN[port]);
    }
  }
}

void PCdetachInterrupt(uint8_t pin) {
  uint8_t bit = digitalPinToBitMask(pin);
  uint8_t port = digitalPinToPort(pin);
  volatile uint8_t *pcmask;
#if defined(__AVR_ATmega1280__)
  uint8_t intnum;
  if ((intnum = pin_to_intnum(pin)) != E_NOINT)
    return detachInterrupt(intnum);
#endif
  // map pin to PCIR register
  if (port == NOT_A_PORT) {
    return;
  } 
  else {
    pcmask = port_to_pcmask[port];
  }

  // disable the mask.
  *pcmask &= ~bit;
  // If that's the last one, disable the interrupt.
  if (*pcmask == 0) {
    PCICR &= ~(0x01 << port);
  }
}

// Common code for ISR handler.  "pcint" is the PCINT number.
// There isn't really a good way to back-map ports and masks to pins.
static void PCint(uint8_t pcint) {
  uint8_t bit;
  uint8_t curr;
  uint8_t mask;
  uint8_t slot;
  uint8_t port;
  port = pcint_to_port[pcint];
  // Get the pin states for the indicated port.
  curr = *portInputRegister(port);
  mask = curr ^ PCintLast[pcint];
  PCintLast[pcint] = curr;
  // mask is pins that have changed. screen out non pcint pins.
  if (mask &= *port_to_pcmask[port]) {
    // mask is PCINT pins that have changed.
    for (uint8_t i=0; i < 8; i++) {
      bit = _BV(i);
      if (bit & mask) {
        slot = port_to_pcicrN[port] * 8 + bit;
        if (PCintFunc[slot] != NULL) {
          PCintFunc[slot]();
        }
      }
    }
  }
}

ISR(PCINT0_vect) {
  PCint(0);
}
ISR(PCINT1_vect) {
  PCint(1);
}
ISR(PCINT2_vect) {
  PCint(2);
}

#endif // PCINT_H
