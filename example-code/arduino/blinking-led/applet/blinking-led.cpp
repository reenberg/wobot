#include "WProgram.h"
#include <util/delay.h>

bool led[8];

int number[10];
long rnd;


void setup () {

  DDRB = 0xFF; // Set whole PortB as output.
  DDRD = 0xFF; // Set whole PortD as output.

  Serial.begin(9600);
  
  // Nothing must be connected to pin 0, for it to have best effect
  srand(analogRead(0));
  //randomSeed(analogRead(0)); 


  //srandom(millis()); // Seed rand().

  led[0] = 0;
  led[1] = 0;
  led[2] = 0;
  led[3] = 0;
  led[4] = 0;
  led[5] = 0;
  led[6] = 0;
  led[7] = 0;

  number[0] = 0b00111111;;
  number[1] = 0b00000110;
  number[2] = 0b01011011;
  number[3] = 0b01001111;
  number[4] = 0b01100110;
  number[5] = 0b01101101;
  number[6] = 0b01111101;
  number[7] = 0b00000111;
  number[8] = 0b01111111;
  number[9] = 0b01100111;
}

int updateLed() {
  return
    led[7] * 128 +
    led[6] * 64 +
    led[5] * 32 +
    led[4] * 16 +
    led[3] * 8 +
    led[2] * 4 +
    led[1] * 2 +
    led[0] * 1;
}
 
void loop ()
{
  
  
  for (int i=0; i < 10; i++) {
    
    //rndom between 0 and 8 (not included)
    rnd = rand() % 8;
    led[rnd] ^= 1; // Flip

    //Serial.println(rnd);

    PORTB = updateLed(); // Write the new led values.
    PORTD = number[i]; // Write number to 7-segment display.

    //_delay_us(20); // Sleep

  }

 
}
int main(void)
{
	init();

	setup();
    
	for (;;)
		loop();
        
	return 0;
}

