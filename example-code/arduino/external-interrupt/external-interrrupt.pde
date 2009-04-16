#include <avr/interrupt.h>
#include <avr/io.h>

#define LED1 13
#define LED2 12
#define LED3 11


volatile long i = 0;


/*
void setup() {
  pinMode(LED1, OUTPUT);
  pinMode(LED2, OUTPUT);
  pinMode(LED3, OUTPUT);

  Serial.begin(9600);

  attachInterrupt(0, interrupt(), CHANGE);

  //sei();  
}


void loop() {

  Serial.print("loop: ");
  Serial.println(i);

  if (i % 2 == 0)
    {
      
      digitalWrite(LED2, HIGH);
      digitalWrite(LED3, LOW);
    }
  else
    {
      digitalWrite(LED2, LOW);
      digitalWrite(LED3, HIGH);
    }
}

void interrupt() {
  ++ i;
}
*/

volatile int state = LOW;

void setup()
{
  pinMode(LED1, OUTPUT);
  attachInterrupt(0, blink, CHANGE);
}

void loop()
{
  digitalWrite(LED1, state);
}

void blink()
{
  state = !state;
}
