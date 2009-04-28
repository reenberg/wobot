#include <common/PCINT.h>

SIGNAL(PCINT0_vect) {
  PCint(0);
}
SIGNAL(PCINT1_vect) {
  PCint(1);
}
SIGNAL(PCINT2_vect) {
  PCint(2);
}

volatile long ticktocks = 0;
long i = 0;
volatile int on = 0;


void tick(void) {
  ticktocks++;
      on = 1 - on;
  digitalWrite(13, on);
}

void tock(void) {
  ticktocks--;
}

void setup()
{
  pinMode(13, OUTPUT);
  Serial.begin(9600);
  pinMode(4, INPUT);
  pinMode(5, INPUT);
  delay(3000);
  PCattachInterrupt(4, tick, CHANGE);
  PCattachInterrupt(5, tock, CHANGE);
}

void loop() {
  i++;
  delay(1000);

  Serial.print(i, DEC);
  Serial.print(" ");
  Serial.println(ticktocks);
  
  if (i > 256) {
    PCdetachInterrupt(4);
    PCdetachInterrupt(5);
  }
}
