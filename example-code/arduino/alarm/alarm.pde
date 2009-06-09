#include <avr/interrupt.h>
#include <avr/io.h>
#include <../../../fladuino/runtime/common/PCINT.h>
#include <../../../fladuino/runtime/common/timersupport.h>

#define incButtonPin 2
#define decButtonPin 3
#define alarmPin 13

#define secondsPerUnit (15*60)

#define stateBits 4
#define startDiode 4


volatile unsigned int timer = 0;

void changeTimer(unsigned int delta) {
  if (delta > timer) {
    timer = 0;
  } else {
    timer = min(1<<stateBits, timer + secondsPerUnit);
  }
  for (unsigned int d = startDiode; d < startDiode+stateBits; ++d) {
    digitalWrite(d, timer & (1 << (d-startDiode)) ? HIGH : LOW);
  }
  if (timer > 0) {
    analogWrite(alarmPin, 0);
  } else {
    analogWrite(alarmPin, 128);
  }
}

void increaseButton() {
  changeTimer(secondsPerUnit);
}

void decreaseButton() {
  changeTimer(-secondsPerUnit);
}

void timerHit() {
  changeTimer(-1);
}

void setup()
{
  for (unsigned int d = startDiode; d < startDiode+stateBits; ++d) {
    pinMode(d, OUTPUT);
    digitalWrite(d, LOW);
  }
  analogWrite(alarmPin, 0);
  PCattachInterrupt(incButtonPin, increaseButton, CHANGE);
  PCattachInterrupt(decButtonPin, decreaseButton, CHANGE);
  SetupTimer2(1000.0, timer_hit);
}

void loop()
{
}
