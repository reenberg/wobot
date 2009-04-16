#include <avr/interrupt.h>
#include <avr/io.h>
#include <common/timersupport.h>

#define LED1 13
#define LED2 12
#define LED3 11



volatile int on = 0;
volatile int counter;

void timer2_interrupt_handler() {
  // Because the prescaler is 1024 and the timer is 8 bit, the timer will fire:
  // 16000000/1024/256 times per second ~ 61 times per second
  if (counter == 1000) {
    counter = 0;
    if(on) {
      digitalWrite(LED1, LOW);
    }
    else {
      digitalWrite(LED1, HIGH);
    }
    on = 1 - on;
  }

  counter++;
}


void setup() {
  pinMode(LED1, OUTPUT);
  pinMode(LED2, OUTPUT);
  pinMode(LED3, OUTPUT);

  Serial.begin(9600);
  
  SetupTimer2(1000);
}

int i = 0;

void loop() {
  Serial.print("loop: ");
  Serial.println(on);

  digitalWrite(LED2, HIGH);
  digitalWrite(LED3, LOW);
  delay(1000);

  digitalWrite(LED2, LOW);
  digitalWrite(LED3, HIGH);
  delay(1000);

  i++;
}
