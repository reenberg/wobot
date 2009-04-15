#include <avr/interrupt.h>
#include <avr/io.h>

#define LED1 13
#define LED2 12
#define LED3 11



volatile int on = 0;
volatile int counter;

ISR(TIMER2_OVF_vect) {

  // Because the prescaler is 1024 and the timer is 8 bit, the timer will fire:
  // 16000000/1024/256 times per second ~ 61 times per second
  if (counter == 61) {
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


  TCCR2A = 0;   // Normal counting mode
  TCCR2B = _BV(CS22) | _BV(CS21) | _BV(CS20); // prescaler: 1024


  // enable the timer
  TIMSK2 = _BV(TOIE2); 
  sei();  
}

int i = 0;

void loop() {
  Serial.print("loop: ");
  Serial.println(i);

  digitalWrite(LED2, HIGH);
  digitalWrite(LED3, LOW);
  delay(1000);

  digitalWrite(LED2, LOW);
  digitalWrite(LED3, HIGH);
  delay(1000);

  i++;
}
