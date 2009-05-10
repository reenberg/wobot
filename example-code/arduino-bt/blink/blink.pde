#include <avr/interrupt.h>
#include <avr/io.h>

#define LED1 13
#define RESET 7


void setup() {

  pinMode(LED1, OUTPUT);
  pinMode(RESET, OUTPUT);
  
  Serial.begin(115200);        // connect to the serial port

  digitalWrite(RESET, HIGH);
  delay(10);
  digitalWrite(RESET, LOW);
  delay(2000);

  Serial.println("SET BT PAGEMODE 3 2000 1");
  Serial.println("SET BT NAME ArduinoBT");
  Serial.println("SET BT ROLE 0 f 7d00");
  Serial.println("SET CONTROL ECHO 0");
  Serial.println("SET BT AUTH * 12345");
  Serial.println("SET CONTROL ESCAPE - 00 1");


}

void loop() {

     digitalWrite(LED1, HIGH);
     delay(1000);

}
