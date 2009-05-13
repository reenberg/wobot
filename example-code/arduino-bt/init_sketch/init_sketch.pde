int LED = 13;   // select the pin for the LED
int RESET = 7;

void setup() {
  pinMode(LED,OUTPUT);   // declare the LED's pin as output
  pinMode(RESET,OUTPUT);   // declare the LED's pin as output
  Serial.begin(115200);        // connect to the serial port
  digitalWrite(RESET, HIGH);
  delay(10);
  digitalWrite(RESET, LOW);
  delay(2000);
  Serial.println("SET BT PAGEMODE 3 2000 1");
  Serial.println("SET BT NAME ARDUINOBT");
  Serial.println("SET BT ROLE 0 f 7d00");
  Serial.println("SET CONTROL ECHO 0");
  Serial.println("SET BT AUTH * 12345");
  Serial.println("SET CONTROL ESCAPE - 00 1");
  Serial.println("SET CONTROL BAUD 115200,8n1");      //first release 19200
}

void loop () {
  digitalWrite(LED, HIGH);
  delay(100);
  digitalWrite(LED, LOW);
  Serial.println("ciao");
  delay(1000);
}

