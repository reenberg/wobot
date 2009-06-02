int ledPin = 13;                // LED connected to digital pin 13
int resetPin = 7;                  // BT module uses pin 7 for reset

void setup() 
{ 
  pinMode(ledPin, OUTPUT);      // sets the digital pin as output
  pinMode(resetPin, OUTPUT);  
  Serial.begin(115200);        // start serial at 115200 kbs

  digitalWrite(resetPin, HIGH);
  delay(10);
  digitalWrite(resetPin, LOW);
  delay(2000);

  Serial.println("SET BT PAGEMODE 3 2000 1");
  Serial.println("SET BT NAME ArduinoBT");
  Serial.println("SET BT ROLE 0 f 7d00");
  Serial.println("SET CONTROL ECHO 0");
  Serial.println("SET BT AUTH * 12345");


//  Serial.println("SET CONTROL ESCAPE - 00 1");

  Serial.println("SET CONTROL ESCAPE 43 00 0");

} 
 
int number = 33; // first visible character '!' is #33 
 
void loop() 
{ 
  number++; // to the next character 

  digitalWrite(ledPin, number % 2);
  
  delay(700); // allow some time for the Serial data to be sent 
} 
