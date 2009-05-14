#include <WString.cpp>  
#include <EEPROM.cpp>

int ledPin = 13;                // LED connected to digital pin 13
int resetPin = 7;                  // BT module uses pin 7 for reset
char inByte = 0;                // incoming serial byte
String incomingString = String(50);
int  infoSize = 0 ;
long lastTime = 0;
boolean ledon = true;
boolean commandDone = false;


void getString();
void doCommand(char *command);
char getbyte();



void setup()                    // run once, when the sketch starts
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
//  Serial.println("SET CONTROL ECHO 0");
  Serial.println("SET BT AUTH * 12345");


//  Serial.println("SET CONTROL ESCAPE - 00 1");

  Serial.println("SET CONTROL ESCAPE 43 00 0");



}

void loop()
{
  if (millis()-lastTime > 300)
  {
    ledon = !ledon;
    lastTime = millis();
  }
  if (ledon) digitalWrite(ledPin, HIGH);
  else digitalWrite(ledPin, LOW);
  if(Serial.available() > 1 )
    {
      getString();
    }
    else if (commandDone)
    {
      digitalWrite(ledPin, LOW); // set led LOW
      Serial.print("Get string:  ");  
      for(int i=0;i<infoSize;i++) //eeprom print lus
      {
        Serial.print(EEPROM.read(i));
      }
      Serial.println();
      Serial.print("Cleared string  size: ");
      Serial.println(infoSize);
      commandDone = false;
    }
}

void getString()
{
  delay(100);
  //int stringSize = 3;
  int stringSize = Serial.available();
  char comStr[stringSize];
  for (int i=0;i<stringSize;i++)
  {
    comStr[i] = getbyte();
  }
  comStr[stringSize] = 0;
  Serial.print("stringSize: ");
  Serial.print(stringSize);
  Serial.print("   ");
  Serial.println(comStr);
  doCommand(comStr);
}

void doCommand(char *command)
{
  //Serial.print("final test: ");
  //Serial.println(command);
  
  int j=0;
  digitalWrite(ledPin, HIGH); // set led HIGH
  delay(2000);  
  digitalWrite(ledPin, LOW);
  Serial.print("+++");
  delay(2000);
  digitalWrite(ledPin, HIGH);
  Serial.println(command);
  for (int i=0; i <= 10; i++){
    delay(1000);
    while (Serial.available() > 0 && j <512) {    
      inByte = getbyte();  // get incoming byte    
      EEPROM.write(j, inByte);
      j++;
    }
    delay(1000);
    digitalWrite(ledPin, LOW);
  }
  infoSize = j;  
  delay(2000);
  digitalWrite(ledPin, HIGH);
  Serial.print("+++");
  delay(2000);
  digitalWrite(ledPin, LOW); // set led low
  commandDone = true;
}


char getbyte()
{
  while (Serial.available() == 0) { //look for available data
    // do nothing, wait for incoming data
  }
  return Serial.read(); //return data if aviable
}
