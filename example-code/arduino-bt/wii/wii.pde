int ledPin = 13;                // LED connected to digital pin 13
int resetPin = 7;                  // BT module uses pin 7 for reset
int gndPin = 8;

int pin1 = 9;
int pin2 = 10;
int pin3 = 11;

#include <SoftwareSerial.h>


SoftwareSerial mySerial =  SoftwareSerial(2,3);


void sendMuxCommand(const char cmd[], unsigned char link) {
  
  int cmdLen;
  // Find the length of the cmd char array.
  for (cmdLen = 0; cmd[cmdLen] != 0x00; cmdLen++);

#ifdef debug
  int pos = 0;

  // The resulting mux frame contains 5 fixed size fields + data field plus the same
  // minus one for spaces
  unsigned char muxFrame[5+cmdLen];

  //Generate packet
  muxFrame[pos++] = 0xbf;       // SOF
  muxFrame[pos++] = link;       // Link (0xFF=Control, 0x00 = connection 1, etc.)
  muxFrame[pos++] = 0x00;       // Flags
  muxFrame[pos++] = cmdLen;     // Data length

  //Insert data into correct position in the frame
  while (pos-4 < cmdLen) {
    muxFrame[pos] = cmd[pos-4];
    pos++;
  }

  muxFrame[pos]=link^0xff;    //nlink

  int i;
  for (i = 0; i < pos ; i++)
      Serial.print(muxFrame[i]);
#endif

  Serial.print((unsigned char) 0xBF);         // SOF
  Serial.print((unsigned char) link);         // Link (0xFF=Control, 0x00 = connection 1, etc.)
  Serial.print((unsigned char) 0x00);         // Flags (always 0x00)
  Serial.print((unsigned char) cmdLen);       // Data length

  int i;
  for (i = 0; i < cmdLen; i++) {
    Serial.print((unsigned char) cmd[i]);     // Data
  }

  Serial.print((unsigned char) (link^0xFF));  // nLink  
}

void setup() 
{ 
  pinMode(ledPin, OUTPUT);      // sets the digital pin as output
  pinMode(resetPin, OUTPUT);  
  Serial.begin(115200);        // start serial at 115200 kbs

  digitalWrite(resetPin, HIGH);
  delay(10);
  digitalWrite(resetPin, LOW);
  delay(2000);

  //Make sure we are not in mux mode
  sendMuxCommand("SET CONTROL MUX 0", 0xFF);
  delay(100);

  Serial.println("SET BT PAGEMODE 3 2000 1");
  Serial.println("SET BT NAME ArduinoBT");
  Serial.println("SET BT ROLE 0 f 7d00");
//  Serial.println("SET CONTROL ECHO 7");
  Serial.println("SET CONTROL ECHO 0");
  Serial.println("SET BT AUTH * 12345");
  //  Serial.println("SET BT AUTH *");
//  Serial.println("SET CONTROL ESCAPE - 00 1");
  Serial.println("SET CONTROL ESCAPE 43 00 0");

  pinMode(gndPin, OUTPUT);
  digitalWrite(gndPin, LOW);

  // Set pins as input.
  pinMode(pin1, INPUT);
  pinMode(pin2, INPUT);
  pinMode(pin3, INPUT);
  // Utilize the internal pullup resistor
  digitalWrite(pin1, HIGH);
  digitalWrite(pin2, HIGH);
  digitalWrite(pin3, HIGH);


  //Software serial
  pinMode(2, INPUT);
  pinMode(3, OUTPUT);

  mySerial.begin((long)6900);
  

  
}

int number = 0;
int connected = 0;

unsigned char buffer[200];
int i = 0;
 
void loop() 
{ 
  number++;

  // Toggle ledPin.
  digitalWrite(ledPin, number % 2);

  if (digitalRead(pin1) == LOW) {
    digitalWrite(ledPin, HIGH);

    sendMuxCommand("close 0", 0xFF);
    delay(100);
    sendMuxCommand("SET CONTROL MUX 0", 0xFF);
    delay(500);
    Serial.println(" ");
    Serial.println("SET BT AUTH * 12345");

    connected = 0;
    
    while(digitalRead(pin1) == LOW)
      delay(100);
  }

  if (digitalRead(pin2) == LOW) { 
    digitalWrite(ledPin, HIGH);
    
    Serial.println("set bt auth *");
    Serial.println("SET CONTROL MUX 1");

    sendMuxCommand("call 00:1E:A9:7D:E0:9A 11 hid", 0xFF);

    connected = 1;

    delay(500);

    while (digitalRead(pin2) == LOW) {
      // Set led and rumble
      sendMuxCommand("\x52\x11\xF1", 0x00);
      delay(100);
    }

    // Unset rumble
    sendMuxCommand("\x52\x11\xF0", 0x00);
  }


  if (digitalRead(pin3) == LOW) {
    //sendMuxCommand("\x52\x12\x30", 0x00);

    int a;
    for (a = 0; a < i; a++){
      // Break line if start of frame
      if (buffer[a] == 0xBF)
        mySerial.print("\n");

      // If below or eq to 0x0F then prefix with a zero
      // so it always displays two digits
      if (buffer[a] <= 0x0F) 
        mySerial.print("0");
      
      mySerial.print(buffer[a]);
    }
    mySerial.print("\n");
    mySerial.print(i);


    i = 0;

    while (digitalRead(pin3) == LOW)
      delay(100);
  }



  if (i<200) {
    while (Serial.available() > 0) {
      
      buffer[i++] = (unsigned char)Serial.read();

    }
      //    Serial.print("I received: ");
    //Serial.println(Serial.read(), HEX);
  }

  
  delay(700); // allow some time for the Serial data to be sent 
} 


    /*
    sendMuxCommand(&chr, 0x02);
    sendMuxCommand("\n", 0x02);
    
    if (chr == 0xA2) {
      sendMuxCommand(&chr, 0x02);
      sendMuxCommand("\n", 0x02);
      while (Serial.available() > 0) {
        chr = Serial.read();
        sendMuxCommand(&chr, 0x02);
        sendMuxCommand("\n", 0x02);
      }
    */
