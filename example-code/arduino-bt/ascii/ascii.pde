// ASCII Table 
// by Nicholas Zambetti <http://www.zambetti.com> 

int ledPin = 13;                // LED connected to digital pin 13
int resetPin = 7;                  // BT module uses pin 7 for res

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
  Serial.println("SET CONTROL ESCAPE - 00 1");

  delay(2000);

  // prints title with ending line break 
  Serial.println("ASCII Table ~ Character Map"); 
 
  // wait for the long string to be sent 
  delay(100); 
} 
 
int number = 33; // first visible character '!' is #33 
 
void loop() 
{ 
  Serial.print(number, BYTE);    // prints value unaltered, first will be '!' 
  
  Serial.print(", dec: "); 
  Serial.print(number);          // prints value as string in decimal (base 10) 
  // Serial.print(number, DEC);  // this also works 
  
  Serial.print(", hex: "); 
  Serial.print(number, HEX);     // prints value as string in hexadecimal (base 16) 
  
  Serial.print(", oct: "); 
  Serial.print(number, OCT);     // prints value as string in octal (base 8) 
  
  Serial.print(", bin: "); 
  Serial.println(number, BIN);   // prints value as string in binary (base 2) 
                                 // also prints ending line break  
 
  number++; // to the next character 

  // if printed last visible character '~' #126 ... 
  if(number > 126) { 
    // loop forever 
    //while(true) { 
    //  continue; 
    //} 
    number = 33;
  }

  digitalWrite(ledPin, number % 2);
  
  delay(100); // allow some time for the Serial data to be sent 
} 
