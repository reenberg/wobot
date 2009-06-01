#include <stdio.h>
#include <stdlib.h>

/*
   Convert a hex string to its ascii representation.
   Each hex value must be seperated by a space.
*/
void hexToAscii(char msg[], char out[])
{
  char *msgEnd;
  out[0] = strtol(msg, &msgEnd, 16);
  if (out[0] != 0) {
    int i = 1;
    while ((out[i] = strtol(msgEnd, &msgEnd, 16)) != 0)
      i++;
    out[i] = '\0';
  }
  else
        out[1] = '\0';
}

void chr2hexStr(unsigned char hexStr[], int *outPos, unsigned char chr, unsigned char buf[3]) {

  // Needs to handle hex value 0x00 in a special way else it would generate '0'
  // as firsch char and '' as second
  if (chr == 0x00) {
    hexStr[(*outPos)++] = '0';
    hexStr[(*outPos)++] = '0';
  }
  else {
    sprintf(buf, "%X", chr);
    
    hexStr[(*outPos)++] = buf[0];
    hexStr[(*outPos)++] = buf[1];
  }

  
}

unsigned char * muxCommand (const char cmd[], int * outCmdLen, unsigned char link) {

  int cmdLen;
  for (cmdLen = 0; cmd[cmdLen] != 0x00; cmdLen++);
  int pos = 0;
  // The resulting mux frame contains 5 fixed size fields + data field plus the same
  // minus one for spaces
  int resultLen = 5+cmdLen;
  
  unsigned char * muxCmd = (unsigned char *)malloc(sizeof(char[resultLen]));


  //Generate packet
  muxCmd[pos++] = 0xbf;       // SOF
  muxCmd[pos++] = link;       // Link (0xFF=Control, 0x00 = connection 1, etc.)
  muxCmd[pos++] = 0x00;       // Flags
  muxCmd[pos++] = cmdLen;     //Data length

  //Insert data into correct position in the frame
  while (pos < cmdLen+4) {
    muxCmd[pos] = cmd[pos-4];
    pos++;
  }

  muxCmd[pos]=link^0xff;    //nlink

  printf ("resultlen: %i\n", resultLen);
  printf ("cmdLen: %i\n", cmdLen);
  printf ("pos: %i\n", pos);

  // Save resulting char array length
  *outCmdLen = resultLen; 

  return muxCmd;

}

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

  for (i = 0; i < pos ; i++)
      Serial.print(outbuf[i]);

#endif

  printf("%X\n",(unsigned char) 0xBF);        // SOF
  printf("%X\n",(unsigned char) link);        // Link (0xFF=Control, 0x00 = connection 1, etc.)
  printf("%X\n",(unsigned char) 0x00);        // Flags (always 0x00)
  printf("%X\n",(unsigned char) cmdLen);      // Data length

  int i;
  for (i = 0; i < cmdLen; i++) {
    printf("%X\n",(unsigned char) cmd[i]);    // Data
  }

  printf("%X\n",(unsigned char) link^0xFF);  // nLink  
}




int main(int argc, char* argv[])
{
  //char hexMsg[4];
  //hexToAscii("52 11 40", hexMsg);
  //printf("'%i'\n", hexMsg[0]);

  
  int outbufLen;
  int i;
  unsigned char * outbuf = muxCommand("\x52\x11\x40", &outbufLen, 0x00);
  
  for (i = 0; i < outbufLen ; i++)
    printf("%c\t%X\t%i\n", outbuf[i], outbuf[i], i);

  //printf("%s\n", outbuf);

  free(outbuf);

  
  sendMuxCommand("\x52\x11\x40", 0x00);
  
  /*
  unsigned char buff[3]; // Buffer for converting hex value to hex string.
  unsigned char * hexCmdStr = malloc (sizeof(char[2*resultLen])); // each hex value is two chars.
  int hexCmdStrlen = 0, i = 0;

  while (i < pos){ 
    chr2hexStr(hexCmdStr, &hexCmdStrlen, muxCmd[i++], buff);
  }

  */


}
