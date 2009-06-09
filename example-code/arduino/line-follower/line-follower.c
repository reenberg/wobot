#include <pololu/3pi.h>

void calibrate_3pi_sensors () {
  int counter;
  while(!button_is_pressed(BUTTON_B));
  for(counter=0; counter<80; counter++) {
      if(counter < 20 || counter >= 60)
        set_motors(40,-40);
      else
        set_motors(-40,40);

      calibrate_line_sensors(IR_EMITTERS_ON);
      delay_ms(20);
  }
  set_motors(0,0);
  while(!button_is_pressed(BUTTON_B));
}

int main()
{
  unsigned int sensors[5];

  pololu_3pi_init(2000);
  calibrate_3pi_sensors();

  while(1) {
      unsigned int position = read_line(sensors,IR_EMITTERS_ON);

      if(position < 1000)
         set_motors(0,100);
      else if(position < 3000) 
          set_motors(100,100);
      else 
          set_motors(100,0);
  }
}
