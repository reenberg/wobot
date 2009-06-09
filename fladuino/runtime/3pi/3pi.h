extern "C" {
#include <pololu/analog.h>
#include <pololu/buzzer.h>
#include <pololu/time.h>
#include <pololu/motors.h>
#include <pololu/lcd.h>
#include <pololu/leds.h>
#include <pololu/pushbuttons.h>
#include <pololu/qtr.h>
#include <pololu/encoders.h>
#include <pololu/resources.h>
#include <pololu/serial.h>  


#define IR_EMITTERS_OFF 0
#define IR_EMITTERS_ON 1
#define IR_EMITTERS_ON_AND_OFF 2

void pololu_3pi_init(unsigned int line_sensor_timeout);
void read_line_sensors(unsigned int *sensor_values, unsigned char readMode);
void emitters_on();
void emitters_off();
void calibrate_line_sensors(unsigned char readMode);
void line_sensors_reset_calibration();
void read_line_sensors_calibrated(unsigned int *sensor_values, unsigned char readMode);
unsigned int read_line(unsigned int *sensor_values, unsigned char readMode);
unsigned int read_line_white(unsigned int *sensor_values, unsigned char readMode);

unsigned int *get_line_sensors_calibrated_minimum_on();
unsigned int *get_line_sensors_calibrated_maximum_on();
unsigned int *get_line_sensors_calibrated_minimum_off();
unsigned int *get_line_sensors_calibrated_maximum_off();


}
