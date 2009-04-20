#include <util/delay.h>
#include <avr/interrupt.h>
#include <avr/io.h>
#include <common/Flask.h>
#include <common/event_dispatch.h>
void toggle3_in(int arg1);
void smap2_out_itou0(int arg1);
int f_itoi0(int arg1);
void smap2_in_itou0(int arg1);
void adc1_out_itou0(int arg1);
void adc1_in();
#include <common/timersupport.h>
void clock0_out_utou0();
void clock0_in_utou0();
void timer1(void);
void timer2_interrupt_handler(void);
void toggle3_in(int arg1)
{
    analogWrite(10, arg1);
}
void smap2_out_itou0(int arg1)
{
    toggle3_in(arg1);
}
int f_itoi0(int arg1)
{
    return arg1 / 4;
}
void smap2_in_itou0(int arg1)
{
    int temp;
    
    temp = f_itoi0(arg1);
    smap2_out_itou0(temp);
}
void adc1_out_itou0(int arg1)
{
    smap2_in_itou0(arg1);
}
void adc1_in()
{
    int v = analogRead(0);
    
    adc1_out_itou0(v);
}
void clock0_out_utou0()
{
    adc1_in();
}
void clock0_in_utou0()
{
    clock0_out_utou0();
}
void timer1(void)
{
    clock0_in_utou0();
}
void timer2_interrupt_handler(void)
{
    static int timer1_counter = 0;
    
    timer1_counter = timer1_counter + 1;
    if(timer1_counter >= OVERFLOWS_PER_SECOND / 1000 * 1) {
        queue_funcall(&timer1);
        timer1_counter = 0;
    }
}
void setup()
{
    pinMode(10, OUTPUT);
    analogWrite(10, 0);
    SetupTimer2(OVERFLOWS_PER_SECOND);
}
void loop()
{
    if(event_available()) {
        struct event event = pop_event();
        
        switch(event.type) {
            
          case FCALL_EVENT:
            (*event.data.fcall_event_data.func)();
            break;
            
          default:
            break;
        }
    }
}