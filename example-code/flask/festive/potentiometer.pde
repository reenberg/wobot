#include <util/delay.h>
#include <avr/interrupt.h>
#include <avr/io.h>
#include <common/Flask.h>
#include <common/event_dispatch.h>
void toggle1_in();
void toggle2_in();
void setValue5_in(int arg1);
int v_i0;
int v0_i0;
int sintegrate4_cstate;
typedef struct t2_i_i {
    int element1;
    int element2;
} t2_i_i;
t2_i_i f_t2_u_itot2_i_i0(int arg1);
void sintegrate4_out_itou0(int arg1);
void sintegrate4_in();
unsigned char device_1state;
unsigned char device_2state;
#include <common/timersupport.h>
void clock3_out_utou0();
void clock3_in_utou0();
void clock0_out_utou0();
void clock0_in_utou0();
void timer1(void);
void timer500(void);
void timer2_interrupt_handler(void);
void toggle1_in()
{
    if(device_2state == HIGH)
        device_2state = LOW;
     else
        device_2state = HIGH;
    
    digitalWrite(8, device_2state);
}
void toggle2_in()
{
    if(device_1state == HIGH)
        device_1state = LOW;
     else
        device_1state = HIGH;
    
    digitalWrite(9, device_1state);
}
void setValue5_in(int arg1)
{
    analogWrite(10, arg1);
}
t2_i_i f_t2_u_itot2_i_i0(int arg1)
{
    t2_i_i temp;
    
    {
        t2_i_i temp1;
        
        if(arg1 == 512) {
            t2_i_i temp2;
            
            temp2.element1 = 0;
            temp2.element2 = 0;
            temp1 = temp2;
        } else {
            t2_i_i temp2;
            
            if(arg1 >= 256) {
                t2_i_i temp3;
                
                temp3.element1 = 256 - (arg1 - 256);
                temp3.element2 = arg1 + 1;
                temp2 = temp3;
            } else {
                t2_i_i temp3;
                
                temp3.element1 = arg1;
                temp3.element2 = arg1 + 1;
                temp2 = temp3;
            }
            temp1 = temp2;
        }
        temp = temp1;
    }
    return temp;
}
void sintegrate4_out_itou0(int arg1)
{
    setValue5_in(arg1);
}
void sintegrate4_in()
{
    t2_i_i _out;
    
    {
        _out = f_t2_u_itot2_i_i0(sintegrate4_cstate);
        sintegrate4_cstate = _out.element2;
        sintegrate4_out_itou0(_out.element1);
    }
    ;
}
void clock3_out_utou0()
{
    sintegrate4_in();
}
void clock3_in_utou0()
{
    clock3_out_utou0();
}
void clock0_out_utou0()
{
    toggle2_in(), toggle1_in();
}
void clock0_in_utou0()
{
    clock0_out_utou0();
}
void timer1(void)
{
    clock3_in_utou0();
}
void timer500(void)
{
    clock0_in_utou0();
}
void timer2_interrupt_handler(void)
{
    static int timer1_counter = 0;
    static int timer500_counter = 0;
    
    timer1_counter = timer1_counter + 1;
    if(timer1_counter >= OVERFLOWS_PER_SECOND / 1000 * 1) {
        queue_funcall(&timer1);
        timer1_counter = 0;
    }
    timer500_counter = timer500_counter + 1;
    if(timer500_counter >= OVERFLOWS_PER_SECOND / 1000 * 500) {
        queue_funcall(&timer500);
        timer500_counter = 0;
    }
}
void setup()
{
    v_i0 = 0;
    v0_i0 = v_i0;
    sintegrate4_cstate = v0_i0;
    pinMode(10, OUTPUT);
    analogWrite(10, 0);
    pinMode(9, OUTPUT);
    device_1state = LOW;
    digitalWrite(9, LOW);
    pinMode(8, OUTPUT);
    device_2state = HIGH;
    digitalWrite(8, HIGH);
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