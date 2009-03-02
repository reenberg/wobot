module EWMAM {
    provides interface StdControl;
    uses interface Timer as Timer10;
    uses interface Flow;
} implementation {
    #include <Flask.h>
    void ssend3_in(double arg1);
    double v_Float0;
    double v0_Float0;
    double sintegrate2_cstate;
    typedef struct t2_Float_Float {
        double element1;
        double element2;
    } t2_Float_Float;
    t2_Float_Float v_t2_Float_Floattot2_Float_Float0(double arg1, double arg2);
    void sintegrate2_out_Floattou0(double arg1);
    void sintegrate2_in(double arg1);
    int v_i0;
    int v2_i0;
    int sintegrate1_cstate;
    typedef struct t2_Float_i {
        double element1;
        int element2;
    } t2_Float_i;
    t2_Float_i f_t2_u_itot2_Float_i0(int arg1);
    void sintegrate1_out_Floattou0(double arg1);
    void sintegrate1_in();
    void clock0_out_utou0();
    void clock0_in_utou0();
    void ssend3_in(double arg1)
    {
        call Flow.anycast(1, &arg1, sizeof(arg1));
    }
    t2_Float_Float v_t2_Float_Floattot2_Float_Float0(double arg1, double arg2)
    {
        t2_Float_Float temp;
        
        {
            double cur_Float0;
            t2_Float_Float temp1;
            
            cur_Float0 = 0.5 * arg1 + 0.5 * arg2;
            temp1.element1 = cur_Float0;
            temp1.element2 = cur_Float0;
            temp = temp1;
        }
        return temp;
    }
    void sintegrate2_out_Floattou0(double arg1)
    {
        ssend3_in(arg1);
    }
    void sintegrate2_in(double arg1)
    {
        t2_Float_Float _out;
        
        {
            _out = v_t2_Float_Floattot2_Float_Float0(arg1, sintegrate2_cstate);
            sintegrate2_cstate = _out.element2;
            sintegrate2_out_Floattou0(_out.element1);
        }
        ;
    }
    t2_Float_i f_t2_u_itot2_Float_i0(int arg1)
    {
        t2_Float_i temp;
        
        {
            t2_Float_i temp1;
            
            if(arg1 == 0) {
                t2_Float_i temp2;
                
                temp2.element1 = 1.0;
                temp2.element2 = 1;
                temp1 = temp2;
            } else if(arg1 == 1) {
                t2_Float_i temp2;
                
                temp2.element1 = 0.0;
                temp2.element2 = 0;
                temp1 = temp2;
            } else {
                error("No match");
                temp1 = *(t2_Float_i *) NULL;
            }
            temp = temp1;
        }
        return temp;
    }
    void sintegrate1_out_Floattou0(double arg1)
    {
        sintegrate2_in(arg1);
    }
    void sintegrate1_in()
    {
        t2_Float_i _out;
        
        {
            _out = f_t2_u_itot2_Float_i0(sintegrate1_cstate);
            sintegrate1_cstate = _out.element2;
            sintegrate1_out_Floattou0(_out.element1);
        }
        ;
    }
    void clock0_out_utou0()
    {
        sintegrate1_in();
    }
    void clock0_in_utou0()
    {
        clock0_out_utou0();
    }
    event result_t Timer10.fired()
    {
        clock0_in_utou0();
        ;
        return SUCCESS;
    }
    event void Flow.receive(flowid_t flow_id, void *data, size_t size)
    {
        switch(flow_id) {
        }
    }
    command result_t StdControl.init()
    {
        return SUCCESS;
    }
    command result_t StdControl.start()
    {
        v_Float0 = 0.0;
        v0_Float0 = v_Float0;
        sintegrate2_cstate = v0_Float0;
        v_i0 = 0;
        v2_i0 = v_i0;
        sintegrate1_cstate = v2_i0;
        call Timer10.start(TIMER_REPEAT, 10L);
        return SUCCESS;
    }
    command result_t StdControl.stop()
    {
        return SUCCESS;
    }
}
