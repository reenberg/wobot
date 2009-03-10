#ifndef __QUERY_H__
#define __QUERY_H__

enum {
    AM_LOCALMSG = 64
};

struct LocalMsg {
    uint16_t id;
    uint16_t temp;
    uint16_t radio_send_unicast_cnt;
    uint16_t radio_send_bcast_cnt;
    uint16_t radio_recv_unicast_cnt;
    uint16_t radio_recv_bcast_cnt;
};

enum {
    AM_RESULTMSG = 65
};

struct ResultMsg {
    uint16_t count;
    uint16_t avg_temp;
};

#endif /* __QUERY_H__ */
