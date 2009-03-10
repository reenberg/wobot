/*
 * Copyright (c) 2005-2007
 *      The President and Fellows of Harvard College.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/* Author: Geoffrey Mainland <mainland@eecs.harvard.edu> */

#include "fassert.h"
#include "Flow.h"
#include "FlowAction.h"

#define FLOW_INCSTAT(stat) ++mStats.stat

module FlowM {
    provides interface StdControl;
    provides interface NetTime;
    provides interface Flow;
    provides interface FlowControl;
    provides interface FlowDebug;

    uses interface SendMsg;
    uses interface SendMsg as UrgentSendMsg;
    uses interface ReceiveMsg;

    uses interface FlowInterest as SubscribeInterest;
    uses interface FlowInterest as PublishInterest;

    uses interface FlowSeqno;

    uses interface NeighborTable;

    uses interface Pool<struct flowaction_t> as Pool;
    uses interface Queue<struct flowaction_t*> as SendQueue;
    uses interface Queue<struct flowaction_t*> as ReceiveQueue;

    uses interface LinkEstimator;

    uses interface SysTime;
    uses interface Timer as RefreshTimer;

#if defined(FLOW_APP_ACKS)
    uses interface Timer as AckTimer;
#endif /* defined(FLOW_APP_ACKS) */

#if defined(TOSMSG_HAS_ACK)
    uses interface MacControl;
#endif /* defined(TOSMSG_HAS_ACK) */
} implementation {
#include "FlowMsg.h"
    bool mInited = FALSE;

    /* FlowControl variables */
    struct flowstats_t mStats;
    uint16_t mPubsubRepeat;
    uint16_t mRefreshTime;
    uint16_t mPurgeFactor;
    uint16_t mMaxAckRetrans;

    uint32_t mRefreshSecs;

    bool mSending = FALSE;
    bool mSendPending = FALSE;
    bool mReceivePending = FALSE;

    struct flowaction_t* allocAction();
    void freeAction(struct flowaction_t*);

    task void processSendQueue();
    result_t scheduleSendQueue();
    result_t enqueueSendImmed(struct flowaction_t* act);
    result_t enqueueSend(struct flowaction_t* act);
    result_t performSend(struct flowaction_t* act);

    task void processReceiveQueue();
    result_t enqueueReceive(struct flowaction_t* act);
    result_t performReceive(struct flowaction_t* act);

    bool addSubscription(flowid_t flow_id,
                         uint16_t subscriber_id,
                         uint16_t next_hop,
                         metric_t metric,
                         bool active);
    void removeSubscription(flowid_t flow_id,
                            uint16_t subscriber_id);

    bool addPublication(flowid_t flow_id,
                        uint16_t publisher_id,
                        uint16_t next_hop,
                        metric_t metric,
                        bool active);
    void removePublication(flowid_t flow_id,
                           uint16_t publisher_id);

#if defined(FLOW_APP_ACKS)
    result_t sendAck(uint16_t src,
                     flowseqno_t seqno,
                     uint16_t hop);
#endif /* defined(FLOW_APP_ACKS) */

    result_t sendSubscribe(flowid_t flow_id,
                           uint16_t subscriber_id,
                           flowseqno_t seqno,
                           uint8_t ttl,
                           uint8_t repeat,
                           uint16_t next_hop,
                           metric_t metric);
    result_t sendUnsubscribe(flowid_t flow_id,
                             uint16_t subscriber_id,
                             flowseqno_t seqno,
                             uint8_t ttl,
                             uint8_t repeat);

    result_t sendPublish(flowid_t flow_id,
                         uint16_t publisher_id,
                         flowseqno_t seqno,
                         uint8_t ttl,
                         uint8_t repeat,
                         uint16_t next_hop,
                         metric_t metric);
    result_t sendUnpublish(flowid_t flow_id,
                           uint16_t publisher_id,
                           flowseqno_t seqno,
                           uint8_t ttl,
                           uint8_t repeat);

    result_t resendSubscriptions();
    result_t resendPublications();

    result_t sendData(flowid_t flow_id,
                      uint16_t src,
                      flowseqno_t seqno,
                      uint16_t flags,
                      void* data, size_t size);
    result_t sendDataCont(struct flowaction_t* act);

    result_t sendBcast(flowid_t flow_id,
                       uint16_t src,
                       flowseqno_t seqno,
                       uint8_t ttl,
                       uint8_t repeat,
                       void* data, size_t size);

    struct flowint_t* bestNextHop(flowid_t flow_id);

    TOS_MsgPtr postReceive(TOS_MsgPtr m, int type);

#if defined(FLOW_APP_ACKS)
    struct flowaction_t* mAckHead = NULL;

    void enqueueAck(struct flowaction_t* act);
#endif /* defined(FLOW_APP_ACKS) */

#if defined(STATIC_ROUTES)
#include "routes.c"
    uint16_t         mFixedNextHop = TOS_BCAST_ADDR;
    struct flowint_t mFixedInterest;

    uint16_t
    getParent()
    {
        int i;

        for (i = 0; i < sizeof(links)/sizeof(struct link_t); ++i) {
            if (links[i].node == TOS_LOCAL_ADDRESS)
                return links[i].nexthop;
        }

        return TOS_BCAST_ADDR;
    }
#endif /* defined(STATIC_ROUTES) */

    struct flowaction_t*
    allocAction()
    {
        struct flowaction_t* act = call Pool.alloc();

        if (act == NULL)
            FLOW_INCSTAT(alloc_failure_cnt);

        return act;
    }

    void
    freeAction(struct flowaction_t* act)
    {
        act->type = FLOW_ACTION_NONE;
        call Pool.free(act);
    }

    /**********************************************************************
     *
     * Send queue management
     *
     **********************************************************************/
    result_t performSendMsg(struct flowaction_t* act,
                            uint16_t address, uint8_t length, TOS_MsgPtr msg);
    result_t performSendAckAction(struct flowaction_t* act);
    result_t performSendSubscribeAction(struct flowaction_t* act);
    result_t performSendPublishAction(struct flowaction_t* act);
    result_t performSendDataAction(struct flowaction_t* act);
    result_t performSendBcastAction(struct flowaction_t* act);

    task void
    processSendQueue()
    {
        mSendPending = FALSE;

        fassert(!call SendQueue.isEmpty());
        performSend(call SendQueue.pop());
    }

    result_t
    scheduleSendQueue()
    {
        if (!mSendPending && !mSending && !call SendQueue.isEmpty()) {
            if (post processSendQueue()) {
                mSendPending = TRUE;
                return SUCCESS;
            } else
                return FAIL;
        } else
            return SUCCESS;
    }

    result_t
    enqueueSendImmed(struct flowaction_t* act)
    {
        fassert(   act->type >= FLOW_ACTION_SEND_FIRST
                && act->type <= FLOW_ACTION_SEND_LAST);

        act->urgent = TRUE;

        if (call SendQueue.prepend(act) == SUCCESS)
            return scheduleSendQueue();
        else {
            freeAction(act);
            return FAIL;
        }
    }

    result_t
    enqueueSend(struct flowaction_t* act)
    {
        fassert(   act->type >= FLOW_ACTION_SEND_FIRST
                && act->type <= FLOW_ACTION_SEND_LAST);

        act->urgent = FALSE;

        if (call SendQueue.push(act) == SUCCESS)
            return scheduleSendQueue();
        else {
            freeAction(act);
            return FAIL;
        }
    }

    result_t
    performSend(struct flowaction_t* act)
    {
        result_t result = SUCCESS;

        fassert(mSending == FALSE);

#if defined(TOSMSG_HAS_ACK)
        act->msg.ack = 0;
#endif /* defined(TOSMSG_HAS_ACK) */

        switch (act->type) {
#if defined(FLOW_APP_ACKS)
            case FLOW_ACTION_SEND_ACK:
                result = performSendAckAction(act);
                break;
#endif /* defined(FLOW_APP_ACKS) */

            case FLOW_ACTION_SEND_SUBSCRIBE:
            case FLOW_ACTION_SEND_UNSUBSCRIBE:
                result = performSendSubscribeAction(act);
                break;

            case FLOW_ACTION_SEND_PUBLISH:
            case FLOW_ACTION_SEND_UNPUBLISH:
                result = performSendPublishAction(act);
                break;

            case FLOW_ACTION_SEND_DATA:
                result = performSendDataAction(act);
                break;

            case FLOW_ACTION_SEND_BCAST:
                result = performSendBcastAction(act);
                break;

            default:
                fassert(FALSE);
                break;
        }

        if (result == FAIL) {
            FLOW_INCSTAT(send_failure_cnt);
            freeAction(act);
        }

        return result;
    }

    result_t performSendMsg(struct flowaction_t* act,
                            uint16_t address, uint8_t length, TOS_MsgPtr msg)
    {
        if (act->urgent)
            return call UrgentSendMsg.send(address, length, msg);
        else
            return call SendMsg.send(address, length, msg);
    }

#if defined(FLOW_APP_ACKS)
    result_t
    performSendAckAction(struct flowaction_t* act)
    {
        result_t result;

        result = performSendMsg(act, act->u.send_ack.dest,
                                act->msg.length,
                                &(act->msg));
        if (result != FAIL) {
            struct AckMsg* ackMsg = (struct AckMsg*) act->msg.data;

            FLOW_INCSTAT(radio_send_unicast_cnt);

            signal FlowDebug.ack(act->u.send_ack.dest,
                                 ackMsg->src,
                                 ackMsg->seqno);
            mSending = TRUE;
        }

        return result;
    }
#endif /* defined(FLOW_APP_ACKS) */

    result_t
    performSendSubscribeAction(struct flowaction_t* act)
    {
        result_t result;
        uint16_t dest;

        if (act->u.send_subscribe.pubinterest == NULL)
            dest = TOS_BCAST_ADDR;
        else
            dest = act->u.send_subscribe.pubinterest->next_hop;

        result = performSendMsg(act, dest,
                                act->msg.length,
                                &(act->msg));
        if (result != FAIL) {
            if (dest == TOS_BCAST_ADDR)
                FLOW_INCSTAT(radio_send_bcast_cnt);
            else
                FLOW_INCSTAT(radio_send_unicast_cnt);

            mSending = TRUE;
        }

        return result;
    }

    result_t
    performSendPublishAction(struct flowaction_t* act)
    {
        result_t result;

        result = performSendMsg(act, TOS_BCAST_ADDR,
                                act->msg.length,
                                &(act->msg));
        if (result != FAIL) {
            FLOW_INCSTAT(radio_send_bcast_cnt);
            mSending = TRUE;
        }

        return result;
    }

    result_t
    performSendDataAction(struct flowaction_t* act)
    {
        result_t result;

        fassert(act->u.send_data.interest != NULL);

#if defined(STATIC_ROUTES)
        if (mFixedNextHop == TOS_BCAST_ADDR)
            return SUCCESS;
#endif /* !defined(STATIC_ROUTES) */

        result = performSendMsg(act, act->u.send_data.interest->next_hop,
                                act->msg.length,
                                &(act->msg));
        if (result != FAIL) {
            FLOW_INCSTAT(radio_send_unicast_cnt);
            mSending = TRUE;
        }

        return result;
    }

    result_t
    performSendBcastAction(struct flowaction_t* act)
    {
        result_t result;

        result = performSendMsg(act, TOS_BCAST_ADDR,
                                act->msg.length,
                                &(act->msg));
        if (result != FAIL) {
            FLOW_INCSTAT(radio_send_bcast_cnt);
            mSending = TRUE;
        }

        return result;
    }

    /**********************************************************************
     *
     * Receive queue management
     *
     **********************************************************************/
    result_t performReceiveAckAction(struct flowaction_t* act);
    result_t performReceiveSubscriptionAction(struct flowaction_t* act);
    result_t performReceiveDataAction(struct flowaction_t* act);
    result_t performReceiveBcastAction(struct flowaction_t* act);

    task void
    processReceiveQueue()
    {
        mReceivePending = FALSE;

        if (!call ReceiveQueue.isEmpty())
            performReceive(call ReceiveQueue.pop());
    }

    result_t
    enqueueReceive(struct flowaction_t* act)
    {
        fassert(   act->type >= FLOW_ACTION_RECEIVE_FIRST
                && act->type <= FLOW_ACTION_RECEIVE_LAST);

        if (call ReceiveQueue.push(act) == SUCCESS) {
            if (!mReceivePending && !call ReceiveQueue.isEmpty()) {
                if (post processReceiveQueue()) {
                    mReceivePending = TRUE;
                    return SUCCESS;
                } else
                    return FAIL;
            } else
                return SUCCESS;
        } else {
            freeAction(act);
            return FAIL;
        }
    }

    result_t
    performReceive(struct flowaction_t* act)
    {
        result_t result = SUCCESS;

        switch (act->type) {
#if defined(FLOW_APP_ACKS)
            case FLOW_ACTION_RECEIVE_ACK:
                result = performReceiveAckAction(act);
                break;
#endif /* defined(FLOW_APP_ACKS) */

            case FLOW_ACTION_RECEIVE_SUBSCRIPTION:
                result = performReceiveSubscriptionAction(act);
                break;

            case FLOW_ACTION_RECEIVE_DATA:
                result = performReceiveDataAction(act);
                break;

            case FLOW_ACTION_RECEIVE_BCAST:
                result = performReceiveBcastAction(act);
                break;

            default:
                fassert(FALSE);
                break;
        }

        return result;
    }

#if defined(FLOW_APP_ACKS)
    result_t
    performReceiveAckAction(struct flowaction_t* act)
    {
        struct AckMsg*        ackMsg = (struct AckMsg*) act->msg.data;
        struct flowaction_t*  sendAct;
        struct SendMsg*       sendMsg;
        struct flowaction_t** cur;

        call LinkEstimator.updateLink(ackMsg->acker, &(act->msg));

        for (cur = &mAckHead; *cur != NULL;) {
            sendAct = *cur;
            sendMsg = (struct SendMsg*) sendAct->msg.data;

            if (   ackMsg->src == sendMsg->src
                && ackMsg->seqno == sendMsg->seqno
                && ackMsg->acker == sendAct->u.send_data.interest->next_hop) {
                signal FlowDebug.receiveAck(ackMsg->acker,
                                            ackMsg->src,
                                            ackMsg->seqno);
                /* We got our ACK for this send, so remove this send action from
                 * the list of actions in needs of ACKs and do whatever else it
                 * is we need to do with it.
                 */
                *cur = (*cur)->u.send_data.next;
                sendAct->u.send_data.next = NULL;
                sendDataCont(sendAct);
            } else
                cur = &((*cur)->u.send_data.next);
        }

        freeAction(act);
        return SUCCESS;
    }
#endif /* defined(FLOW_APP_ACKS) */

    result_t
    performReceiveSubscriptionAction(struct flowaction_t* act)
    {
        struct SubscriptionMsg* sub = (struct SubscriptionMsg*) act->msg.data;

        call LinkEstimator.updateLink(sub->next_hop, &(act->msg));

        /* If we've already seen this message, ignore it. */
        if (call FlowSeqno.sawSeqno(sub->flow_id,
                                    sub->src,
                                    TRUE,
                                    sub->seqno)) {
            FLOW_INCSTAT(seqno_suppression_cnt);
#if 0
            signal FlowDebug.dropSeqno(sub->flow_id,
                                       sub->src,
                                       sub->seqno);
#endif
            goto free_and_succeed;
        }

        /* For publish and subscribe messages we do not want to squelch
         * processing immediately because although we may have already seen the
         * sequence number for this pubsub request, this request may be coming
         * from a better link! The squelching will happen naturally if we
         * already have a better next hop to go with this pubsub request. If
         * this next hop is better, we don't want to squelch anyway!
         */
        if ((sub->flags & SUBSCRIBE_SUBSCRIBE) != 0) {
            /* If we don't know where to forward data for this flow id or if
             * we're forwarding data over a more costly route, use this new
             * route and also forward the subscription request.
             */
            struct flowint_t* interest;
            metric_t          linkMetric;

            /* Our link metric is the link metric seen from us to the sender of
             * the subscription message (which is not necessarily the
             * subscriber!)  combined with the one hop (forward) link metric
             * from us to the sender (next hop).
             */
            linkMetric = combine_metrics(sub->metric,
                           call NeighborTable.forward(sub->next_hop));

            /* interest will be NULL if we don't have any recorded subscription
             * for this flow/subscriber combination.
             */
            interest = call SubscribeInterest.get(sub->flow_id,
                                                  sub->src);

            if (interest == NULL || is_better(linkMetric, interest->metric)) {
                signal FlowDebug.addSubscription(sub->flow_id,
                                                 sub->src,
                                                 sub->seqno,
                                                 sub->next_hop,
                                                 linkMetric);

                addSubscription(sub->flow_id,
                                sub->src,
                                sub->next_hop, /* next hop is the sender of the
                                                * subscription request, NOT
                                                * necessarily the source of the
                                                * request */
                                linkMetric,
                                TRUE); /* this must be an active subscription or
                                        * we wouldn't have seen it! */
            } else {
                call SubscribeInterest.refresh(interest);

                /* If the subscription message came from the node we already
                 * have installed as the next hop, update the metric in the
                 * interest struct. Otherwise we have a better metric so send it
                 * instead when we forward the request
                 */
                if (interest->next_hop == sub->next_hop)
                    interest->metric = linkMetric;
                else
                    linkMetric = interest->metric;

                signal FlowDebug.refreshSubscription(sub->flow_id,
                                                     sub->src,
                                                     sub->seqno);
            }

            if (--sub->ttl != 0) {
                FLOW_INCSTAT(sub_fwd_cnt);

                sendSubscribe(sub->flow_id,
                              sub->src,
                              sub->seqno,
                              sub->ttl,
                              sub->repeat,
                              TOS_LOCAL_ADDRESS, /* current node is the new next
                                                    hop */
                              linkMetric);
            }

            goto free_and_succeed;
        } else if ((sub->flags & SUBSCRIBE_UNSUBSCRIBE) != 0) {
            /* If we hold a subscription for the specified subscriber, delete
             * the subscription and forward the unsubscribe request.
             */
            struct flowint_t* interest;

            interest = call SubscribeInterest.get(sub->flow_id, sub->src);

            if (interest != NULL)
                removeSubscription(sub->flow_id, sub->src);

            if (--sub->ttl != 0) {
                FLOW_INCSTAT(unsub_fwd_cnt);
                sendUnsubscribe(sub->flow_id,
                                sub->src,
                                sub->seqno,
                                sub->ttl,
                                sub->repeat);
            }

            goto free_and_succeed;
        } else if ((sub->flags & SUBSCRIBE_PUBLISH) != 0) {
            /* If we don't know where to forward data for this flow id or if
             * we're forwarding data over a more costly route, use this new
             * route and also forward the publication request.
             */
            struct flowint_t* interest;
            metric_t          linkMetric;

            /* Our link metric is the link metric seen by the sender of the
             * publication (which is not necessarily the publisher!) times the
             * one hop (forward) link metric from us to the sender. */
            linkMetric = combine_metrics(sub->metric,
                           call NeighborTable.forward(sub->next_hop));

            /* interest will be NULL if we don't have any recorded publication
             * for this flow/publisher combination. */
            interest = call PublishInterest.get(sub->flow_id, sub->src);

            if (   interest == NULL
                   || is_better(linkMetric, interest->metric)) {
                signal FlowDebug.addPublication(sub->flow_id,
                                                sub->src,
                                                sub->seqno,
                                                sub->next_hop,
                                                linkMetric);

                addPublication(sub->flow_id,
                               sub->src,
                               sub->next_hop, /* next hop is the sender of the
                                               * subscription request, NOT
                                               * necessarily the source of the
                                               * request */
                               linkMetric,
                               TRUE); /* this must be an active subscription or
                                       * we wouldn't have seen it! */
            } else {
                call PublishInterest.refresh(interest);

                if (sub->next_hop == interest->next_hop)
                    interest->metric = linkMetric;
                else
                    linkMetric = interest->metric;

                signal FlowDebug.refreshPublication(sub->flow_id,
                                                    sub->src,
                                                    sub->seqno);
            }

            if (--sub->ttl != 0) {
                FLOW_INCSTAT(pub_fwd_cnt);
                sendPublish(sub->flow_id,
                            sub->src,
                            sub->seqno,
                            sub->ttl,
                            sub->repeat,
                            TOS_LOCAL_ADDRESS, /* current node is the new next
                                                  hop */
                            linkMetric);

                /* If the current node is subscribed to the flow for which we
                 * received a publication notice, we need to send a subscription
                 * request.
                 */
                interest = call SubscribeInterest.get(sub->flow_id,
                                                      TOS_LOCAL_ADDRESS);
                if (interest != NULL)
                    sendSubscribe(sub->flow_id,
                                  TOS_LOCAL_ADDRESS,
                                  call FlowSeqno.nextSeqno(sub->flow_id),
                                  sub->ttl,
                                  sub->repeat,
                                  TOS_LOCAL_ADDRESS,
                                  BEST_METRIC);
            }

            goto free_and_succeed;
        } else if ((sub->flags & SUBSCRIBE_UNPUBLISH) != 0) {
            /* If we hold a publication notice for the specified publisher,
             * delete the publication and forward the publish request.
             */
            struct flowint_t* interest;

            interest = call PublishInterest.get(sub->flow_id, sub->src);

            if (interest != NULL)
                removePublication(sub->flow_id, sub->src);

            if (--sub->ttl != 0) {
                FLOW_INCSTAT(unpub_fwd_cnt);
                sendUnpublish(sub->flow_id, sub->src,
                              sub->seqno,
                              sub->ttl,
                              sub->repeat);
            }
            goto free_and_succeed;
        }

    free_and_succeed:
        freeAction(act);
        return SUCCESS;
    }

    result_t
    performReceiveDataAction(struct flowaction_t* act)
    {
        struct SendMsg*   data = (struct SendMsg*) act->msg.data;
        struct flowint_t* interest;

        call LinkEstimator.updateLink(data->prev_hop, &(act->msg));

#if defined(FLOW_APP_ACKS)
        /* Send an ACK, even if we've already seen the data. */
        sendAck(data->src, data->seqno, data->prev_hop);
#endif /* defined(FLOW_APP_ACKS) */

#if !defined(FLOW_DONT_SUPPRESS_UNICAST)
        /* If we've already seen this message, ignore it. */
        if (call FlowSeqno.sawSeqno(data->flow_id,
                                    data->src,
                                    FALSE,
                                    data->seqno)) {
            FLOW_INCSTAT(seqno_suppression_cnt);
            signal FlowDebug.dropSeqno(data->flow_id,
                                       data->src,
                                       data->seqno);
            goto free_and_succeed;
        }
#endif /* !defined(FLOW_DONT_SUPPRESS_UNICAST) */

#if !defined(STATIC_ROUTES)
        /* If nobody is interested in this message, ignore it. */
        if (!(call SubscribeInterest.isInterested(data->flow_id)))
            goto free_and_succeed;
#endif /* !defined(STATIC_ROUTES) */

        /* If the current node is subscribed to this flow, receive the data. */
        interest = call SubscribeInterest.get(data->flow_id,
                                              TOS_LOCAL_ADDRESS);
        if (interest != NULL) {
            FLOW_INCSTAT(recv_cnt);

            signal FlowDebug.receiveData(data->flow_id,
                                         data->src,
                                         data->seqno);
            signal Flow.receive(data->flow_id,
                                act->msg.data + sizeof(*data),
                                act->msg.length - sizeof(struct SendMsg));
            goto free_and_succeed;
        } else {
            /* XXX Here we duplicate code from sendData to reuse this action
             * rather than call sendData and allocate a new action. */
            act->type = FLOW_ACTION_SEND_DATA;
#if defined(FLOW_APP_USE_ACK)
            act->u.send_data.ack_retran_count = 0;
#endif /* defined(FLOW_APP_USE_ACK) */

            data->prev_hop = TOS_LOCAL_ADDRESS;

#if defined(STATIC_ROUTES)
            act->u.send_data.interest = bestNextHop(data->flow_id);
            act->u.send_data.idx = -1;
#else /* !defined(STATIC_ROUTES) */
            if ((data->flags & SEND_MULTICAST) == 0) {
                act->u.send_data.interest = bestNextHop(data->flow_id);
                act->u.send_data.idx = -1;
            } else
                act->u.send_data.interest
                    = call SubscribeInterest.first(data->flow_id,
                                                   &(act->u.send_data.idx));
#endif /* !defined(STATIC_ROUTES) */

            if (act->u.send_data.interest == NULL)
                goto free_and_succeed;
            else {
                FLOW_INCSTAT(send_fwd_cnt);
                signal FlowDebug.fwdData(data->flow_id,
                                         data->src,
                                         data->seqno,
                                         act->u.send_data.interest->next_hop);
                return enqueueSend(act);
            }
        }

    free_and_succeed:
        freeAction(act);
        return SUCCESS;
    }

    result_t
    performReceiveBcastAction(struct flowaction_t* act)
    {
        struct BcastMsg*  data = (struct BcastMsg*) act->msg.data;
        struct flowint_t* interest;

        /* If we've already seen this message, ignore it. */
        if (call FlowSeqno.sawSeqno(data->flow_id,
                                    data->src,
                                    TRUE,
                                    data->seqno)) {
            FLOW_INCSTAT(seqno_suppression_cnt);
#if 0
            signal FlowDebug.dropSeqno(data->flow_id,
                                       data->src,
                                       data->seqno);
#endif
            goto free_and_succeed;
        }

        /* If the current node is subscribed to this flow, receive the data. */
        interest = call SubscribeInterest.get(data->flow_id,
                                              TOS_LOCAL_ADDRESS);

        if (interest != NULL) {
            FLOW_INCSTAT(recv_cnt);

            signal FlowDebug.receiveBcast(data->flow_id,
                                          data->src,
                                          data->seqno);
            signal Flow.receive(data->flow_id,
                                act->msg.data + sizeof(*data),
                                act->msg.length - sizeof(struct BcastMsg));
        }

        /* Forward the broadcast */
        if (--data->ttl != 0) {
            /* XXX Here we duplicate code from sendBcast to reuse this action
             * rather than call sendBcast and allocate a new action. */
            act->type = FLOW_ACTION_SEND_BCAST;

            data->prev_hop = TOS_LOCAL_ADDRESS;

            act->u.send_bcast.remaining = (data->repeat == 0 ? 1 : data->repeat);

            FLOW_INCSTAT(bcast_fwd_cnt);
            signal FlowDebug.fwdBcast(data->flow_id,
                                      data->src,
                                      data->seqno);
            return enqueueSend(act);
        }

    free_and_succeed:
        freeAction(act);
        return SUCCESS;
    }

    /**********************************************************************
     *
     * Subscription and publication management.
     *
     **********************************************************************/

    bool
    addSubscription(flowid_t flow_id,
                    uint16_t subscriber_id,
                    uint16_t next_hop,
                    metric_t metric,
                    bool active)
    {
        struct flowint_t interest;

        interest.flow_id = flow_id;
        interest.node_id = subscriber_id;
        interest.next_hop = next_hop;
        interest.metric = metric;
        interest.flags = 0;

        if (active)
            interest.flags |= FLOWINT_ACTIVE;

        return call SubscribeInterest.add(flow_id, subscriber_id, &interest);
    }

    void
    removeSubscription(flowid_t flow_id,
                       uint16_t subscriber_id)
    {
        if (call SubscribeInterest.remove(flow_id, subscriber_id))
            signal FlowDebug.removeSubscription(flow_id, subscriber_id);
    }

    bool
    addPublication(flowid_t flow_id,
                   uint16_t publisher_id,
                   uint16_t next_hop,
                   metric_t metric,
                   bool active)
    {
        struct flowint_t interest;

        interest.flow_id = flow_id;
        interest.node_id = publisher_id;
        interest.next_hop = next_hop;
        interest.metric = metric;
        interest.flags = 0;

        if (active)
            interest.flags |= FLOWINT_ACTIVE;

        return call PublishInterest.add(flow_id, publisher_id, &interest);
    }

    void
    removePublication(flowid_t flow_id,
                      uint16_t publisher_id)
    {
        if (call PublishInterest.remove(flow_id, publisher_id))
            signal FlowDebug.removePublication(flow_id, publisher_id);
    }

    /**********************************************************************
     *
     * Message send utilities
     *
     **********************************************************************/

#if defined(FLOW_APP_ACKS)
    result_t
    sendAck(uint16_t src,
            flowseqno_t seqno,
            uint16_t hop)
    {
        struct flowaction_t* act = allocAction();
        struct AckMsg*       msg;

        if (act == NULL)
            return FAIL;

        msg = (struct AckMsg*) act->msg.data;

        act->type = FLOW_ACTION_SEND_ACK;
        act->u.send_ack.dest = hop;
        act->msg.length = sizeof(struct AckMsg);

        msg->type = FLOW_MSG_ACK;
        msg->version = FLOW_VERSION;
        msg->src = src;
        msg->seqno = seqno;
        msg->acker = TOS_LOCAL_ADDRESS;

        return enqueueSend(act);
    }
#endif /* defined(FLOW_APP_ACKS) */

    result_t
    sendSubscribe(flowid_t flow_id,
                  uint16_t subscriber_id,
                  flowseqno_t seqno,
                  uint8_t ttl,
                  uint8_t repeat,
                  uint16_t next_hop,
                  metric_t metric)
    {
        struct flowaction_t*    act = allocAction();
        struct SubscriptionMsg* msg;

        if (act == NULL)
            return FAIL;

        msg = (struct SubscriptionMsg*) act->msg.data;

        act->type = FLOW_ACTION_SEND_SUBSCRIBE;
#if defined(FLOW_APP_USE_ACK)
        act->u.send_subscribe.ack_retran_count = 0;
#endif /* defined(FLOW_APP_USE_ACK) */

        msg->type = FLOW_MSG_SUB;
        msg->version = FLOW_VERSION;
        msg->flow_id = flow_id;
        msg->src = subscriber_id;
        msg->next_hop = TOS_LOCAL_ADDRESS;
        msg->seqno = seqno;
        msg->ttl = ttl;
        msg->repeat = repeat;
        msg->metric = metric;
        msg->flags = SUBSCRIBE_SUBSCRIBE;

        /* If we've seen a publish request for this flow, then send the
         * subscription request along the reverse path. Otherwise broadcast the
         * subscription. performSendSubscribeAction will broadcast the
         * subscription if nobody has expressed interest.
         */
        act->u.send_subscribe.remaining = (repeat == 0 ? 1 : repeat);
        act->u.send_subscribe.pubinterest
            = call PublishInterest.first(flow_id,
                                         &(act->u.send_subscribe.pubidx));
        act->u.send_subscribe.subinterest = NULL;
        act->u.send_subscribe.subidx = -1;

        act->msg.length = sizeof(struct SubscriptionMsg);

        return enqueueSend(act);
    }

    result_t
    sendUnsubscribe(flowid_t flow_id,
                    uint16_t subscriber_id,
                    flowseqno_t seqno,
                    uint8_t ttl,
                    uint8_t repeat)
    {
        struct flowaction_t*    act = allocAction();
        struct SubscriptionMsg* msg;

        if (act == NULL)
            return FAIL;

        msg = (struct SubscriptionMsg*) act->msg.data;

        act->type = FLOW_ACTION_SEND_UNSUBSCRIBE;
#if defined(FLOW_APP_USE_ACK)
        act->u.send_subscribe.ack_retran_count = 0;
#endif /* defined(FLOW_APP_USE_ACK) */

        msg->type = FLOW_MSG_SUB;
        msg->version = FLOW_VERSION;
        msg->flow_id = flow_id;
        msg->src = subscriber_id;
        msg->next_hop = TOS_LOCAL_ADDRESS;
        msg->seqno = seqno;
        msg->ttl = ttl;
        msg->repeat = repeat;
        msg->metric = WORST_METRIC;
        msg->flags = SUBSCRIBE_UNSUBSCRIBE;

        /* If we've seen a publish request for this flow, then send the
         * unsubscribe request along the reverse path. Otherwise broadcast the
         * unsubscribe. performSendSubscribeAction will broadcast the
         * unsubscribe if nobody has expressed interest.
         */
        act->u.send_subscribe.remaining = (repeat == 0 ? 1 : repeat);
        act->u.send_subscribe.pubinterest
            = call PublishInterest.first(flow_id,
                                         &(act->u.send_subscribe.pubidx));
        act->u.send_subscribe.subinterest = NULL;
        act->u.send_subscribe.subidx = -1;

        act->msg.length = sizeof(struct SubscriptionMsg);

        return enqueueSend(act);
    }

    result_t
    sendPublish(flowid_t flow_id,
                uint16_t publisher_id,
                flowseqno_t seqno,
                uint8_t ttl,
                uint8_t repeat,
                uint16_t next_hop,
                metric_t metric)
    {
        struct flowaction_t*    act = allocAction();
        struct SubscriptionMsg* msg;

        if (act == NULL)
            return FAIL;

        msg = (struct SubscriptionMsg*) act->msg.data;

        act->type = FLOW_ACTION_SEND_PUBLISH;

        msg->type = FLOW_MSG_SUB;
        msg->version = FLOW_VERSION;
        msg->flow_id = flow_id;
        msg->src = publisher_id;
        msg->next_hop = TOS_LOCAL_ADDRESS;
        msg->seqno = seqno;
        msg->ttl = ttl;
        msg->repeat = repeat;
        msg->metric = metric;
        msg->flags = SUBSCRIBE_PUBLISH;

        act->u.send_publish.remaining = (repeat == 0 ? 1 : repeat);
        act->u.send_publish.pubinterest = NULL;
        act->u.send_publish.pubidx = -1;

        act->msg.length = sizeof(struct SubscriptionMsg);

        return enqueueSend(act);
    }

    result_t
    sendUnpublish(flowid_t flow_id,
                  uint16_t publisher_id,
                  flowseqno_t seqno,
                  uint8_t ttl,
                  uint8_t repeat)
    {
        struct flowaction_t*    act = allocAction();
        struct SubscriptionMsg* msg;

        if (act == NULL)
            return FAIL;

        msg = (struct SubscriptionMsg*) act->msg.data;

        act->type = FLOW_ACTION_SEND_UNPUBLISH;

        msg->type = FLOW_MSG_SUB;
        msg->version = FLOW_VERSION;
        msg->flow_id = flow_id;
        msg->src = publisher_id;
        msg->next_hop = TOS_LOCAL_ADDRESS;
        msg->seqno = seqno;
        msg->ttl = ttl;
        msg->repeat = repeat;
        msg->metric = 0.0;
        msg->flags = SUBSCRIBE_UNPUBLISH;

        act->u.send_publish.remaining = (repeat == 0 ? 1 : repeat);
        act->u.send_publish.pubinterest = NULL;
        act->u.send_publish.pubidx = -1;

        act->msg.length = sizeof(struct SubscriptionMsg);

        return enqueueSend(act);
    }

    result_t
    resendSubscriptions()
    {
        struct flowaction_t*    act;
        struct SubscriptionMsg* msg;
        struct flowint_t*       interest;
        int                     idx;
        flowid_t                flow_id;

        interest = call SubscribeInterest.firstActiveLocal(&idx, &flow_id);
        if (interest == NULL)
            return SUCCESS;

        act = allocAction();
        if (act == NULL)
            return FAIL;

        msg = (struct SubscriptionMsg*) act->msg.data;

        act->type = FLOW_ACTION_SEND_SUBSCRIBE;

        msg->type = FLOW_MSG_SUB;
        msg->version = FLOW_VERSION;
        msg->flow_id = flow_id;
        msg->src = TOS_LOCAL_ADDRESS;
        msg->seqno = call FlowSeqno.nextSeqno(msg->flow_id);
        msg->flags = SUBSCRIBE_SUBSCRIBE;
        msg->ttl = MAX_FLOW_TTL;
        msg->repeat = mPubsubRepeat;
        msg->next_hop = TOS_LOCAL_ADDRESS;
        msg->metric = BEST_METRIC;

        /*
         * We walk through the list of all subscriptions of the current node and
         * resend them.
         */
        act->u.send_subscribe.remaining = mPubsubRepeat;
        act->u.send_subscribe.pubinterest
            = call PublishInterest.first(flow_id,
                                         &(act->u.send_subscribe.pubidx));
        act->u.send_subscribe.subinterest = interest;
        act->u.send_subscribe.subidx = idx;

        act->msg.length = sizeof(struct SubscriptionMsg);

        return enqueueSend(act);
    }

    result_t
    resendPublications()
    {
        struct flowaction_t*    act;
        struct SubscriptionMsg* msg;
        struct flowint_t*       interest;
        int                     idx;
        flowid_t                flow_id;

        interest = call PublishInterest.firstActiveLocal(&idx, &flow_id);
        if (interest == NULL)
            return SUCCESS;

        act = allocAction();
        if (act == NULL)
            return FAIL;

        msg = (struct SubscriptionMsg*) act->msg.data;

        act->type = FLOW_ACTION_SEND_PUBLISH;

        msg->type = FLOW_MSG_SUB;
        msg->version = FLOW_VERSION;
        msg->flow_id = flow_id;
        msg->src = TOS_LOCAL_ADDRESS;
        msg->seqno = call FlowSeqno.nextSeqno(msg->flow_id);
        msg->flags = SUBSCRIBE_PUBLISH;
        msg->ttl = MAX_FLOW_TTL;
        msg->repeat = mPubsubRepeat;
        msg->next_hop = TOS_LOCAL_ADDRESS;
        msg->metric = BEST_METRIC;

        /*
         * We walk through the list of all publications of the current node and
         * resend them.
         */
        act->u.send_publish.remaining = mPubsubRepeat;
        act->u.send_publish.pubinterest = interest;
        act->u.send_publish.pubidx = idx;

        act->msg.length = sizeof(struct SubscriptionMsg);

        return enqueueSend(act);
    }

    result_t
    sendData(flowid_t flow_id,
             uint16_t src,
             flowseqno_t seqno,
             uint16_t flags,
             void* data, size_t size)
    {
        struct flowaction_t* act = allocAction();
        struct SendMsg*      msg;

        if (act == NULL)
            return FAIL;

        msg = (struct SendMsg*) act->msg.data;

        act->type = FLOW_ACTION_SEND_DATA;
#if defined(FLOW_APP_USE_ACK)
        act->u.send_data.ack_retran_count = 0;
#endif /* defined(FLOW_APP_USE_ACK) */

        msg->type = FLOW_MSG_SEND;
        msg->version = FLOW_VERSION;
        msg->flow_id = flow_id;
        msg->src = src;
        msg->seqno = seqno;
        msg->flags = flags;
        msg->prev_hop = TOS_LOCAL_ADDRESS;

        memcpy(act->msg.data + sizeof(*msg), data, size);
        act->msg.length = sizeof(struct SendMsg) + size;

        fassert(act->msg.length <= TOSH_DATA_LENGTH);

#if defined(STATIC_ROUTES)
        act->u.send_data.interest = bestNextHop(flow_id);
        act->u.send_data.idx = -1;
#else /* !defined(STATIC_ROUTES) */
        if ((flags & SEND_MULTICAST) == 0) {
            act->u.send_data.interest = bestNextHop(flow_id);
            act->u.send_data.idx = -1;
        } else
            act->u.send_data.interest
                = call SubscribeInterest.first(flow_id,
                                               &(act->u.send_data.idx));
#endif /* !defined(STATIC_ROUTES) */

        if (act->u.send_data.interest == NULL) {
            freeAction(act);
            return FAIL;
        } else {
            signal FlowDebug.sendData(flow_id,
                                      act->u.send_data.interest->node_id,
                                      src,
                                      seqno,
                                      act->u.send_data.interest->next_hop,
                                      (flags & SEND_MULTICAST) != 0);
            return enqueueSend(act);
        }
    }

    result_t
    sendDataCont(struct flowaction_t* act)
    {
        struct SendMsg* data = (struct SendMsg*) act->msg.data;

        /* If act->u.send_data.idx is -1 then this was not a multicast send, so
         * we're done. Otherwise we need to find the next possible destination
         * and send to it by re-posting the action. If there are no more
         * possible destinations, we're also done :)
         */
        if (act->u.send_data.idx == -1) {
            freeAction(act);
            scheduleSendQueue();
        } else {
            act->u.send_data.interest
                = call SubscribeInterest.next(data->flow_id,
                                              &(act->u.send_data.idx));
            if (act->u.send_data.interest == NULL) {
                freeAction(act);
                scheduleSendQueue();
            } else
                enqueueSend(act);
        }

        return SUCCESS;
    }

    result_t
    sendBcast(flowid_t flow_id,
              uint16_t src,
              flowseqno_t seqno,
              uint8_t ttl,
              uint8_t repeat,
              void* data, size_t size)
    {
        struct flowaction_t* act;
        struct BcastMsg*     msg;

        if (ttl == 0)
            ttl = MAX_FLOW_TTL;

        act = allocAction();
        if (act == NULL)
            return FAIL;

        msg = (struct BcastMsg*) act->msg.data;

        act->type = FLOW_ACTION_SEND_BCAST;

        msg->type = FLOW_MSG_BCAST;
        msg->version = FLOW_VERSION;
        msg->flow_id = flow_id;
        msg->src = src;
        msg->seqno = seqno;
        msg->ttl = ttl;
        msg->repeat = repeat;
        msg->prev_hop = TOS_LOCAL_ADDRESS;

        memcpy(act->msg.data + sizeof(*msg), data, size);
        act->msg.length = sizeof(struct BcastMsg) + size;

        fassert(act->msg.length <= TOSH_DATA_LENGTH);

        act->u.send_bcast.remaining = (repeat == 0 ? 1 : repeat);

        return enqueueSend(act);
    }

    struct flowint_t*
    bestNextHop(flowid_t flow_id)
    {
#if defined(STATIC_ROUTES)
        if (mFixedNextHop == TOS_BCAST_ADDR)
            return NULL;
        else {
            mFixedInterest.flow_id = flow_id;
            return &mFixedInterest;
        }
#else /* !defined(STATIC_ROUTES) */
        struct flowint_t* bestInterest;
        struct flowint_t* curInterest;
        int               curIdx;

        bestInterest = curInterest =
            call SubscribeInterest.first(flow_id, &curIdx);
        if (curInterest == NULL)
            return NULL;

        while ((curInterest = call SubscribeInterest.next(flow_id,
                                                          &curIdx)) != NULL) {
            if (is_better(curInterest->metric, bestInterest->metric))
                bestInterest = curInterest;
        }

        return bestInterest;
#endif /* !defined(STATIC_ROUTES) */
    }

#if defined(FLOW_APP_ACKS)
    /**********************************************************************
     *
     * Application-layer acks
     *
     **********************************************************************/

    void
    enqueueAck(struct flowaction_t* act)
    {
        bool was_empty = (mAckHead == NULL);

        fassert(act->type == FLOW_ACTION_SEND_DATA);

        fassert(act->u.send_data.next == NULL);
        act->u.send_data.next = mAckHead;
        mAckHead = act;

        if (was_empty)
            call AckTimer.start(TIMER_REPEAT, FLOW_APP_ACK_TICK);
    }

    event result_t
    AckTimer.fired()
    {
        struct flowaction_t** cur;
        struct flowaction_t*  act;

        for (cur = &mAckHead; *cur != NULL;) {
            act = *cur;

            if (--(act->u.send_data.ack_ticks_remaining) == 0) {
                struct SendMsg* data = (struct SendMsg*) act->msg.data;

                *cur = (*cur)->u.send_data.next;
                act->u.send_data.next = NULL;

                FLOW_INCSTAT(app_ack_failure_cnt);

                if (act->u.send_data.ack_retran_count++ < mMaxAckRetrans) {
                    signal FlowDebug.resendData(data->flow_id, data->src, data->seqno,
                                                act->u.send_data.interest->next_hop,
                                                FALSE);

                    enqueueSendImmed(act);
                } else {
                    signal FlowDebug.dropData(data->flow_id, data->src, data->seqno,
                                              act->u.send_data.interest->next_hop,
                                              FALSE);

                    /* XXX could instead call sendDataCont here */
                    freeAction(act);
                    scheduleSendQueue();
                }
            } else
                cur = &((*cur)->u.send_data.next);
        }

        if (mAckHead == NULL)
            call AckTimer.stop();

        return SUCCESS;
    }
#endif /* defined(FLOW_APP_ACKS) */

    /**********************************************************************
     *
     * Flow interface
     *
     **********************************************************************/
    command void
    Flow.init()
    {
        memset(&mStats, 0, sizeof(mStats));

        mPubsubRepeat = FLOW_DEFAULT_PUBSUB_REPEAT;
        mRefreshTime = FLOW_DEFAULT_REFRESH_TIME;
        mPurgeFactor = FLOW_DEFAULT_PURGE_FACTOR;
        mMaxAckRetrans = FLOW_DEFAULT_MAX_ACK_RETRANS;

        mRefreshSecs = 0;

        mSending = FALSE;
        mSendPending = FALSE;
        mReceivePending = FALSE;

#if defined(FLOW_APP_ACKS)
        mAckHead = NULL;
#endif /* defined(FLOW_APP_ACKS) */

        call FlowSeqno.init();
        call NeighborTable.init();
        call SubscribeInterest.init();
        call PublishInterest.init();

#if defined(TOSMSG_HAS_ACK)
        call MacControl.enableAck();
#endif /* defined(TOSMSG_HAS_ACK) */

#if defined(STATIC_ROUTES)
        mFixedNextHop = getParent();
        mFixedInterest.node_id = 0;
        mFixedInterest.next_hop = mFixedNextHop;
        mFixedInterest.metric = 100;
        mFixedInterest.flags = 0;
        mFixedInterest.timestamp = 0;
#endif /* defined(STATIC_ROUTES) */

        mInited = TRUE;
    }

    command result_t
    Flow.subscribe(flowid_t flow_id, bool active)
    {
        signal FlowDebug.addSubscription(flow_id,
                                         TOS_LOCAL_ADDRESS,
                                         0,
                                         TOS_LOCAL_ADDRESS,
                                         BEST_METRIC);

        if (addSubscription(flow_id,
                            TOS_LOCAL_ADDRESS,
                            TOS_LOCAL_ADDRESS,
                            BEST_METRIC,
                            active)
            && active)
            return sendSubscribe(flow_id,
                                 TOS_LOCAL_ADDRESS,
                                 call FlowSeqno.nextSeqno(flow_id),
                                 MAX_FLOW_TTL,
                                 mPubsubRepeat,
                                 TOS_LOCAL_ADDRESS,
                                 BEST_METRIC);
        else
            return SUCCESS;
    }

    command void
    Flow.unsubscribe(flowid_t flow_id)
    {
        struct flowint_t* interest;

        interest = call SubscribeInterest.get(flow_id, TOS_LOCAL_ADDRESS);
        if (interest != NULL) {
            if ((interest->flags & FLOWINT_ACTIVE) != 0)
                sendUnsubscribe(flow_id,
                                TOS_LOCAL_ADDRESS,
                                call FlowSeqno.nextSeqno(flow_id),
                                MAX_FLOW_TTL,
                                mPubsubRepeat);

            removeSubscription(flow_id, TOS_LOCAL_ADDRESS);
        }
    }

    command result_t
    Flow.publish(flowid_t flow_id, bool active)
    {
        signal FlowDebug.addPublication(flow_id,
                                        TOS_LOCAL_ADDRESS,
                                        0,
                                        TOS_LOCAL_ADDRESS,
                                        BEST_METRIC);

        if(   addPublication(flow_id,
                             TOS_LOCAL_ADDRESS,
                             TOS_LOCAL_ADDRESS,
                             BEST_METRIC,
                             active)
           && active)
            return sendPublish(flow_id,
                               TOS_LOCAL_ADDRESS,
                               call FlowSeqno.nextSeqno(flow_id),
                               MAX_FLOW_TTL,
                               mPubsubRepeat,
                               TOS_LOCAL_ADDRESS,
                               BEST_METRIC);
        else
            return SUCCESS;
    }

    command void
    Flow.unpublish(flowid_t flow_id)
    {
        struct flowint_t* interest;

        interest = call PublishInterest.get(flow_id, TOS_LOCAL_ADDRESS);
        if (interest != NULL) {
            if ((interest->flags & FLOWINT_ACTIVE) == 0)
                sendUnpublish(flow_id,
                              TOS_LOCAL_ADDRESS,
                              call FlowSeqno.nextSeqno(flow_id),
                              MAX_FLOW_TTL,
                              mPubsubRepeat);

            removePublication(flow_id, TOS_LOCAL_ADDRESS);
        }
    }

    command result_t
    Flow.anycast(flowid_t flow_id, void* data, size_t size)
    {
        FLOW_INCSTAT(send_cnt);

        return sendData(flow_id,
                        TOS_LOCAL_ADDRESS,
                        call FlowSeqno.nextSeqno(flow_id),
                        0,
                        data, size);
    }

    command result_t
    Flow.multicast(flowid_t flow_id, void* data, size_t size)
    {
        FLOW_INCSTAT(send_cnt);

        return sendData(flow_id,
                        TOS_LOCAL_ADDRESS,
                        call FlowSeqno.nextSeqno(flow_id),
                        SEND_MULTICAST,
                        data, size);
    }

    command result_t
    Flow.flood(flowid_t flow_id, void* data, size_t size,
               uint8_t ttl, uint8_t repeat)
    {
        FLOW_INCSTAT(bcast_cnt);

        return sendBcast(flow_id,
                         TOS_LOCAL_ADDRESS,
                         call FlowSeqno.nextSeqno(flow_id),
                         ttl,
                         repeat,
                         data, size);
    }

    command uint16_t
    Flow.bestNextHop(flowid_t flow_id, metric_t* metric)
    {
        struct flowint_t* interest = bestNextHop(flow_id);

        if (interest == NULL) {
            *metric = WORST_METRIC;
            return TOS_BCAST_ADDR;
        } else {
            *metric = interest->metric;
            return interest->next_hop;
        }
    }

    /**********************************************************************
     *
     * FlowControl interface
     *
     **********************************************************************/

    command const struct flowstats_t*
    FlowControl.stats()
    {
        return &mStats;
    }

    command uint16_t
    FlowControl.getPubsubRepeat()
    {
        return mPubsubRepeat;
    }

    command void
    FlowControl.setPubsubRepeat(uint16_t repeat)
    {
        mPubsubRepeat = repeat;
    }

    command uint16_t
    FlowControl.getRefreshTime()
    {
        return mRefreshTime;
    }

    command void
    FlowControl.setRefreshTime(uint16_t secs)
    {
        mRefreshTime = secs;
    }

    command uint16_t
    FlowControl.getPurgeFactor()
    {
        return mPurgeFactor;
    }

    command void
    FlowControl.setPurgeFactor(uint16_t n)
    {
        mPurgeFactor = n;
    }

    command uint16_t
    FlowControl.getMaxAckRetrans()
    {
        return mMaxAckRetrans;
    }

    command void
    FlowControl.setMaxAckRetrans(uint16_t n)
    {
        mMaxAckRetrans = n;
    }

    /**********************************************************************
     *
     * FlowDebug interface
     *
     **********************************************************************/
    default event void
    FlowDebug.forwardNeighborTable(uint16_t node_id, metric_t metric)
    {
    }

    default event void
    FlowDebug.dropSeqno(flowid_t flow_id, uint16_t src, flowseqno_t seqno)
    {
    }

    default event void
    FlowDebug.sendData(flowid_t flow_id, uint16_t dest_id,
                       uint16_t src, flowseqno_t seqno,
                       uint16_t next_hop, bool multicast)
    {
    }

    default event void
    FlowDebug.fwdData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                      uint16_t next_hop)
    {
    }

    default event void
    FlowDebug.resendData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                         uint16_t next_hop, bool ack_failure)
    {
    }

    default event void
    FlowDebug.dropData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                       uint16_t next_hop, bool ack_failure)
    {
    }

    default event void
    FlowDebug.sendDoneData(flowid_t flow_id, uint16_t src, flowseqno_t seqno,
                           uint16_t next_hop)
    {
    }

    default event void
    FlowDebug.receiveData(flowid_t flow_id, uint16_t src, flowseqno_t seqno)
    {
    }

    default event void
    FlowDebug.bcast(flowid_t flow_id, uint16_t src, flowseqno_t seqno)
    {
    }

    default event void
    FlowDebug.fwdBcast(flowid_t flow_id, uint16_t src, flowseqno_t seqno)
    {
    }

    default event void
    FlowDebug.receiveBcast(flowid_t flow_id, uint16_t src, flowseqno_t seqno)
    {
    }

    default event void
    FlowDebug.ack(uint16_t sender, uint16_t src, flowseqno_t seqno)
    {
    }

    default event void
    FlowDebug.receiveAck(uint16_t acker, uint16_t src, flowseqno_t seqno)
    {
    }

    default event void
    FlowDebug.addSubscription(flowid_t flow_id,
                              uint16_t subscriber, flowseqno_t seqno,
                              uint16_t next_hop, metric_t metric)
    {
    }

    default event void
    FlowDebug.removeSubscription(flowid_t flow_id, uint16_t subscriber)
    {
    }

    default event void
    FlowDebug.refreshSubscription(flowid_t flow_id,
                                  uint16_t subscriber, flowseqno_t seqno)
    {
    }

    default event void
    FlowDebug.purgeSubscription(flowid_t flow_id, uint16_t subscriber)
    {
    }

    default event void
    FlowDebug.addPublication(flowid_t flow_id,
                             uint16_t publisher, flowseqno_t seqno,
                             uint16_t next_hop, metric_t metric)
    {
    }

    default event void
    FlowDebug.removePublication(flowid_t flow_id, uint16_t publisher)
    {
    }

    default event void
    FlowDebug.refreshPublication(flowid_t flow_id,
                                 uint16_t publisher, flowseqno_t seqno)
    {
    }

    default event void
    FlowDebug.purgePublication(flowid_t flow_id, uint16_t publisher)
    {
    }

    /**********************************************************************
     *
     * FlowInterest interface
     *
     **********************************************************************/

    event void
    SubscribeInterest.purged(flowid_t flow_id, uint16_t node_id)
    {
        signal FlowDebug.purgeSubscription(flow_id, node_id);
    }

    event void
    PublishInterest.purged(flowid_t flow_id, uint16_t node_id)
    {
        signal FlowDebug.purgePublication(flow_id, node_id);
    }

    /**********************************************************************
     *
     * SendMsg interface
     *
     **********************************************************************/
    result_t performSendDone(TOS_MsgPtr msg,
                             result_t success);
    result_t performSendDoneAckAction(struct flowaction_t* act,
                                      result_t success);
    result_t performSendDonePubSubAction(struct flowaction_t* act,
                                         result_t success);
    result_t performSendDoneDataAction(struct flowaction_t* act,
                                       result_t success);
    result_t performSendDoneBcastAction(struct flowaction_t* act,
                                        result_t success);

    event result_t
    SendMsg.sendDone(TOS_MsgPtr msg, result_t success)
    {
        return performSendDone(msg, success);
    }

    event result_t
    UrgentSendMsg.sendDone(TOS_MsgPtr msg, result_t success)
    {
        return performSendDone(msg, success);
    }

    result_t
    performSendDone(TOS_MsgPtr msg, result_t success)
    {
        struct flowaction_t* act = (struct flowaction_t*) msg;

        switch (*((uint16_t*) act->msg.data)) {
#if defined(FLOW_APP_ACKS)
            case FLOW_MSG_ACK:
                return performSendDoneAckAction(act, success);
#endif /* defined(FLOW_APP_ACKS) */

            case FLOW_MSG_SUB:
                return performSendDonePubSubAction(act, success);

            case FLOW_MSG_SEND:
                return performSendDoneDataAction(act, success);

            case FLOW_MSG_BCAST:
                return performSendDoneBcastAction(act, success);

            default:
                mSending = FALSE;
                freeAction(act);
                scheduleSendQueue();
                return SUCCESS;
        }
    }

#if defined(FLOW_APP_ACKS)
    result_t
    performSendDoneAckAction(struct flowaction_t* act, result_t success)
    {
        mSending = FALSE;

        if (success != SUCCESS) {
            FLOW_INCSTAT(sendDone_failure_cnt);
            enqueueSend(act);
            return SUCCESS;
        }

        freeAction(act);
        scheduleSendQueue();

        return SUCCESS;
    }
#endif /* defined(FLOW_APP_ACKS) */

    result_t
    performSendDonePubSubAction(struct flowaction_t* act, result_t success)
    {
        struct SubscriptionMsg* sub = (struct SubscriptionMsg*) act->msg.data;

        mSending = FALSE;

        if (success != SUCCESS) {
            FLOW_INCSTAT(sendDone_failure_cnt);
            enqueueSend(act);
            return SUCCESS;
        }

        if (   act->type == FLOW_ACTION_SEND_SUBSCRIBE
            || act->type == FLOW_ACTION_SEND_UNSUBSCRIBE) {
#if defined(TOSMSG_HAS_ACK)
            if (act->u.send_subscribe.pubinterest != NULL && !act->msg.ack) {
                FLOW_INCSTAT(radio_ack_failure_cnt);

                if (act->u.send_subscribe.ack_retran_count++
                    < mMaxAckRetrans) {
                    enqueueSend(act);
                    return SUCCESS;
                }
            }
#endif /* defined(TOSMSG_HAS_ACK) */

            if (--(act->u.send_subscribe.remaining) == 0) {
                /*
                 * We first try to find the next active publisher and send the
                 * subscription to it by re-posting the action.
                 */
                if (act->u.send_subscribe.pubinterest != NULL)
                    act->u.send_subscribe.pubinterest
                        = call PublishInterest.next(
                            sub->flow_id,
                            &(act->u.send_subscribe.pubidx));

                /*
                 * If there is not next active publisher and if we're re-sending
                 * subscriptions, i.e., act->u.send_subscribe.subinterest !=
                 * NULL, we need to find the next flow to which we are
                 * subscribed.
                 */
                if (    act->u.send_subscribe.pubinterest == NULL
                    &&  act->u.send_subscribe.subinterest != NULL ) {
                    act->u.send_subscribe.subinterest
                        = call SubscribeInterest.nextActiveLocal(
                            &(act->u.send_subscribe.subidx),
                            &(sub->flow_id));

                    if (act->u.send_subscribe.subinterest != NULL) {
                        sub->seqno = call FlowSeqno.nextSeqno(sub->flow_id);

                        act->u.send_subscribe.pubinterest
                            = call PublishInterest.first(
                                sub->flow_id,
                                &(act->u.send_subscribe.pubidx));
                    }
                }

                /*
                 * If there is no next active publisher and no current
                 * subscription to resend, we're done. Otherwise reset the
                 * remaining field and re-post the action.
                 */
                if (   act->u.send_subscribe.pubinterest == NULL
                    && act->u.send_subscribe.subinterest == NULL) {
                    freeAction(act);
                    scheduleSendQueue();
                } else {
                    act->u.send_subscribe.remaining = mPubsubRepeat;
                    enqueueSend(act);
                }
            } else
                enqueueSend(act);
        } else if (   act->type == FLOW_ACTION_SEND_PUBLISH
                   || act->type == FLOW_ACTION_SEND_UNPUBLISH) {
            if (--(act->u.send_publish.remaining) == 0) {
                /* Look for the next active publication to resend. */
                if (act->u.send_publish.pubinterest != NULL) {
                    act->u.send_publish.pubinterest
                        = call PublishInterest.nextActiveLocal(
                            &(act->u.send_publish.pubidx),
                            &(sub->flow_id));

                    if (act->u.send_publish.pubinterest != NULL)
                        sub->seqno = call FlowSeqno.nextSeqno(sub->flow_id);
                }

                /*
                 * If there is no next active publication to resend, we're
                 * done. Otherwise reset the remaining field and re-post the
                 * action.
                 */
                if (act->u.send_publish.pubinterest == NULL) {
                    freeAction(act);
                    scheduleSendQueue();
                } else {
                    act->u.send_publish.remaining = mPubsubRepeat;
                    enqueueSend(act);
                }
            } else
                enqueueSend(act);
        }

        return SUCCESS;
    }

    result_t
    performSendDoneDataAction(struct flowaction_t* act, result_t success)
    {
        struct SendMsg* data = (struct SendMsg*) act->msg.data;

        mSending = FALSE;

        if (success != SUCCESS) {
            signal FlowDebug.resendData(data->flow_id, data->src, data->seqno,
                                        act->u.send_data.interest->next_hop,
                                        FALSE);

            FLOW_INCSTAT(sendDone_failure_cnt);
            enqueueSendImmed(act);
            return SUCCESS;
        }

#if defined(TOSMSG_HAS_ACK)
        if (!act->msg.ack) {
            FLOW_INCSTAT(radio_ack_failure_cnt);

            if (act->u.send_data.ack_retran_count++ < mMaxAckRetrans) {
                signal FlowDebug.resendData(data->flow_id,
                                            data->src,
                                            data->seqno,
                                            act->u.send_data.interest->next_hop,
                                            TRUE);

                enqueueSendImmed(act);
                return SUCCESS;
            } else {
                signal FlowDebug.dropData(data->flow_id,
                                          data->src,
                                          data->seqno,
                                          act->u.send_data.interest->next_hop,
                                          TRUE);

                /* XXX could instead call sendDataCont here */
                freeAction(act);
                scheduleSendQueue();
                return SUCCESS;
            }
        }
#endif /* defined(TOSMSG_HAS_ACK) */

        signal FlowDebug.sendDoneData(data->flow_id, data->src, data->seqno,
                                      act->u.send_data.interest->next_hop);

#if defined(FLOW_APP_ACKS)
        act->u.send_data.next = NULL;
        act->u.send_data.ack_ticks_remaining = FLOW_APP_ACK_WAIT;
        enqueueAck(act);

        return SUCCESS;
#else /* !defined(FLOW_APP_ACKS) */
        return sendDataCont(act);
#endif /* !defined(FLOW_APP_ACKS) */
    }

    result_t
    performSendDoneBcastAction(struct flowaction_t* act, result_t success)
    {
        mSending = FALSE;

        if (success != SUCCESS) {
            FLOW_INCSTAT(sendDone_failure_cnt);
            enqueueSend(act);
            return SUCCESS;
        }

        if (--(act->u.send_bcast.remaining) == 0) {
            freeAction(act);
            scheduleSendQueue();
        } else
            enqueueSend(act);

        return SUCCESS;
    }

    /**********************************************************************
     *
     * ReceiveMsg interface
     *
     **********************************************************************/
    TOS_MsgPtr
    postReceive(TOS_MsgPtr m, int type)
    {
        struct flowaction_t* act = allocAction();

        if (act == NULL) {
            FLOW_INCSTAT(drop_cnt);
            return m;
        }

        memcpy(&(act->msg), m, sizeof(struct TOS_Msg));

        act->type = type;
        enqueueReceive(act);

        return m;
    }

    event TOS_MsgPtr
    ReceiveMsg.receive(TOS_MsgPtr m)
    {
        union FlowMsg* msg = (union FlowMsg*) m->data;

        if (!mInited)
            return m;

        /* We should *never* see these packets over the UART. And node 126 is
         * evil! */
        if (m->addr == TOS_UART_ADDR)
            return m;

        if (m->addr == TOS_BCAST_ADDR)
            FLOW_INCSTAT(radio_recv_bcast_cnt);
        else
            FLOW_INCSTAT(radio_recv_unicast_cnt);

        if (m->length < sizeof(struct MsgHeader))
            return m;

        if (msg->header.version != FLOW_VERSION)
            return m;

        switch (msg->header.type) {
#if defined(FLOW_APP_ACKS)
            case FLOW_MSG_ACK:
                if (m->length != sizeof(struct AckMsg))
                    return m;

                return postReceive(m, FLOW_ACTION_RECEIVE_ACK);
#endif /* defined(FLOW_APP_ACKS) */

            case FLOW_MSG_SUB:
                if (m->length != sizeof(struct SubscriptionMsg))
                    return m;

                return postReceive(m, FLOW_ACTION_RECEIVE_SUBSCRIPTION);

            case FLOW_MSG_SEND:
                if (m->length < sizeof(struct SendMsg))
                    return m;

                return postReceive(m, FLOW_ACTION_RECEIVE_DATA);

            case FLOW_MSG_BCAST:
                if (m->length < sizeof(struct BcastMsg))
                    return m;

                return postReceive(m, FLOW_ACTION_RECEIVE_BCAST);

            default:
                return m;
        }
    }

    command result_t
    StdControl.init()
    {
        call Pool.init();
        call SendQueue.init();
        call ReceiveQueue.init();
        call Flow.init();
        return SUCCESS;
    }

    command result_t
    StdControl.start()
    {
        call LinkEstimator.setBeaconInterval(FLOW_DEFAULT_BEACON_INTERVAL);
        call RefreshTimer.start(TIMER_REPEAT, 1024);
        return SUCCESS;
    }

    command result_t
    StdControl.stop()
    {
        return SUCCESS;
    }

    command nettime_t
    NetTime.get()
    {
        return call SysTime.getTime32();
    }

    event result_t
    RefreshTimer.fired()
    {
        ++mRefreshSecs;

        if (mRefreshTime != 0 && mRefreshSecs % mRefreshTime == 0 ) {
            if (mPurgeFactor != 0) {
                nettime_t deadline =
                    call NetTime.get() - mPurgeFactor*mRefreshTime*NETTIME_RATE;

                call SubscribeInterest.purge(deadline);
                call PublishInterest.purge(deadline);
            }

            resendSubscriptions();
            resendPublications();
        }

        return SUCCESS;
    }
}
