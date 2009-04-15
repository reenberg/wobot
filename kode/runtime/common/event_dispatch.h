#define EVENT_QUEUE_SIZE 10

typedef unsigned char byte;
typedef unsigned int word;

enum event_type { 
  TIMER_EVENT, 
  DUMMY_EVENT 
};

struct timer_event_data {
  byte data;
};

struct dummy_event_data {
  word data;
};

union event_data {
  struct timer_event_data timer_event_data;
  struct dummy_event_data dummy_event_data;
};

struct event {
  enum event_type type;
  union event_data data;
};

struct event_queue {
  struct event events[EVENT_QUEUE_SIZE];
  unsigned char p;
  unsigned char n;
};

void event_queue_push(struct event_queue* queue, struct event event) {
  if (queue->n < EVENT_QUEUE_SIZE) {
    queue->events[(queue->p + queue->n++) % EVENT_QUEUE_SIZE] = event;
  }
}

unsigned char event_queue_empty(struct event_queue* queue) {
  return queue->n == queue->p;
}

struct event event_queue_pop(struct event_queue* queue) {
  return queue->events[queue->p++ % EVENT_QUEUE_SIZE];
}

struct event_queue event_queue;

void setup_event_queue() {
  event_queue.p = 0;
  event_queue.n = 0;
}

void push_event(struct event event) {
  event_queue_push(&event_queue, event);
}

struct event pop_event() {
  event_queue_pop(&event_queue);
}
