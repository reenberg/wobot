#define EVENT_QUEUE_SIZE 10

typedef unsigned char byte;
typedef unsigned int word;

enum event_type { 
  TIMER_EVENT,
  DUMMY_EVENT,
  FCALL_EVENT,
  FARGCALL_EVENT
};

struct timer_event_data {
  byte data;
};

struct dummy_event_data {
  word data;
};

struct fcall_event_data {
  void (*func)(void);
};

struct fargcall_event_data {
  void (*func)(void*);
  void* data;
};

union event_data {
  struct timer_event_data timer_event_data;
  struct dummy_event_data dummy_event_data;
  struct fcall_event_data fcall_event_data;
  struct fargcall_event_data fargcall_event_data;
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
  return queue->n == 0;
}

struct event event_queue_pop(struct event_queue* queue) {
  queue->n--;
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
  return event_queue_pop(&event_queue);
}

unsigned char event_available() {
  return !event_queue_empty(&event_queue);
}

void queue_funcall(void (*func)()) {
  struct event event;
  struct fcall_event_data data;
  data.func = func;
  event.type = FCALL_EVENT;
  event.data.fcall_event_data = data;
  push_event(event);
}

void queue_callback(void (*func)(void*), void* cb_data) {
  struct event event;
  struct fargcall_event_data data;
  data.func = func;
  data.data = cb_data;
  event.type = FARGCALL_EVENT;
  event.data.fargcall_event_data = data;
  push_event(event);
}
