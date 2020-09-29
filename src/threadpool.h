#ifndef __THREADPOOL_H__
#define __THREADPOOL_H__

#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define TP_DONT_STOP (1)
#define TP_IMMEDIATE (2)
#define TP_GRACEFUL  (3)

typedef void   (*tp_task_fn_t)  (void*);
typedef void * (*tp_thread_fn_t)(void*);

typedef struct tp_task {
    tp_task_fn_t    fn;
    void           *arg;
    struct tp_task *prev, *next;
} tp_task_t;

typedef struct {
    tp_task_t    *head, *tail;
    unsigned int  len;
} tp_queue_t;

typedef struct {
    pthread_mutex_t  mutex;
    pthread_cond_t   cond;
    pthread_t       *threads;
    tp_queue_t       queue;
    int              stop_mode;
    int              n_started;
    int              n_running;
} tp_t;


tp_t * tp_make(int n_workers);
void   tp_free(tp_t *tp);
void   tp_stop(tp_t *tp, int stop_mode);
void   tp_add_task(tp_t *tp, tp_task_fn_t fn, void *arg);
void   tp_wait(tp_t *tp);
int    tp_running(tp_t *tp);

#endif

#ifdef THREADPOOL_IMPLEMENTATION

static tp_task_t * tp_task_make(tp_task_fn_t fn, void *arg) {
    tp_task_t *task;

    task = (tp_task_t*)malloc(sizeof(*task));

    task->fn   = fn;
    task->arg  = arg;
    task->prev = task->next = NULL;

    return task;
}

static void tp_task_free(tp_task_t *task) { free(task); }


static tp_queue_t tp_queue_make(void) {
    tp_queue_t queue;

    memset(&queue, 0, sizeof(queue));

    return queue;
}

static void tp_queue_en(tp_queue_t *queue, tp_task_fn_t fn, void *arg) {
    tp_task_t *task;

    task = tp_task_make(fn, arg);

    if (queue->head != NULL) {
        task->next        = queue->head;
        queue->head->prev = task;
        queue->head       = task;
    } else {
        queue->head = queue->tail = task;
    }

    queue->len += 1;
}

static tp_task_t * tp_queue_de(tp_queue_t *queue) {
    tp_task_t *tail;

    if (queue->tail == NULL) {
        return NULL;
    }

    tail = queue->tail;

    if (tail->prev != NULL) {
        tail->prev->next = NULL;
        queue->tail      = tail->prev;
    } else {
        queue->head = queue->tail = NULL;
    }

    tail->prev = tail->next = NULL;

    queue->len -= 1;

    return tail;
}

static void tp_queue_clear(tp_queue_t *queue) {
    tp_task_t *task;

    while ((task = tp_queue_de(queue))) {
        tp_task_free(task);
    }
}

static void _tp_thread_task(void *_tp) {
    tp_t         *tp;
    tp_task_t    *task;
    tp_task_fn_t  fn;
    void         *arg;

    tp = (tp_t*)_tp;

    for (;;) {
        /* Lock must be taken to wait on conditional variable. */

        pthread_mutex_lock(&tp->mutex);

        /*
         * Wait on condition variable, check for spurious wakeups.
         * When returning from pthread_cond_wait(), we own the lock.
         */
        while (tp->queue.len == 0
        &&     tp->stop_mode == TP_DONT_STOP) {
            pthread_cond_wait(&tp->cond, &tp->mutex);
        }

        if (tp->stop_mode == TP_IMMEDIATE
        ||  (tp->stop_mode == TP_GRACEFUL && tp->queue.len == 0)) {
            break;
        }

        /* Grab our task. */
        task = tp_queue_de(&tp->queue);

        if (task != NULL) {
            /* Get to work. */
            fn  = task->fn;
            arg = task->arg;

            tp->n_running += 1;

            tp_task_free(task);

            pthread_mutex_unlock(&tp->mutex);

            fn(arg);

            pthread_mutex_lock(&tp->mutex);
            tp->n_running -= 1;
            pthread_mutex_unlock(&tp->mutex);
        }
    }

    pthread_mutex_unlock(&tp->mutex);

    pthread_exit(NULL);
}

tp_t * tp_make(int n_workers) {
    tp_t *tp;
    int   i;

    if (n_workers <= 0) { n_workers = 1; }

    tp = (tp_t*)malloc(sizeof(*tp));

    pthread_mutex_init(&tp->mutex, NULL);
    pthread_cond_init(&tp->cond, NULL);
    tp->threads   = (pthread_t*)malloc(n_workers * sizeof(pthread_t));
    tp->queue     = tp_queue_make();
    tp->stop_mode = TP_DONT_STOP;
    tp->n_started = 0;
    tp->n_running = 0;

    for (i = 0; i < n_workers; i += 1) {
        pthread_create(tp->threads + i, NULL, (void*(*)(void*))_tp_thread_task, tp);
        tp->n_started += 1;
    }

    return tp;
}

void tp_free(tp_t *tp) {
    tp_queue_clear(&tp->queue);
    free(tp->threads);
    pthread_cond_destroy(&tp->cond);
    pthread_mutex_destroy(&tp->mutex);
    free(tp);
}

void tp_stop(tp_t *tp, int stop_mode) {
    int   i;
    void *ignore;

    if (stop_mode != TP_DONT_STOP
    &&  stop_mode != TP_IMMEDIATE
    &&  stop_mode != TP_GRACEFUL) {
        return;
    }

    if (stop_mode     == TP_DONT_STOP
    ||  tp->stop_mode != TP_DONT_STOP) {
        pthread_mutex_unlock(&tp->mutex);
        return;
    }

    tp->stop_mode = stop_mode;

    pthread_cond_broadcast(&tp->cond);
    pthread_mutex_unlock(&tp->mutex);

    for (i = 0; i < tp->n_started; i += 1) {
        pthread_join(tp->threads[i], &ignore);
    }
}

void tp_add_task(tp_t *tp, tp_task_fn_t fn, void *arg) {
    pthread_mutex_lock(&tp->mutex);

    if (tp->stop_mode == TP_DONT_STOP) {
        tp_queue_en(&tp->queue, fn, arg);
        pthread_cond_signal(&tp->cond);
    }

    pthread_mutex_unlock(&tp->mutex);
}

void tp_wait(tp_t *tp) {
    struct timespec ts;

    ts.tv_sec  = 0;
    ts.tv_nsec = 100000; /* 100 microseconds */

    for (;;) {
        pthread_mutex_lock(&tp->mutex);

        if (tp->queue.len == 0
        &&  tp->n_running == 0) {
            pthread_mutex_unlock(&tp->mutex);
            break;
        }

        pthread_mutex_unlock(&tp->mutex);
        nanosleep(&ts, NULL);
    }
}

int tp_running(tp_t *tp) {
    int r;

    pthread_mutex_lock(&tp->mutex);
    r = tp->n_running;
    pthread_mutex_unlock(&tp->mutex);

    return r;
}

#endif
