/* The MIT License

   Copyright (c) 2008, by Attractive Chaos <attractor@live.co.uk>

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*/

/*
  An example:

#include "kvec.h"
int main() {
    kvec_t(int) array;
    kv_init(array);
    kv_push(int, array, 10); // append
    kv_a(int, array, 20) = 5; // dynamic
    kv_A(array, 20) = 4; // static
    kv_destroy(array);
    return 0;
}
*/

/*
  2008-09-22 (0.1.0):

    * The initial version.

*/

#ifndef AC_KVEC_H
#define AC_KVEC_H

#include <stdlib.h>

#define kv_roundup32(x) (--(x), (x)|=(x)>>1, (x)|=(x)>>2, (x)|=(x)>>4, (x)|=(x)>>8, (x)|=(x)>>16, ++(x))

#define kvec_t(type) struct { size_t n, m; type *a; }
#define kv_init(v) ((v).n = (v).m = 0, (v).a = 0)
#define kv_destroy(v) free((v).a)
#define kv_A(v, i) ((v).a[(i)])
#define kv_pop(v) ((v).a[--(v).n])
#define kv_size(v) ((v).n)
#define kv_max(v) ((v).m)

#define kv_resize(type, v, s)  ((v).m = (s), (v).a = (type*)realloc((v).a, sizeof(type) * (v).m))

#define kv_copy(type, v1, v0) do {                          \
        if ((v1).m < (v0).n) kv_resize(type, v1, (v0).n);   \
        (v1).n = (v0).n;                                    \
        memcpy((v1).a, (v0).a, sizeof(type) * (v0).n);      \
    } while (0)                                             \

#define kv_push(type, v, x) do {                                    \
        if ((v).n == (v).m) {                                       \
            (v).m = (v).m? (v).m<<1 : 2;                            \
            (v).a = (type*)realloc((v).a, sizeof(type) * (v).m);    \
        }                                                           \
        (v).a[(v).n++] = (x);                                       \
    } while (0)

#define kv_pushp(type, v) (((v).n == (v).m)?                            \
                           ((v).m = ((v).m? (v).m<<1 : 2),              \
                            (v).a = (type*)realloc((v).a, sizeof(type) * (v).m), 0) \
                           : 0), ((v).a + ((v).n++))

#define kv_a(type, v, i) (((v).m <= (size_t)(i)? \
                          ((v).m = (v).n = (i) + 1, kv_roundup32((v).m), \
                           (v).a = (type*)realloc((v).a, sizeof(type) * (v).m), 0) \
                          : (v).n <= (size_t)(i)? (v).n = (i) + 1 \
                          : 0), (v).a[(i)])

#endif





#ifndef WALNUT
#define wALNUT

#include <zmq.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>

// Program State Wrapper
// -----------------------------------------------------------------------------
typedef const char *(*callback)(void *);

typedef struct {
    const char *tag;
    callback    call;
} Callback;

typedef struct {
    kvec_t(Callback*) callbacks;
} Walnut;


// Message Type Wrappers
// -----------------------------------------------------------------------------
typedef struct {
    char *tag;
    char **args;
    char *payload;
} Message;

typedef struct {
    char *prefix;
    char *command;
    char *trail;
    char **args;
} IRCMessage;

Walnut
walnut_init() {
    Walnut walnut;
    kv_init(walnut.callbacks);
    return walnut;
}

void
walnut_register(Walnut *ctx, const char *tag, callback c) {
    Callback *call = malloc(sizeof(Callback *));
    call->tag = tag;
    call->call = c;
    kv_push(Callback*, ctx->callbacks, call);
}


// Parsers for Messages
// -----------------------------------------------------------------------------
char *
strcut(char *s, size_t len) {
    char *m = malloc(len + 1);
    strncpy(m, s, len);
    m[len] = 0;
    return m;
}


Message *
parse_payload(char *message) {
    /* Allocate Structure. */
    Message *output = malloc(sizeof(message));

    /* Maintain pointer into string for parsing. */
    char *index = strchr(message, '(');
    output->tag = strcut(message, (size_t)(index - message));

    /* Move to arguments. */
    message = index + 1;

    /* Allocate Arguments. */
    size_t argc = 0;
    char *end = strchr(message, ')');
    output->args = malloc(sizeof(const char *) * 16);

    while(message <= end) {
        /* Make sure this isn't the last argument in the list. */
        index = strchr(message, ',');

        if(index == NULL || end <= index) {
            if(end <= index) {
                break;
            }

            index = strchr(message, ')');
        }

        /* Allocate the argument. */
        char *argument = strcut(message, (size_t)(index - message));
        output->args[argc++] = argument;
        message = index + 1;
    }

    output->args[argc] = 0;

    /* Payload doesn't need allocating. Already null terminated and whole. */
    output->payload = message;

    return output;
}

Message *
free_payload(Message *m) {
    size_t argc = 0;

    while(m->args[argc] != NULL) {
        free(m->args[argc++]);
    }

    printf("Freeing Payload\n");
    free(m->args);
    free(m->tag);
    free(m);

    return NULL;
}

IRCMessage *
parse_irc(Message *message) {
    /* Allocate Structure. */
    IRCMessage *irc_m = malloc(sizeof(IRCMessage));

    char *payload = message->payload;
    char *index   = payload;

    /* Check for a prefix. */
    if(payload[0] == ':') {
        index = strchr(payload, ' ');
        irc_m->prefix = strcut(payload + 1, (size_t)(index - payload));
        payload = index + 1;
    }

    /* Check if there's any trailing. */
    char *trail = strstr(payload, " :");
    if(trail != NULL) {
        irc_m->trail = strcut(trail + 2, strlen(trail));
    } else {
        trail = payload + strlen(payload);
    }

    /* Parse arguments out. */
    size_t argc = 0;
    irc_m->args = malloc(sizeof(const char *) * 16);

    while(payload <= trail) {
        /* Make sure this isn't the last argument in the list. */
        index = strchr(payload, ' ');

        if(index == NULL || trail <= index) {
            break;
        }

        /* Allocate the argument. */
        char *argument = strcut(payload, (size_t)(index - payload));
        irc_m->args[argc++] = argument;
        payload = index + 1;
    }

    return irc_m;
}

IRCMessage *
free_irc(IRCMessage *m) {
    printf("Freeing IRC\n");
    if(m->trail != NULL) free(m->trail);
    if(m->prefix != NULL) free(m->prefix);
    free(m);

    return NULL;
}

void
walnut_run(Walnut *walnut) {
    /* Open and connect sockets to the core. */
    void *context = zmq_ctx_new();
    void *sub     = zmq_socket(context, ZMQ_SUB);
    void *req     = zmq_socket(context, ZMQ_REQ);

    zmq_connect(req, "tcp://0.0.0.0:9891");
    zmq_connect(sub, "tcp://0.0.0.0:9890");
    zmq_setsockopt(sub, ZMQ_SUBSCRIBE, "IRC:PRIVMSG", strlen("IRC:PRIVMSG"));
    zmq_setsockopt(sub, ZMQ_SUBSCRIBE, "IRC:PING", strlen("IRC:PING"));
    zmq_setsockopt(sub, ZMQ_SUBSCRIBE, "IPC:CALL", strlen("IRC:CALL"));

    /* Buffer used for the currently handled message. */
    char buffer[513];

    /* Plugin event loop. */
    while(1) {
        /* Receive message from the Publisher. */
        size_t size = zmq_recv(sub, buffer, 512, 0);
        assert(size != -1);

        if(size > 512) {
            size = 512;
        }

        buffer[size] = '\0';
        Message *m = parse_payload(buffer);

        /* Run plugins listening for IRC namespace commands. */
        if(strncmp(m->tag, "IRC:", 4) == 0) {
            IRCMessage *irc_m = parse_irc(m);

            size_t i = 0;
            for(i = 0; i < kv_size(walnut->callbacks); ++i) {
                Callback *call = kv_A(walnut->callbacks, i);
                if(strncmp(m->tag, call->tag, strlen(call->tag)) == 0) {
                    char buf[512];
                    const char *output = (*call->call)(irc_m);

                    if(output == NULL) {
                        continue;
                    }

                    sprintf(buf, "WAR:FORWARD(%s,%s)%s", "bruh", m->args[0], output);
                    zmq_send(req, buf, strlen(buf), 0);
                    zmq_recv(sub, buf, 8, 0);
                }
            }

            free_irc(irc_m);
        }

        free_payload(m);
    }

    zmq_close(req);
    zmq_ctx_destroy(context);
}

#endif
