#ifndef WALNUT
#define wALNUT

#include <zmq.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>

// Message Type Wrappers
// -----------------------------------------------------------------------------
typedef struct {
    void   *callbacks[512];
    size_t  callbackn;
} Walnut;


typedef struct {
    char *tag;
    char **args;
    char *payload;
} Message;


Walnut
walnut_init() {
    Walnut walnut = {
        {0},
        0
    };
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
    output->args = malloc(sizeof(const char *) * 8);

    while(message <= end) {
        /* Make sure this isn't the last argument in the list. */
        index = strchr(message, ',');

        if(index == 0 || end <= index) {
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

    free(m->tag);
    free(m);

    return (Message *)0;
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
        free_payload(m);
    }

    zmq_close(req);
    zmq_ctx_destroy(context);
}

#endif
