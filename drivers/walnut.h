#include <stdio.h>
#include <stdlib.h>
#include <msgpack.h>
#include <nanomsg/nn.h>
#include <nanomsg/pubsub.h>
#include <nanomsg/pipeline.h>

#define UNPACKER(T, handler) \
    T * \
    unpack_to_##T(const char *message, int bytes) { \
        msgpack_unpacker unp; \
        msgpack_unpacked und; \
        msgpack_unpack_return ret; \
        msgpack_unpacker_init(&unp, 100); \
        if(msgpack_unpacker_buffer_capacity(&unp) < bytes) { \
            msgpack_unpacker_reserve_buffer(&unp, bytes); \
        } \
        memcpy(msgpack_unpacker_buffer(&unp), message, bytes); \
        msgpack_unpacker_buffer_consumed(&unp, bytes); \
        msgpack_unpacked_init(&und); \
        T *output = (T *)malloc(sizeof(T)); \
        switch(ret = msgpack_unpacker_next(&unp, &und)) {\
            case MSGPACK_UNPACK_SUCCESS: \
                { \
                    msgpack_object obj = und.data; \
                    if(obj.type != MSGPACK_OBJECT_ARRAY) { \
                        break; \
                    } \
                    handler(output, &obj); \
                } \
                break; \
            default: \
                break; \
        } \
        msgpack_unpacked_destroy(&und); \
        msgpack_unpacker_destroy(&unp); \
        return output; \
    }


/* Define Protocol. */
/* ------------------------------------------------------------------------- */
typedef struct {
    char *from;
    char *to;
    char *tag;
    char *data;
    int data_size;
} Protocol;

void
unpack_protocol(Protocol *protocol, msgpack_object *obj) {
    msgpack_object_array arr = obj->via.array;
    int from_size = arr.ptr[0].via.str.size;
    int to_size = arr.ptr[1].via.str.size;
    int tag_size = arr.ptr[2].via.str.size;
    int data_size = arr.ptr[3].via.bin.size;

    protocol->from = (char *)malloc(from_size);
    protocol->to = (char *)malloc(to_size);
    protocol->tag = (char *)malloc(tag_size);
    protocol->data = (char *)malloc(data_size);

    memcpy(protocol->from, arr.ptr[0].via.str.ptr, from_size);
    memcpy(protocol->to, arr.ptr[1].via.str.ptr, to_size);
    memcpy(protocol->tag, arr.ptr[2].via.str.ptr, tag_size);
    memcpy(protocol->data, arr.ptr[3].via.bin.ptr, data_size);

    protocol->from[from_size] = 0;
    protocol->to[to_size] = 0;
    protocol->tag[tag_size] = 0;
    protocol->data_size = data_size;
}

void
free_protocol(Protocol *protocol) {
    free(protocol->from);
    free(protocol->to);
    free(protocol->tag);
    free(protocol->data);
    free(protocol);
}

UNPACKER(Protocol, unpack_protocol)
/* ------------------------------------------------------------------------- */





/* Define Message. */
/* ------------------------------------------------------------------------- */
typedef struct {
    char *protocol;
    char *from;
    char *to;
    char *line;
    char *meta;
} Message;

void
unpack_message(Message *message, msgpack_object *obj) {
    msgpack_object_array arr = obj->via.array;
    int protocol_size = arr.ptr[0].via.str.size;
    int from_size = arr.ptr[1].via.bin.size;
    int to_size = arr.ptr[2].via.bin.size;
    int line_size = arr.ptr[3].via.bin.size;
    int meta_size = arr.ptr[4].via.bin.size;

    message->protocol = (char *)malloc(protocol_size);
    message->from = (char *)malloc(from_size);
    message->to = (char *)malloc(to_size);
    message->line = (char *)malloc(line_size);
    message->meta = (char *)malloc(meta_size);

    memcpy(message->protocol, arr.ptr[0].via.str.ptr, protocol_size);
    memcpy(message->from, arr.ptr[1].via.bin.ptr, from_size);
    memcpy(message->to, arr.ptr[2].via.bin.ptr, to_size);
    memcpy(message->line, arr.ptr[3].via.bin.ptr, line_size);
    memcpy(message->meta, arr.ptr[4].via.bin.ptr, meta_size);

    message->protocol[protocol_size] = 0;
    message->line[line_size] = 0;
    message->from[from_size] = 0;
    message->to[to_size] = 0;
}

void
free_message(Message *message) {
    free(message->protocol);
    free(message->from);
    free(message->to);
    free(message->line);
    free(message->meta);
    free(message);
}

UNPACKER(Message, unpack_message)
/* ------------------------------------------------------------------------- */





/* Define Command. */
/* ------------------------------------------------------------------------- */
typedef struct {
    char *message;
    char **commands;
} Cmd;


void
unpack_command(Cmd *command, msgpack_object *obj) {
    msgpack_object_array arr = obj->via.array;
    int message_size = arr.ptr[0].via.bin.size;
    int list_size = arr.ptr[1].via.array.size;

    command->message = (char *)malloc(message_size);
    command->commands = (char**)malloc((list_size * sizeof(char*)) + 1);

    for(int i = 0; i < list_size; ++i) {
        command->commands[i] = (char*)malloc(arr.ptr[1].via.array.ptr[i].via.str.size);
        memcpy(command->commands[i], arr.ptr[1].via.array.ptr[i].via.str.ptr, arr.ptr[1].via.array.ptr[i].via.str.size);
        command->commands[i][arr.ptr[1].via.array.ptr[i].via.str.size] = 0;
    }

    command->commands[list_size] = 0;
}

void
free_command(Cmd *command) {
    for(int i = 0;; ++i) {
        if(command->commands[i] == 0)
            return;

        free(command->commands[i]);
    }

    free(command->message);
    free(command);
}

UNPACKER(Cmd, unpack_command)
/* ------------------------------------------------------------------------- */





/* Define Runtime. */
/* ------------------------------------------------------------------------- */
typedef const char *(*Callback)(Message *m);

typedef struct {
    const char *name;
    Callback callback;
} Command;

typedef struct {
    int count;
    Command *callbacks;
} Context;

Context *
init_context() {
    Context *context = (Context *)malloc(sizeof(Context));
    context->count = 0;
    context->callbacks = (Command *)malloc(sizeof(Command) * 128);
    return context;
}

void
register_command(Context *context, const char *command, Callback callback) {
    Command new_command = {command, callback};
    context->callbacks[context->count] = new_command;
}




void
run_context(Context *context) {
    // Pull Socket.
    int sock = nn_socket(AF_SP, NN_SUB);
    nn_setsockopt(sock, NN_SUB, NN_SUB_SUBSCRIBE, "", 0);
    nn_connect(sock, "tcp://127.0.0.1:5005");

    // Push Socket.
    // ...

    // Messagepack
    while (1) {
        // Get incoming nanomsg messages.
        char *buffer = NULL;
        int bytes = nn_recv(sock, &buffer, NN_MSG, 0);

        Protocol *protocol = unpack_to_Protocol(buffer, bytes);

        // Incoming messages from protocols.
        if(protocol && strcmp(protocol->tag, "message") == 0) {
            Message *message = unpack_to_Message(protocol->data, protocol->data_size);

            if(message) {
                printf("Message:\n");
                printf(" Protocol: %s\n", message->protocol);
                printf(" Message: %s\n", message->line);
                printf("\n");
                free_message(message);
            }

            printf("Protocol:\n");
            printf(" From: %s\n", protocol->from);
            printf(" To: %s\n", protocol->to);
            printf(" Tag: %s\n", protocol->tag);
            printf("\n");
        }

        // Incoming messages from commands.
        if(protocol && strcmp(protocol->tag, "command") == 0) {
            Cmd *command = unpack_to_Cmd(protocol->data, protocol->data_size);

            if(command) {
                printf("Command:\n");
                printf(" Commands:\n");
                for(int i = 0;; ++i) {
                    if(command->commands[i] == 0)
                        break;

                    printf("  ->: %s\n", command->commands[i]);
                }
                printf("\n");

                free_command(command);
            }
        }

        free_protocol(protocol);
        nn_freemsg(buffer);
    }

    // Shutdown.
    nn_shutdown(sock, 0);
}
