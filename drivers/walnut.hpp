#ifndef WALNUT
#define WALNUT

#include <tuple>
#include <array>
#include <sstream>
#include <iostream>
#include <stdexcept>
#include <forward_list>
#include <unordered_map>

#include <msgpack.hpp>
#include <nanomsg/nn.h>
#include <nanomsg/pubsub.h>
#include <nanomsg/pipeline.h>

using std::tuple;
using std::array;
using std::string;
using std::forward_list;
using std::stringstream;
using std::unordered_map;
using msgpack::unpacked;
using msgpack::object;

class Protocol {
    public:
        string from;
        string to;
        string tag;
        string data;

        Protocol(const char* buffer, size_t length) {
            unpack(this->result, buffer, length);
            object obj(result.get());
            array<string, 4> elements = obj.as<array<string, 4>>();

            this->from = elements[0];
            this->to = elements[1];
            this->tag = elements[2];
            this->data = elements[3];
        }

        stringstream
        pack() {
            stringstream ss;
            array<string, 4> elements { this->from, this->to, this->tag, this->data };
            msgpack::pack(ss, elements);
            return ss;
        }

    private:
        unpacked result;
};

class Message {
    public:
        string protocol;
        string from;
        string to;
        string line;
        string meta;

        Message(const char *buffer, size_t length) {
            unpack(this->result, buffer, length);
            object obj(result.get());
            array<string, 5> elements = obj.as<array<string, 5>>();

            this->protocol = elements[0];
            this->from = elements[1];
            this->to = elements[2];
            this->line = elements[3];
            this->meta = elements[4];
        }

        stringstream
        pack() {
            stringstream ss;
            array<string, 5> elements { this->protocol, this->from, this->to, this->line, this->meta };
            msgpack::pack(ss, elements);
            return ss;
        }

    private:
        unpacked result;
};

class Command {
    public:
        string message;
        forward_list<string> commands;

        Command(const char *buffer, size_t length) {
            unpack(this->result, buffer, length);
            object obj(result.get());
            tuple<string, forward_list<string>> elements = obj.as<tuple<string, forward_list<string>>>();

            this->message = std::get<0>(elements);
            this->commands = std::get<1>(elements);
        }

        stringstream
        pack() {
            stringstream ss;
            tuple<string, forward_list<string>> elements(
                this->message,
                this->commands
            );
            msgpack::pack(ss, elements);
            return ss;
        }

    private:
        unpacked result;
};

class Context {
    public:
        void register_message(string (*f)(Message &)) {
            this->messages.push_front(f);
        }

        void register_command(const char *name, string (*f)(Command &)) {
            this->commands[name] = f;
        }

        void run() {
            // Pull Socket.
            int sock = nn_socket(AF_SP, NN_SUB);
            nn_setsockopt(sock, NN_SUB, NN_SUB_SUBSCRIBE, "", 0);
            nn_connect(sock, "tcp://127.0.0.1:5005");

            while (true) {
                // Get incoming nanomsg messages.
                char *buffer = NULL;
                printf("Parsing...\n");
                int bytes = nn_recv(sock, &buffer, NN_MSG, 0);

                // Parse protocol.
                Protocol protocol(buffer, bytes);

                if(protocol.tag == "message") {
                    Message message(protocol.data.c_str(), protocol.data.size());
                    std::printf("Message\n");
                    for(auto f : this->messages) {
                        f(message);
                    }
                }

                if(protocol.tag == "command") {
                    Command command(protocol.data.c_str(), protocol.data.size());
                    std::printf("Command\n");
                    try {
                        string piece(command.commands.front());
                        string name = piece.substr(1, piece.find(' ') - 1);
                        std::printf("Position: %d\n", name.find(' '));
                        std::printf("Command name: %s\n", name.c_str());
                        this->commands.at(name)(command);
                    }
                    catch (const std::out_of_range& oor) {
                        std::printf("No command found.\n");
                    }
                }

                nn_freemsg(buffer);
            }

            nn_shutdown(sock, 0);
        }

    private:
        forward_list<string (*)(Message &)> messages;
        unordered_map<string, string (*)(Command &)> commands;
};

#endif
