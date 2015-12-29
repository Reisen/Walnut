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

        Protocol() {};
        Protocol(const char* buffer, size_t length) {
            unpack(this->result, buffer, length);
            object obj(result.get());
            auto elements = obj.as<array<string, 4>>();

            this->from = elements[0];
            this->to = elements[1];
            this->tag = elements[2];
            this->data = elements[3];
        }

        stringstream
        pack() {
            stringstream ss;
            array<string, 4> elements {
                this->from,
                this->to,
                this->tag,
                this->data
            };

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

        Message() {};
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
            array<string, 5> elements {
                this->protocol,
                this->from,
                this->to,
                this->line,
                this->meta
            };

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

        Command() {};
        Command(const char *buffer, size_t length) {
            unpack(this->result, buffer, length);
            object obj(result.get());
            auto elements = obj.as<tuple<string, forward_list<string>>>();

            this->message = std::get<0>(elements);
            this->commands = std::get<1>(elements);
        }

        stringstream
        pack() const {
            stringstream ss;
            tuple<string, forward_list<string>> elements(
                this->message,
                this->commands
            );
            msgpack::pack(ss, elements);
            return ss;
        }

        string
        command() const {
            string piece(this->commands.front());
            string name = piece.substr(1, piece.find(' ') - 1);
            name.erase(name.find_last_not_of(" \n\r\t") + 1);
            return name;
        }

        string
        text() const {
            string piece(this->commands.front());
            if(piece.find(' ') != -1) {
                string line = piece.substr(piece.find(' '));
                line.erase(line.find_last_not_of(" \n\r\t") + 1);
                return line.substr(line.find_first_not_of(" \n\r\t"));
            }

            return "";
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

        void run(string name) {
            /* Pull Socket. */
            int pull = nn_socket(AF_SP, NN_SUB);
            nn_setsockopt(pull, NN_SUB, NN_SUB_SUBSCRIBE, "", 0);
            nn_connect(pull, "tcp://127.0.0.1:5005");

            /* Push Socket. */
            int push = nn_socket(AF_SP, NN_PUSH);
            nn_connect(push, "tcp://127.0.0.1:5006");

            while (true) {
                // Get incoming nanomsg messages.
                char *buffer = NULL;
                int bytes = nn_recv(pull, &buffer, NN_MSG, 0);

                // Parse protocol.
                Protocol protocol(buffer, bytes);

                // Ignore messages not sent to us.
                if(protocol.to != "*" && protocol.to != name) {
                    continue;
                }

                if(protocol.tag == "message") {
                    Message message(protocol.data.c_str(), protocol.data.size());

                    for(auto f : this->messages) {
                        auto result = f(message);
                        if(result != "") {
                            Message m_response;
                            m_response.protocol = message.protocol;
                            m_response.from = message.to;
                            m_response.to = message.from;
                            m_response.line = result;
                            m_response.meta = message.meta;

                            Protocol response;
                            response.from = name;
                            response.to = ("protocol." + message.protocol);
                            response.tag = "message";
                            response.data = m_response.pack().str();

                            auto output = response.pack().str();
                            nn_send(push, output.c_str(), output.size(), 0);
                        }
                    }
                }

                if(protocol.tag == "command") {
                    Command command(protocol.data.c_str(), protocol.data.size());
                    Message message(command.message.c_str(), command.message.size());

                    try {
                        auto result = this->commands.at(command.command())(command);
                        if(result != "") {
                            command.commands.pop_front();
                            command.commands.push_front(result);

                            Protocol response;
                            response.from = name;
                            response.to = protocol.from;
                            response.tag = "response";
                            response.data = command.pack().str();

                            auto output = response.pack().str();
                            nn_send(push, output.c_str(), output.size(), 0);
                        }
                    }
                    catch (const std::out_of_range& oor) {
                        std::printf("No command found.\n");
                    }
                }

                nn_freemsg(buffer);
            }

            nn_shutdown(pull, 0);
            nn_shutdown(push, 0);
        }

    private:
        forward_list<string (*)(Message &)> messages;
        unordered_map<string, string (*)(Command &)> commands;
};

#endif
