package main

import (
    "fmt"
    "github.com/gdamore/mangos"
    "github.com/gdamore/mangos/protocol/push"
    "github.com/gdamore/mangos/transport/tcp"
    "github.com/gdamore/mangos/protocol/sub"
    "gopkg.in/vmihailenco/msgpack.v2"
)

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */
type Message struct {
    Protocol string
    From string
    To string
    Line string
    Meta string
}

/* -------------------------------------------------------------------------- */
type Command struct {
    Message []byte
    Commands []string
}

/* -------------------------------------------------------------------------- */
type Context struct {
    messages []func(Message)string
    commands map[string]func(Command)string
}

func make_context() Context {
    return Context {
        messages: make([]func(Message)string, 0),
        commands: make(map[string]func(Command)string),
    }
}

func (c Context) register_message(f func(Message) string) {
    c.messages = append(c.messages, f)
}

func (c Context) register_command(cmd string, f func(Command) string) {
    c.commands[cmd] = f
}

func (c Context) run(name string) {
    push, _ := push.NewSocket()
    push.AddTransport(tcp.NewTransport())
    push.Listen("tcp://127.0.0.1:5006")

    pull, _ := sub.NewSocket()
    pull.AddTransport(tcp.NewTransport())
    pull.Dial("tcp://127.0.0.1:5005")
    pull.SetOption(mangos.OptionSubscribe, []byte(""))

    for {
        var protocol [4][]byte
        bytes, _ := pull.Recv()
        msgpack.Unmarshal(bytes, &protocol)

        if string(protocol[1]) != "*" && string(protocol[1]) != name {
            continue
        }

        if string(protocol[2]) == "message" {
            fmt.Printf("Message\n")
            var message [5][]byte
            err := msgpack.Unmarshal(protocol[3], &message)
            if err != nil {
                fmt.Printf("Fuck\n")
            } else {
                fmt.Printf("Nice\n")
            }

            fmt.Printf("Length: %d\n", len(c.messages))
            for _, f := range c.messages {
                fmt.Printf("Calling\n")
                result := f(Message {
                    string(message[0]),
                    string(message[1]),
                    string(message[2]),
                    string(message[3]),
                    string(message[4]),
                })

                fmt.Printf("%s\n", result)
            }
        }

        if string(protocol[2]) == "command" {
        }
    }
}

/* -------------------------------------------------------------------------- */
func main() {
    context := make_context()

    context.register_message(func (m Message) string {
        return "Really!"
    })

    context.register_command("hello", func (m Command) string {
        return "Really!"
    })

    context.run("hello2")
}
