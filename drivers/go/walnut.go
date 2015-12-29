package main

import (
    //"fmt"
    //"github.com/gdamore/mangos"
    //"github.com/gdamore/mangos/protocol/push"
    //"github.com/gdamore/mangos/transport/tcp"
)

type Message struct {
}

type Command struct {
}

type Context struct {
    messages []Message
    commands map[string]Command
}

func register_message(f func(Message)) {
}

func main() {
}
