package main

import "walnut"

func main() {
    context := MakeContext()

    context.RegisterMessage(func (m Message) string {
        return "Really!"
    })

    context.RegisterCommand("hello", func (m Command) string {
        return "Really?"
    })

    context.Run("hello2")
}

