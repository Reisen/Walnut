package main

import "walnut"

func main() {
    context := walnut.MakeContext()

    context.RegisterMessage(func (m walnut.Message) string {
        return "Really!"
    })

    context.RegisterCommand("hello", func (m walnut.Command) string {
        return "Really?"
    })

    context.Run("hello2")
}

