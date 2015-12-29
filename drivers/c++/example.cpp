#include "drivers/walnut.hpp"
#include <string>

using std::string;

string hello(Command &cmd) {
    return "Hello";
}

int
main() {
    Context context;

    /* Register a function to run on every message. */
    context.register_message([](Message &message) -> string {
        printf("From: %s\n", message.from.c_str());
        printf("Message: %s\n", message.line.c_str());
        return "Hey me too!";
    });

    /* Register a command. */
    context.register_command("hello", hello);

    /* Register a second command, with a lambda. */
    context.register_command("goodbye", [](Command &cmd) -> string {
        return "Goodbye: " + cmd.text();
    });

    context.run("hello");
}
