import re
import zmq
import collections
import functools


hooks = collections.defaultdict(list)


# Message Type Wrappers
# ------------------------------------------------------------------------------
class Message:
    def __init__(self, tag, args, payload):
        self.tag     = tag
        self.args    = args
        self.payload = payload


class IRCMessage:
    def __init__(self, message):
        prefix, command, args = parse_irc(message.payload)
        self.prefix  = prefix
        self.command = command
        self.args    = args

        # Additional Useful Members. Not sure about this.
        if '!' in self.prefix:
            self.nick = self.prefix.split('!')[0]


class IPCMessage:
    def __init__(self, message):
        target, args = message.payload.split(' ', 1)
        self.target = target
        self.args   = args


def hook(event):
    def hook_method(f):
        @functools.wraps(f)
        def handler(*args, **kwargs):
            return f(*args, **kwargs)

        hooks[event].append(handler)
        return handler

    return hook_method


# Parsers for Messages.
# ------------------------------------------------------------------------------
def parse_payload(msg):
    tag, arg = msg.split('(', 1)
    arg, pay = arg.split(')', 1)
    arg      = arg.split(',')
    return (tag, arg, pay)


def parse_irc(msg):
    msg = msg.strip()

    # Defaults for prefix and trailing.
    prefix, trailing = '', ''

    # Find prefix if applicable.
    if msg[0] == ':':
        prefix, msg = msg[1:].split(' ', 1)

    # Find trailing and args.
    if msg.find(' :') != -1:
        msg, trailing = msg.split(' :', 1)
        args = msg.split()
        args.append(trailing)
    else:
        args = msg.split()

    # Find command.
    command = args.pop(0)

    return prefix, command, args


# Plugin Start Point
# ------------------------------------------------------------------------------
def walnut(plugin_name):
    context    = zmq.Context()

    subscriber = context.socket(zmq.SUB)
    subscriber.connect("tcp://0.0.0.0:9890")
    subscriber.setsockopt(zmq.SUBSCRIBE, b"IRC:PRIVMSG")
    subscriber.setsockopt(zmq.SUBSCRIBE, b"IRC:PING")
    subscriber.setsockopt(zmq.SUBSCRIBE, b"IPC:CALL")

    requester = context.socket(zmq.REQ)
    requester.connect("tcp://0.0.0.0:9891")

    while True:
        # Receive and parse messages (Just routing messages. Payload type is unknown)
        message        = subscriber.recv()
        tag, args, pay = parse_payload(message.decode('UTF-8'))
        tag, command   = tag.split(':')
        message        = Message(tag, args, pay)

        # Handle IRC namespaced messages.
        if tag == 'IRC':
            # Parse the IRC message.
            message = IRCMessage(message)
            print('R: {}'.format(pay))

            # Run plugins listening for commands in this namespace.
            for f in hooks[command]:
                response = f(message)

                if response:
                    print('S: {}'.format(response))
                    requester.send('WAR:FORWARD({},{}){}'.format(plugin_name, args[0], response).encode('UTF-8'))
                    requester.recv()
