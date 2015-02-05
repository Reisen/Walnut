import re
import zmq
import collections
import functools


hooks = collections.defaultdict(list)


# Message Type Wrappers
# ------------------------------------------------------------------------------
class Message:
    def __init__(self, tag, frm, to, args, payload):
        self.tag     = tag
        self.frm     = frm
        self.to      = to
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
    parts   = msg.split(' ')
    frm, to = parts[1].split('!')
    count   = int(parts[2])
    args    = parts[3:3+count]
    payload = " ".join(parts[3+count:])
    return Message(parts[0], frm, to, args, payload)


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

    # Subscriber. Automatically subscribe to all relevant events.
    subscriber = context.socket(zmq.SUB)
    subscriber.connect("tcp://0.0.0.0:9890")
    for name in hooks:
        subscriber.setsockopt(zmq.SUBSCRIBE, "IRC:{}".format(name).encode('UTF-8'))

    # Requester, for outgoing messages.
    requester = context.socket(zmq.PUSH)
    requester.connect("tcp://0.0.0.0:9891")

    while True:
        # Receive and parse messages (Just routing messages. Payload type is unknown)
        message        = subscriber.recv()
        message        = parse_payload(message.decode('UTF-8'))

        # Handle IRC namespaced messages.
        if message.tag.startswith('IRC'):
            # Parse the IRC message.
            print('R: {}'.format(message.payload))
            irc_message = IRCMessage(message)

            # Run plugins listening for commands in this namespace.
            for f in hooks[irc_message.command]:
                response = f(irc_message)

                if response:
                    print('S: {}'.format(response))
                    requester.send('IPC:CALL {}!{} 1 forward {}'.format(
                        plugin_name,
                        message.frm,
                        response
                    ).encode('UTF-8'))
