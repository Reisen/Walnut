import re
import zmq
import collections
import functools


hooks    = collections.defaultdict(list)
methods  = {}
commands = {}


class Message:
    def __init__(self, message):
        parts    = message.split(b' ')
        frm, *to = parts[1].split(b'!')
        count    = int(parts[2])
        args     = parts[3:3+count]
        payload  = b' '.join(parts[3+count:])

        if len(to) != 1:
            to = [b'']

        self.tag     = parts[0]
        self.frm     = frm
        self.to      = b''.join(to)
        self.args    = args
        self.payload = payload


class IRCMessage:
    def __init__(self, message):
        msg = message.payload.strip().decode('UTF-8')
        prefix, trailing = '', ''

        if msg[0] == ':':
            prefix, msg = msg[1:].split(' ', 1)

        if ' :' in msg:
            msg, trailing = msg.split(' :', 1)
            args = msg.split()
            args.append(trailing)
        else:
            args = msg.split()

        self.parent  = message
        self.prefix  = prefix
        self.command = args.pop(0)
        self.args    = args

        # Additional Useful Members. Not sure about this.
        if '!' in self.prefix:
            self.nick = self.prefix.split('!')[0]


class Walnut:
    def method(name):
        def register_method(f):
            @functools.wraps(f)
            def handler(*args, **kwargs):
                return f(*args, **kwargs)

            methods[name] = handler
            return handler

        return register_method

    def command(name):
        def register_command(f):
            @functools.wraps(f)
            def handler(*args, **kwargs):
                return f(*args, **kwargs)

            commands[name] = handler
            return handler

        return register_command

    def hook(event):
        def register_hook(f):
            @functools.wraps(f)
            def handler(*args, **kwargs):
                return f(*args, **kwargs)

            hooks[event].append(handler)
            return handler

        return register_hook

    def send(message):
        print(message)
        if message:
            Walnut.push.send(message.encode('UTF-8'))

    def ipc(source, destination, name, payload, *args):
        Walnut.send(' '.join('IPC:CALL {}!{} {} {} {} {}'.format(
            source,
            destination,
            len(args) + 1,
            name,
            ' '.join(args),
            payload
        ).split()))

    def fetch():
        while True:
            yield Message(Walnut.sub.recv())

    def run(plugin_name):
        context     = zmq.Context()
        sub         = context.socket(zmq.SUB)
        push        = context.socket(zmq.PUSH)
        Walnut.push = push
        Walnut.sub  = sub
        sub.connect("tcp://0.0.0.0:9890")
        push.connect("tcp://0.0.0.0:9891")

        sub.setsockopt(zmq.SUBSCRIBE, "IPC:CALL".encode('UTF-8'))
        for name in hooks:
            sub.setsockopt(zmq.SUBSCRIBE, "IRC:{}".format(name).encode('UTF-8'))

        for message in Walnut.fetch():
            if message.tag.startswith(b'IRC'):
                message = IRCMessage(message)

                for result in filter(None.__ne__, (r(message) for r in hooks[message.command])):
                    Walnut.ipc(
                        plugin_name,
                        message.parent.frm.decode('UTF-8'),
                        'forward',
                        result
                    )

            elif message.tag.startswith(b'IPC:CALL'):
                method = methods.get(message.args[0].decode('UTF-8'), lambda v: v)
                method(message)
