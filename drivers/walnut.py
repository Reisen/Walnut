import re
import zmq
import collections
import traceback
import functools


hooks    = collections.defaultdict(list)
methods  = {}


class Message:
    def __init__(self, message):
        parts    = message.split(b' ')
        frm, *to = parts[1].split(b'!')
        count    = int(parts[2])
        args     = parts[3:3+count]
        payload  = b' '.join(parts[3+count:])

        if len(to) != 1:
            to = [b'']

        self.tag     = parts[0].decode('UTF-8')
        self.frm     = frm.decode('UTF-8')
        self.to      = b''.join(to).decode('UTF-8')
        self.args    = list(map(lambda v: v.decode('UTF-8'), args))
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
            try:
                if message.tag.startswith('IRC'):
                    message = IRCMessage(message)

                    for hook in hooks[message.command]:
                        try:
                            result = hook(message)
                            if not result:
                                continue

                            Walnut.ipc(
                                plugin_name,
                                message.parent.frm,
                                'forward',
                                result
                            )

                        except Exception as e:
                            traceback.print_exc()

                elif message.tag.startswith('IPC:CALL'):
                    try:
                        method = methods.get(message.args[0], lambda v: v)
                        method(message)

                    except Exception as e:
                        traceback.print_exc()

            except:
                traceback.print_exc()
