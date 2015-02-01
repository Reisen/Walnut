import zmq
from collections import defaultdict
from functools import wraps


hooks = defaultdict(list)


def hook(event):
    def hook_method(f):
        @wraps(f)
        def handler(*args, **kwargs):
            return f(*args, **kwargs)

        hooks[event].append(handler)
        return handler

    return hook_method


@hook('PRIVMSG')
def bruh(tag, arg, pay):
    if 'bruh!' in pay:
        return "PRIVMSG #none :Yes I am bruh."


def parse_message(msg):
    tag, arg = msg.split('(', 1)
    arg, pay = arg.split(')', 1)
    arg      = arg.split(',')
    return (tag, arg, pay)


if __name__ == "__main__":
    context    = zmq.Context()
    subscriber = context.socket(zmq.SUB)
    subscriber.connect("tcp://0.0.0.0:9890")
    subscriber.setsockopt(zmq.SUBSCRIBE, b"PRIVMSG")
    subscriber.setsockopt(zmq.SUBSCRIBE, b"PING")

    requester = context.socket(zmq.REQ)
    requester.connect("tcp://0.0.0.0:9891")

    while True:
        message = subscriber.recv()
        print('Received: {}'.format(message))
        tag, arg, pay = parse_message(message.decode('UTF-8'))

        for f in hooks[tag]:
            response = f(tag, arg, pay)
            if response:
                requester.send('W-OUT(bruh,*){}'.format(response).encode('UTF-8'))
                requester.recv()
