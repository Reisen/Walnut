from re import compile
from functools import wraps
from collections import defaultdict
from redis import StrictRedis
from time import time

# Open a connection to Redis.
r = StrictRedis(host = 'localhost', port = 6379)
p = r.pubsub(ignore_subscribe_messages = True)

# Maintain a hook list, command list, and timer list.
h = defaultdict(list)
c = defaultdict(list)
t = []



# IRC Helper methods.
# ------------------------------------------------------------------------------
def parse(msg, match = compile(r'^(?:[:](\S+) )?(\S+)(?: (?!:)(.+?))?(?: [:](.+))?$')):
    prefix, command, *args = match.search(msg).groups()
    return prefix, command, args


def message(ident, chan, msg):
    r.publish(
        'SND.PRIVMSG:{}'.format(ident),
        'PRIVMSG {} :{}'.format(chan, msg)
    )


# Plugin Helper methods.
# ------------------------------------------------------------------------------
def listen(target):
    def get_target(f):
        p.psubscribe('RCV.{}*'.format(target))
        h[target].append(f)
        return f

    return get_target


def command(f):
    c[f.__name__].append(f)
    return f


def timer(length):
    def get_target(f):
        t.append([time(), length, f])
        return f

    return get_target


# Core listeners that all plugins will want to have setup.
# ------------------------------------------------------------------------------
@listen('PRIVMSG')
def command_router(ident, prefix, command, args):
    nick = prefix.split('!', 1)[0]
    chan = args[0]
    msg  = args[-1]

    if msg.startswith('.'):
        command, *msg = msg[1:].split(' ', 1)

        if not msg:
            msg = [""]

        for hook in c.get(command.lower(), []):
            result = hook(ident, nick, chan, *msg)
            if result:
                return "PRIVMSG {} :{}".format(
                    chan,
                    result
                )


@listen('PING')
def timer_router(ident, prefix, command, args):
    print('Doing Ping Callbacks')
    for timer in t:
        last, distance, callback = timer
        print('Timer Data:', *timer)

        if time() - last > distance:
            print('Calling Callback')
            callback(ident)


# Main event loop, all plugins should call this.
# ------------------------------------------------------------------------------
def main():
    for message in p.listen():
        if message['type'] != 'pmessage':
            continue

        # Extract the right Data.
        channel = message['channel'].decode('UTF-8').strip()
        data    = message['data'].decode('UTF-8').strip()
        ident   = channel.split('.', 1)[1]
        ident   = ident.split(':', 1)
        command = ident[0]
        ident   = ident[1]
        data    = parse(data)

        print('Received: {} command from {}'.format(command, ident))
        print(h)
        for hook in h[command]:
            response = hook(ident, *data)

            if response is not None:
                r.publish(
                    'SND.{}:{}'.format(command, ident),
                    response
                )
