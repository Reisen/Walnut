import msgpack as m
import nanomsg as n


def test_message():
    push = n.Socket(n.PUSH)
    push.connect('tcp://127.0.0.1:5006')

    pull = n.Socket(n.SUB)
    pull.connect('tcp://127.0.0.1:5005')
    pull.set_string_option(n.SUB, n.SUB_SUBSCRIBE, b'')

    push.send(m.packb([
        b'test.message',
        b'protocol.irc',
        b'message',
        m.packb([
            b'irc',
            b'walnut',
            b'#bruh',
            b'I am here',
            b'irc.neetlife.co.uk'
        ])
    ]))

test_message()
