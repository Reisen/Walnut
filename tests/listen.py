import msgpack as m
import nanomsg as n


def test_listen():
    push = n.Socket(n.PUSH)
    push.connect('tcp://127.0.0.1:5006')

    pull = n.Socket(n.SUB)
    pull.connect('tcp://127.0.0.1:5005')
    pull.set_string_option(n.SUB, n.SUB_SUBSCRIBE, b'')

    while True:
        message = m.unpackb(pull.recv())
        print(message)
        print(m.unpackb(message[-1]))
        print()

test_listen()
