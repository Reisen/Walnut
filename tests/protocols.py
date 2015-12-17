import msgpack as m
import nanomsg as n


def test_command():
    push = n.Socket(n.PUSH)
    push.connect('tcp://127.0.0.1:5006')

    pull = n.Socket(n.SUB)
    pull.connect('tcp://127.0.0.1:5005')
    pull.set_string_option(n.SUB, n.SUB_SUBSCRIBE, b'')

    push.send(m.packb([
        b'protocol.irc',
        b'*',
        b'message',
        m.packb([
            b'irc',
            b'Nick',
            b'Channel',
            b'.commandA foo | .commandB bar | .commandA baz',
            b'irc.network.com'
        ])
    ]))

    print('Output:')
    result = m.unpackb(pull.recv())
    embed  = m.unpackb(result[-1])
    print('Result: ', result)
    print('Embed: ', embed)

    command = m.unpackb(pull.recv())
    cmdlist = m.unpackb(command[-1])
    print('Command: ', command)
    print('Command List: ', cmdlist)


test_command()
