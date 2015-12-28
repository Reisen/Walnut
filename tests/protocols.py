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
            b'GayMuscle',
            b'#none',
            b'.hello foo | .hello | .hello bar',
            b'irc.neetlife.co.uk'
        ])
    ]))

    # Ignore first response. It will be a forwarded message.
    pull.recv()

    # Unpack command response.
    message = m.unpackb(pull.recv())
    command = m.unpackb(message[-1])
    cmdlist = command[-1]
    print('Message: ', message)
    print('Command: ', command)
    print('Cmdlist: ', cmdlist)
    print()

    # Send response for testing.
    cmdlist[0] = 'Holy Shit What the Fuck!'
    push.send(m.packb([
        b'awesome_plugin',
        b'router',
        b'response',
        m.packb(command)
    ]))

    # Ignore second response. Forwarded message.
    pull.recv()

    # View following response.
    message = m.unpackb(pull.recv())
    command = m.unpackb(message[-1])
    cmdlist = command[-1]
    print('Message: ', message)
    print('Command: ', command)
    print('Cmdlist: ', cmdlist)
    print()


test_command()
