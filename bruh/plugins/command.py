import re
from drivers.walnut import Walnut, commands
from bruh import r


@Walnut.method('command')
def handle_command(message):
    if message.to.decode('UTF-8') in commands:
        db_key = '{}:{}'.format(message.args[-3].decode('UTF-8'), message.args[-2].decode('UTF-8'))
        prefix = r.get('{}:prefix'.format(db_key))
        prefix = prefix.decode('UTF-8') if prefix else '!'

        # Extract and execute the command in question.
        msg      = message.payload.decode('UTF-8')
        pieces   = re.findall(r'\{0}(.*?)(?:\|\s*(?=\{0})|$)'.format(prefix), msg)
        c, *args = pieces[0].split(' ', 1)

        # Unless the command is blacklisted that is.
        if r.sismember('{}:blacklist'.format(db_key), c):
            pieces = []
            result = 'The {} command has been blacklisted in this channel.'.format(c)

        else:
            result   = commands[message.to.decode('UTF-8')](
                message.args[-1].decode('UTF-8'),
                message.args[-2].decode('UTF-8'),
                args[0] if args else ''
            )

            result = result if result else ''

        # If there are still commands to process, carry on forwarding.
        if len(pieces) > 1:
            # Append the result to the end of the next command in the chain.
            pieces[1] = pieces[1] + ' ' + result

            Walnut.ipc(
                'command',
                pieces[1].split(' ', 1)[0],
                'command',
                prefix + (' | ' + prefix).join(pieces[1:]),
                '0',
                message.args[-3].decode('UTF-8'),
                message.args[-2].decode('UTF-8'),
                message.args[-1].decode('UTF-8')
            )

        else:
            Walnut.ipc(
                'proxy',
                message.args[-3].decode('UTF-8'),
                'forward',
                'PRIVMSG {} :{}'.format(
                    message.args[-2].decode('UTF-8'),
                    result
                )
            )


@Walnut.hook('PRIVMSG')
def command_dispatcher(message):
    msg    = message.args[-1]
    prefix = r.get('{}:{}:prefix'.format(message.parent.frm.decode('UTF-8'), message.args[0]))
    prefix = prefix.decode('UTF-8') if prefix else '!'

    # Escape early for messages that only consist of the command character or
    # are not commands at all.
    if not msg.startswith(prefix) or len(msg) < 2:
        return None

    chan   = message.args[0]
    chan   = chan if chan[0].startswith('#') else message.prefix.split('!')[0]
    pieces = re.findall(r'\{0}(.*?)(?:\|\s*(?=\{0})|$)'.format(prefix), msg)

    # Dispatch the IPC call.
    Walnut.ipc(
        'command',
        pieces[0].split(' ', 1)[0],
        'command',
        msg,
        '0',
        message.parent.frm.decode('UTF-8'),
        chan,
        message.prefix.split('!')[0]
    )
