from drivers.walnut import Walnut
from redis import StrictRedis
from collections import defaultdict
from itertools import dropwhile, takewhile


userlist = defaultdict(lambda: defaultdict(set))


@Walnut.hook('PRIVMSG')
def user_check(message):
    if message.args[0] not in userlist[message.parent.frm]:
        return 'NAMES {}'.format(message.args[0])

    if message.prefix.split('!')[0].lower() not in userlist[message.parent.frm][message.args[0]]:
        return 'NAMES {}'.format(message.args[0])


@Walnut.hook('353')
def user_join_rply(message):
    for user in message.args[3].split():
        name, modes = map(lambda v: ''.join(v(lambda u: u in '~&@%+', user)), (dropwhile, takewhile))
        userlist[message.parent.frm][message.args[2]].add(name.lower())


@Walnut.hook('JOIN')
def user_join(message):
    name = message.prefix.split('!')[0]
    userlist[message.parent.frm][message.args[0]].add(name.lower())


@Walnut.hook('PART')
def user_part(message):
    name = message.prefix.split('!')[0]
    userlist[message.parent.frm][message.args[0]].remove(name.lower())


@Walnut.hook('KICK')
def user_kick(message):
    userlist[message.parent.frm][message.args[0]].remove(message.args[1].lower())


@Walnut.hook('QUIT')
def user_quit(message):
    name = message.prefix.split('!')[0].lower()
    for channel in userlist[message.parent.frm]:
        if name in userlist[message.parent.frm][channel]:
            userlist[message.parent.frm][channel].remove(name)
