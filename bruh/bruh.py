import os
import re
import queue
import collections
from drivers.walnut import Walnut
from redis import StrictRedis


r = StrictRedis(db=4)


@Walnut.command('echo')
def test_cmd(user, chan, message):
    return '(E)'+message


if __name__ == '__main__':
    for plugin in os.listdir('plugins'):
        if not plugin.endswith('.py'):
            continue

        name   = plugin[:-3]
        plugin = __import__('plugins.' + name, globals(), locals(), -1)

    Walnut.run('proxy')
