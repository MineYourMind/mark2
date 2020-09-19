# -*- coding: utf-8 -*-

import getpass
import glob
import json
import os
import math
from string import Template
from twisted.internet import reactor
from twisted.internet.protocol import ClientFactory, ProcessProtocol
from twisted.internet.task import LoopingCall
from twisted.protocols.basic import LineReceiver
from . import properties
import psutil
import re
import sys
import urwid
from .shared import console_repr, open_resource


class TabEvent:
    fail = None

    def __init__(self, line, players):
        pos = line.rfind(' ') + 1
        if pos == 0:
            self.left, right = "", line
        else:
            self.left, right = line[:pos], line[pos:]

        self.players = [p for p in players if re.match(right, p, re.I)]
        if len(self.players) == 0:
            self.fail = line
        self.index = 0

    def __next__(self):
        if self.fail:
            return self.fail
        i = self.index % len(self.players)
        self.index += 1
        return self.left + self.players[i]


class Prompt(urwid.Edit):
    def __init__(self, get_players, run_command, *a, **k):
        self.history = ['']
        self.history_pos = 0
        self.tab = None

        self.get_players = get_players
        self.run_command = run_command

        urwid.Edit.__init__(self, *a, **k)

    def get_prompt(self):
        return self.get_edit_text()

    def set_prompt(self, x):
        self.set_edit_text(x)
        self.set_edit_pos(len(x))

    def save_prompt(self):
        self.history[self.history_pos] = self.get_prompt()

    def load_prompt(self):
        self.set_prompt(self.history[self.history_pos])

    def keypress(self, size, key):
        if key != 'tab':
            self.tab = None

        if key == 'up':
            if self.history_pos > 0:
                self.save_prompt()
                self.history_pos -= 1
                self.load_prompt()
        elif key == 'down':
            if self.history_pos < len(self.history) - 1:
                self.save_prompt()
                self.history_pos += 1
                self.load_prompt()
        elif key == 'enter':
            text = self.get_prompt()
            if len(text) > 0:
                self.run_command(text)
                self.history_pos = len(self.history) - 1
                if self.history[self.history_pos - 1] == text:
                    self.set_prompt('')
                    self.cursor = 0
                    self.save_prompt()
                else:
                    self.save_prompt()
                    self.history.append('')
                    self.history_pos += 1
                    self.load_prompt()
        elif key == 'tab':
            text = self.get_prompt()
            if text == '':
                self.set_prompt('say ')
            else:
                if self.tab is None:
                    self.tab = TabEvent(text, self.get_players())
                self.set_prompt(next(self.tab))
        else:
            return urwid.Edit.keypress(self, size, key)


class PMenuButton(urwid.Button):
    def __init__(self, caption, *a):
        super(PMenuButton, self).__init__(caption, *a)
        self._w = urwid.SelectableIcon(caption, 0)


class UI:
    loop = None
    screen = urwid.raw_display.Screen()

    def __init__(self, palette, get_players, run_command, switch_server, connect_to_server, pmenu_actions, pmenu_reasons):
        self.palette = palette
        self.get_players = get_players
        self.run_command = run_command
        self.switch_server = switch_server
        self.connect_to_server = connect_to_server

        self.pmenu_actions = pmenu_actions
        self.pmenu_reasons = pmenu_reasons

        self.lines = []
        self.filters = {}
        self.filter = lambda *a: True

        self.g_output_list = urwid.SimpleFocusListWalker([])

        self.build()

    def build(self):
        #header
        self.g_servers = urwid.Columns([])
        self.g_users   = urwid.Columns([])
        g_head         = urwid.AttrMap(self.g_servers, 'head')

        #main
        self.g_output      = urwid.ListBox(self.g_output_list)
        self.g_output_wrap = urwid.LineBox(urwid.AttrMap(self.g_output, 'output'))
        g_main             = urwid.WidgetDisable(urwid.AttrMap(self.g_output, 'console'))

        self.g_stats        = urwid.Text([('cpu', " CPU: 20.4% "), ('mem', " MEM: 66.2% "), ('load', " LOAD: 0.5 1.5 1.2 "), ('players', " PLAYERS: 18 of 52 ")], align='right')
        self.g_stats_min    = urwid.WidgetDisable(urwid.AttrMap(self.g_stats, 'stats'))

        #foot
        self.g_prompt = Prompt(self.get_players, self.run_command, ' > ')
        g_foot = self.g_prompt
        g_foot_min = urwid.Pile((
            urwid.AttrMap(self.g_prompt, 'prompt', 'prompt_focus'),
            urwid.Columns((urwid.AttrMap(self.g_users, 'stats'), self.g_stats_min))
            ))

        self.g_frame = urwid.Frame(g_main, g_head, g_foot_min, focus_part='footer')

    def main(self):
        self.loop = urwid.MainLoop(
            self.g_frame,
            self.palette,
            input_filter=self.filter_input,
            event_loop=urwid.TwistedEventLoop()
        )
        self.loop.run()

    def stop(self):
        def exit(*a):
            raise urwid.ExitMainLoop
        self.loop.set_alarm_in(0, exit)

    def filter_input(self, keys, raw):
        passthru = []
        for key in keys:
            if key in ('page up', 'page down'):
                self.g_output.keypress((0, 16), key)
            elif key == 'home':
                self.g_output.set_focus(0)
            elif key == 'end':
                self.g_output.set_focus_valign("bottom")
                self.g_output.set_focus(len(self.g_output_list) - 1, coming_from='above')
            elif key == 'meta left':
                self.switch_server(-1)
            elif key == 'meta right':
                self.switch_server(1)
            elif key == 'ctrl p':
                self.g_frame.focus_position = 'body'
            elif key == 'f8':
                raise urwid.ExitMainLoop
            else:
                passthru.append(key)

        return passthru

    def redraw(self):
        if self.loop:
            self.loop.draw_screen()

    def set_servers(self, servers, current=None):
        screenSize = lambda rows=True, scr=self.screen: scr.get_cols_rows()[rows]
        screenWidth = screenSize(False)
        #rows, columns = os.popen('stty size', 'r').read().split()
        
        extraWidth = 9 # mark2 name
        requiredWidth = len('  '.join(servers)) + extraWidth

        charsToRemove = 0
        requiredSpace = requiredWidth - screenWidth
        if requiredSpace > 0:
            serverAmount = len(servers) - 1 # minus the selected one
            charsToRemove = int(math.ceil(requiredSpace / serverAmount))
        
        new = []
        for s in sorted(servers):
            if s == current:
                e = urwid.AttrMap(urwid.Text((urwid.AttrSpec('default,standout','default'), " %s " % s)), 'server_current')
                self.g_output_wrap.set_title(s)
            else:
                tabName = s
                if screenWidth < requiredWidth:
                    reductionLimit = 3
                    charsToKeep = (len(s) - charsToRemove) - 3 # minus the underscore and ending
                    if charsToKeep < reductionLimit:
                        charsToKeep = reductionLimit

                    indices = slice(0, charsToKeep)
                    startPart = s[indices]
                    if startPart[-1:] != '_':
                        startPart += '_'

                    tabName = startPart + s[-2:]

                e = urwid.AttrMap(PMenuButton(" %s " % tabName, lambda button, _s=s: self.connect_to_server(_s)), 'server')
                
            new.append((e, self.g_servers.options('pack')))

        contents = self.g_servers.contents
        del contents[0:len(contents)]
        contents.append((urwid.AttrMap(urwid.Text(' mark2 '), 'mark2'), self.g_servers.options('pack')))
        contents.extend(new)

    def set_users(self, users):
        new = []
        for user, attached in users:
            e = urwid.Text(" %s " % user)
            e = urwid.AttrMap(e, 'user_attached' if attached else 'user')
            new.append((e, self.g_users.options('pack')))

        contents = self.g_users.contents
        del contents[0:len(contents)]
        contents.extend(new)

    def safe_unicode(self, text):
        if urwid.supports_unicode():
            return text
        else:
            return text.encode('ascii', errors='replace')

    def append_output(self, line):
        scroll = False
        del self.lines[:-999]
        self.lines.append(line)

        if not self.filter(line):
            return

        try:
            p = self.g_output.focus_position
            try:
                self.g_output.body.next_position(p)
            except IndexError:  # scrolled to end
                scroll = True
        except IndexError:  # nothing in listbox
            pass

        self.g_output_list.append(urwid.Text(colorize(self.safe_unicode(console_repr(line)))))
        if scroll:
            self.g_output.focus_position += 1

    def set_output(self, lines=None):
        contents = self.g_output_list
        del contents[0:len(contents)]

        lines = lines or self.lines
        lines = [l for l in lines if self.filter(l)]

        for line in lines:
            contents.append(urwid.Text(colorize(self.safe_unicode(console_repr(line)))))

        try:
            self.g_output.focus_position = len(lines) - 1
        except IndexError:  # nothing in list
            pass
        self.redraw()

    def set_filter(self, filter_):
        if isinstance(filter_, str):
            return self.set_filter(self.filters[filter_])
        self.filter = filter_.apply
        self.set_output()

    def set_stats(self, stats):
        self.g_stats.set_text([
            ('cpu', " CPU: {}% ".format(stats['cpu'])),
            ('mem', " MEM: {}% ".format(stats['memory'])),
            ('load', " LOAD: {} ".format(stats['load'])),
            ('players', " PLAYERS: {} of {} ".format(stats['players_current'], stats['players_max']))
        ])
        self.redraw()


class App(object):
    def __init__(self, name, interval, update, shell, command):
        self.name = name
        self.interval = interval
        self.update = update
        self.cmd = [shell, '-c', command]
        self.stopping = False
        self.start()

    def start(self):
        p = ProcessProtocol()
        self.buff     = ""
        self.protocol = p

        p.outReceived   = self.got_out
        p.processEnded  = self.got_exit
        reactor.spawnProcess(p, self.cmd[0], self.cmd)

    def got_out(self, d):
        self.buff += d

    def got_exit(self, *a):
        self.update(self.name, self.buff.strip())
        if not self.stopping:
            reactor.callLater(self.interval, self.start)


class LineFilter:
    HIDE = 1
    SHOW = 2

    def __init__(self):
        self._actions = []
        self._default = self.SHOW

    def append(self, action, *predicates):
        self.setdefault(action)
        def action_(msg):
            if all(p(msg) for p in predicates):
                return action
            return None
        self._actions.append(action_)

    def setdefault(self, action):
        if len(self._actions) == 0:
            self._default = (self.HIDE if action != self.SHOW else self.SHOW)

    def apply(self, msg):
        current = self._default
        for action in self._actions:
            current = action(msg) or current
        return current == LineFilter.SHOW


class UserClientFactory(ClientFactory):
    def __init__(self, initial_name, shared_path='/tmp/mark2'):
        self.socket_to   = lambda n: os.path.join(shared_path, n + ".sock")
        self.socket_from = lambda p: os.path.splitext(os.path.basename(p))[0]

        self.client = None
        self.stats = {}
        self.me = os.getenv('REMOTEUSER') or 'unknown'

        #read the config
        self.config = properties.load(properties.ClientProperties, open_resource('resources/mark2rc.default.properties'), os.path.expanduser('~/.mark2rc.properties'))
        assert not self.config is None
        self.stats_template = Template(self.config['stats'])

        #start apps
        self.apps = []

        #start ui
        self.ui = UI(self.config.get_palette(), self.get_players, self.run_command, self.switch_server, self.connect_to_server, self.config.get_player_actions(), self.config.get_player_reasons())
        for name, command in self.config.get_apps():
            app = App(name, self.config.get_interval('apps'), self.app_update, self.config['stats.app_shell'], command)
            self.apps.append(app)

        #tasks
        t = LoopingCall(self.update_servers)
        t.start(self.config.get_interval('servers'), now=True)

        t = LoopingCall(self.update_users)
        t.start(self.config.get_interval('users'), now=True)

        t = LoopingCall(self.update_players)
        t.start(self.config.get_interval('players'), now=True)

        t = LoopingCall(self.update_stats)
        t.start(self.config.get_interval('stats'), now=True)

        self.connect_to_server(initial_name)

    def log(self, w):
        self.ui.append_output(str(w))

    def main(self):
        self.ui.main()

    def buildProtocol(self, addr):
        self.client = UserClientProtocol(self.socket_from(addr.name), self.me, self)
        self.update_servers()
        return self.client

    def switch_server(self, delta=1):
        index = self.servers.index(self.client.name)
        self.update_servers()
        if len(self.servers) == 0:  # no running servers
            return self.ui.stop()
        if len(self.servers) == 1 and self.client.name in self.servers: 
            return # don't switch with only one server

        name = self.servers[(index + delta) % len(self.servers)]
        self.connect_to_server(name)

    def connect_to_server(self, name):
        if self.client:
            self.client.close()
        reactor.connectUNIX(self.socket_to(name), self)

    def update_servers(self):
        servers = []
        for f in glob.glob(self.socket_to('*')):
            servers.append(self.socket_from(f))

        self.servers = sorted(servers)
        self.ui.set_servers(self.servers, current=self.client.name if self.client else None)

    def update_users(self):
        if self.client:
            self.client.get_users()

    def update_players(self):
        if self.client:
            self.client.get_players()

    def update_stats(self):
        if self.client:
            self.client.get_stats()

    def app_update(self, name, data):
        self.stats[name] = data

    def get_players(self):
        if self.client:
            return self.client.players
        else:
            return []

    def run_command(self, command):
        if self.client:
            return self.client.run_command(command)

    def server_connected(self, client):
        pass

    def server_disconnected(self, client):
        self.switch_server()

    def server_output(self, line):
        self.ui.append_output(line)

    def server_scrollback(self, lines):
        self.ui.set_output(lines)

    def server_users(self, users_a):
        #users_l = list(self.system_users)

        users = []
        #for u in sorted(set(users_l + users_a), key=str.lower):
        #    users.append((u, u in users_a))
        for u in sorted(users_a, key=str.lower):
            users.append((u, u == self.me))

        self.ui.set_users(users)

    def server_stats(self, stats):
        self.stats.update(stats)
        self.ui.set_stats(self.stats)

    def server_regex(self, patterns):
        self.make_filters(patterns)

    def make_filters(self, server_patterns={}):
        cfg = {}
        cfg.update(server_patterns)
        cfg.update(self.config.get_by_prefix('pattern.'))

        # read patterns from config to get a dict of name: filter function
        def makefilter(p):
            ppp = p
            p = re.compile(p)
            def _filter(msg):
                m = p.match(msg['data'])
                return m and m.end() == len(msg['data'])
            return _filter
        patterns = dict((k, makefilter(p)) for k, p in cfg.items())

        patterns['all'] = lambda a: True

        # read filters
        self.ui.filters = {}
        for name, spec in self.config.get_by_prefix('filter.'):
            filter_ = LineFilter()
            action = LineFilter.SHOW
            for pattern in spec.split(','):
                pattern = pattern.strip().replace('-', '_')
                if ':' in pattern:
                    a, pattern = pattern.split(':', 1)
                    action = {'show': LineFilter.SHOW, 'hide': LineFilter.HIDE}.get(a)
                    filter_.setdefault(action)
                if not pattern:
                    continue
                filter_.append(action, patterns[pattern])
            self.ui.filters[name] = filter_
        self.ui.set_filter(self.config['use_filter'])


class NullFactory(object):
    def __getattr__(self, name):
        return lambda *a, **k: None


class UserClientProtocol(LineReceiver):
    MAX_LENGTH = 999999
    delimiter = '\n'
    enabled = False

    def __init__(self, name, user, factory):
        self.name = name
        self.user = user
        self.users = set()
        self.players = list()
        self.factory = factory

    def close(self):
        self.transport.loseConnection()
        self.factory = NullFactory()

    def connectionMade(self):
        self.alive = 1
        self.send("attach", user=self.user)
        self.send("get_scrollback")
        self.factory.server_connected(self)

    def connectionLost(self, reason):
        self.alive = 0
        self.factory.server_disconnected(self)

    def lineReceived(self, line):
        #log.msg(line)
        msg = json.loads(line)
        ty = msg["type"]

        if ty == "console":
            self.factory.server_output(msg)

        elif ty == "scrollback":
            self.factory.server_scrollback(msg['lines'])

        elif ty == "user_status":
            user = str(msg["user"])
            if msg["online"]:
                self.users.add(user)
            else:
                self.users.discard(user)
            self.factory.server_users(list(self.users))

        elif ty == "players":
            self.players = msg['players']

        elif ty == "stats":
            self.factory.server_stats(msg['stats'])

        elif ty == "regex":
            self.factory.server_regex(msg['patterns'])

        else:
            self.factory.log("wat")

    def send(self, ty, **d):
        d['type'] = ty
        if self.alive:
            self.sendLine(json.dumps(d))

    def run_command(self, command):
        self.send("input", line=command, user=self.user)

    def get_players(self):
        self.send("get_players")

    def get_stats(self):
        self.send("get_stats")

    def get_users(self):
        self.send("get_users")

def colorize(text):

    """
    Convert minecraft color codes to ansi escape codes
    """

    mappings_mc_ansi = {'0':90, '1':34, '2':32, '3':36, '4':31, '5':35, '6':33, '7':97,
                        '8':90, '9':94, 'a':92, 'b':96, 'c':91, 'd':95, 'e':33, 'f':37, 'r':37}
#                        '8':38, '9':42, 'a':40, 'b':44, 'c':39, 'd':43, 'e':41, 'f':45}
    formatting_codes = {'k', 'l', 'm', 'n', 'o'}

    if text.find('\u00A7') != -1:
        for code in mappings_mc_ansi:
            text = text.replace('\u00A7' + code, '\033[' + str(mappings_mc_ansi[code]) + 'm')
        for code in formatting_codes:
            text = text.replace('\u00A7' + code, '')

    """
    Convert ansi escape codes to urwid display attributes
    """

    mappings_fg = {90: 'dark gray', 31: 'dark red', 32: 'dark green', 33: 'yellow', 34: 'dark blue', 35: 'dark magenta', 36: 'dark cyan',
                   91: 'light red', 92: 'light green', 94: 'light blue', 95: 'light magenta', 96: 'light cyan', 97: 'light gray'}
    mappings_bg = {40: 'black', 41: 'dark red', 42: 'dark green', 43: 'brown', 44: 'dark blue', 45: 'dark magenta', 46: 'dark cyan', 47: 'light gray'}

    text_attributed = []

    parts = str(text).split('\x1b')

    regex = re.compile(r"^\[([;\d]*)m(.*)$", re.UNICODE | re.DOTALL)

    for part in parts:
        r = regex.match(part)

        if r:
            if r.group(2) != '':
                foreground = 'default'
                background = 'default'
                for code in [_f for _f in r.group(1).split(';') if _f]:
                    if (int(code) in mappings_fg):
                        foreground = mappings_fg[int(code)]

                    if (int(code) in mappings_bg):
                        background = mappings_bg[int(code)]

                text_attributed.append((urwid.AttrSpec(foreground, background), r.group(2)))
        else:
            if part != '':
                text_attributed.append(part)

    return text_attributed


if __name__ == '__main__':
    thing = UserClientFactory('testserver')
    thing.main()
