import json
import time
import os.path
import signal
from threading import Timer

import tornado.web
import tornado.ioloop
import tornado.httpserver
import tornado.websocket

sessions = dict()
watchdog = None

class FileHandler(tornado.web.RequestHandler):
    filemap = { '' : 'index.htm' }

    def get(self, reqpath):
        remote_ip = self.request.remote_ip

        fname = self.filemap.get(reqpath, reqpath)
        if not os.path.exists(fname):
            print('%s <= 404 (%s)' % (remote_ip, reqpath))
            self.write('404');
            return

        print('%s: %s <= %s' % (remote_ip, self.request.uri, fname))

        contenttype = {'css': 'text/css', 'js': 'text/javascript'}.get(fname.split('.')[-1])
        if contenttype: self.set_header('Content-Type', contenttype)

        self.render(fname)

class WSHandler(tornado.websocket.WebSocketHandler):
    ws_counter = 0

    def open(self):
        self.active = False
        self.waiting = False
        self.id = WSHandler.ws_counter
        WSHandler.ws_counter = WSHandler.ws_counter + 1

        self.ip = self.request.remote_ip
        print('%02d | ws.open() <= %s' % (self.id, self.ip))

    def ping(self):
        self.send_json({'type': 'ping'})

    def send_json(self, msg):
        self.write_message(json.dumps(msg))

    def on_message(self, message):
        self.waiting = False
        if not self.active:
            # we're active now:
            print '%02d | active' % self.id
            self.active = True

        try:
            msg = json.loads(message)
            ty = msg['type']
            if   ty == 'login': self.on_login(msg['user'])
            elif ty == 'ping': self.write_message({'type': 'pong'})
            elif ty == 'pong': pass
            elif ty == 'sent':
                print '%02d | <= %s' % (self.id, message)
                self.on_sent(msg)
        except Exception as e:
            print '%02d | ws.on_message error: %s' % (self.id, str(e))

    def check_presence(self):
        if self.waiting:
            if self.active:
                # we were active but have waited some time without response
                print '%02d | inactive' % self.id
                self.active = False

        self.waiting = True

    def on_sent(self, msg):
        msg = {
            'type': 'recv',
            'from': self.user,
            'time': msg.get('time', int(time.time() * 1000)),
            'text': msg['text']
        }
        for user, ws in sessions.iteritems():
            if self.user != user:
                ws.send_json(msg)

        self.send_json({'type': 'sent', 'status': 'ok', 'time': msg['time']})

    def on_login(self, user):
        already_logged_in = sessions.has_key(user)
        if already_logged_in:
            if sessions[user].active:
                msg = { 'type': 'login',
                        'status': 'failed',
                        'reason': 'user %s already signed in' % user }
                self.send_json(msg)
                print '%02d | already logged in: %s' % (self.id, user)
                return
            else:
                # replace inactive connection
                sessions.pop(user)

        self.user = user

        for user, ws in sessions.iteritems():
            if user != self.user:
                # send 'joined': user to me
                self.send_json({'type': 'join', 'user': user})
                # send 'joined': me to user
                if not already_logged_in:
                    ws.send_json({'type': 'join', 'user': self.user})

        sessions[self.user] = self
        self.send_json({'type': 'login', 'status': 'ok'})
        print '%02d | logged in: %s' % (self.id, self.user)

    def on_close(self):
        if not hasattr(self, 'user'):
            print '%02d | ws.on_close (no session)' % self.id
            return

        print '%02d | ws.on_close(user=%s)' % (self.id, self.user)
        sessions.pop(self.user)

        exitmsg = {'type': 'exit', 'user': self.user}
        for ws in sessions.values():
            ws.send_json(exitmsg)


def status_watcher():
    def set_interval(func, sec):
        def _wrapper():
            watchdog = set_interval(func, sec)
            func()

        watchdog = Timer(sec, _wrapper)
        watchdog.setDaemon(True)
        watchdog.start()

    def ping_all():
        #print('pingall')
        for ws in sessions.values():
            ws.check_presence()
            ws.ping()

    set_interval(ping_all, 10)


def sigusr1(a, b):
    print "===|======= sesssions ========="
    for user, ws in sessions.iteritems():
        print('%02d | %s : %s' % (ws.id, user, ws.ip))
        ws.ping()

    print "===|==========================="

application = tornado.web.Application([
    #(r'/',         FileHandler),
    (r'/ws',       WSHandler),
    (r'/(.*)',     FileHandler),
    #(r'/(.*)',     tornado.web.StaticFileHandler, {'path': '.' }),
])

if __name__ == '__main__':
    signal.signal(signal.SIGUSR1, sigusr1)
    status_watcher()

    http_server = tornado.httpserver.HTTPServer(application)
    http_server.listen(8888);
    tornado.ioloop.IOLoop.instance().start()
