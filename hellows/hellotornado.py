import json
import time
import os.path
import signal

import tornado.web
import tornado.ioloop
import tornado.httpserver
import tornado.websocket

sessions = dict()

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

    def open(self):
        self.ip = self.request.remote_ip
        print('%s <= ws.open()' % self.ip)

    def send_json(self, msg):
        self.write_message(json.dumps(msg))

    def on_message(self, message):
        try:
            print '%s => %s' % (self.ip, message)
            msg = json.loads(message)
            ty = msg['type']
            if ty == 'login':
                self.on_login(msg['user'])
            elif ty == 'sent':
                self.on_sent(msg)
        except Exception as e:
            print 'ws.on_message error: ' + str(e)

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
        if sessions.has_key(user):
            msg = { 'type': 'login',
                    'status': 'failed',
                    'reason': 'user %s already signed in' % user }
            self.send_json(msg)
            print msg
        else:
            self.user = user

            users = [];
            for user, ws in sessions.iteritems():
                if user != self.user:
                    # send 'joined': user to me
                    self.send_json({'type': 'join', 'user': user})
                    # send 'joined': me to user
                    ws.send_json({'type': 'join', 'user': self.user})

            sessions[self.user] = self
            self.send_json({'type': 'login', 'status': 'ok'})

    def on_close(self):
        if not hasattr(self, 'user'):
            print '%s => ws.on_close (no session)' % self.ip
            return

        print '%s => ws.on_close(user=%s)' % (self.ip, self.user)
        sessions.pop(self.user)

        exitmsg = {'type': 'exit', 'user': self.user}
        for ws in sessions.values():
            ws.send_json(exitmsg)

def sigusr1(a, b):
    print "=========== sesssions: "
    for user, ws in sessions.iteritems():
        print('%s : %s' % (user, ws.ip))

    print

application = tornado.web.Application([
    #(r'/',         FileHandler),
    (r'/ws',       WSHandler),
    (r'/(.*)',     FileHandler),
    #(r'/(.*)',     tornado.web.StaticFileHandler, {'path': '.' }),
])

if __name__ == '__main__':
    signal.signal(signal.SIGUSR1, sigusr1)

    http_server = tornado.httpserver.HTTPServer(application)
    http_server.listen(8888);
    tornado.ioloop.IOLoop.instance().start()
