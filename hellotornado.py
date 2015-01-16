import json
import time

import tornado.web
import tornado.ioloop
import tornado.httpserver
import tornado.websocket

sessions = dict()

class FileHandler(tornado.web.RequestHandler):
    filemap = { '/'         : 'index.htm' }

    def get(self):
        fname = self.filemap.get(self.request.uri, '404.htm')
        print('%s <= index.htm' % self.request.remote_ip)
        self.render(fname)

class WSHandler(tornado.websocket.WebSocketHandler):
    sent_ok = json.dumps({'type': 'sent', 'status': 'ok'})
    login_ok = json.dumps({'type': 'login', 'status': 'ok'})

    def open(self):
        self.ip = self.request.remote_ip
        print('%s <= ws.open()' % self.ip)

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
        msg = json.dumps({
            'type': 'recv',
            'from': self.user,
            'time': msg.get('time', int(time.time())),
            'text': msg['text']
        })
        for user, ws in sessions.iteritems():
            if self.user != user:
                ws.write_message(msg)

        self.write_message(self.sent_ok)

    def on_login(self, user):
        if sessions.has_key(user):
            msg = { 'type': 'login',
                    'status': 'failed',
                    'reason': 'user %s already signed in' % user }
            self.write_message(json.dumps(msg))
            print msg
        else:
            self.user = user

            users = [];
            for user, ws in sessions.iteritems():
                if user != self.user:
                    # send 'joined': user to me
                    presentmsg = json.dumps({'type': 'join', 'user': user})
                    self.write_message(presentmsg)
                    # send 'joined': me to user
                    joinmsg = json.dumps({'type': 'join', 'user': self.user})
                    ws.write_message(joinmsg)

            sessions[self.user] = self
            self.write_message(self.login_ok)

    def on_close(self):
        print '%s => ws.on_close' % self.ip
        if not hasattr(self, 'user'):
            print 'self.on_close() with no self.user'
            return

        sessions.pop(self.user)

        exitmsg = json.dumps({'type': 'exit', 'user': self.user})
        for ws in sessions.values():
            ws.write_message(exitmsg)



application = tornado.web.Application([
    (r'/',         FileHandler),
    (r'/ws',       WSHandler),
    (r'/(.*)',     tornado.web.StaticFileHandler, {'path': '.' }),
])

if __name__ == '__main__':
    http_server = tornado.httpserver.HTTPServer(application)
    http_server.listen(8888);
    tornado.ioloop.IOLoop.instance().start()
