import json

import tornado.web
import tornado.ioloop
import tornado.httpserver
import tornado.websocket

sessions = dict()

class MainHandler(tornado.web.RequestHandler):
    def get(self):
        self.render('index.htm')

class WSHandler(tornado.websocket.WebSocketHandler):
    sent_ok = json.dumps({'type': 'sent', 'status': 'ok'})
    login_ok = json.dumps({'type': 'login', 'status': 'ok'})

    def open(self):
        self.ip = self.request.remote_ip
        print('WSHandler.open() <= %s' % self.ip)

    def on_message(self, message):
        try:
            print 'WSHandler<%s>.on_message(%s)' % (self.ip, message)
            msg = json.loads(message)
            ty = msg['type']
            if ty == 'login':
                self.on_login(msg['user'])
            elif ty == 'sent':
                self.on_sent(msg['text'])
        except Exception as e:
            print 'WSHandler.on_message error: ' + str(e)

    def on_sent(self, text):
        msg = json.dumps({ 'type': 'recv', 'from': self.user, 'text': text })
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
        print 'WSHandler<%s>: connection closed' % self.ip
        if not hasattr(self, 'user'):
            print 'self.on_close() with no self.user'
            return

        sessions.pop(self.user)

        exitmsg = json.dumps({'type': 'exit', 'user': self.user})
        for ws in sessions.values():
            ws.write_message(exitmsg)



application = tornado.web.Application([
    #(r'/', tornado.web.StaticFileHandler, {'path': 'index.htm' }),
    (r'/',   MainHandler),
    (r'/ws', WSHandler),
])

if __name__ == '__main__':
    http_server = tornado.httpserver.HTTPServer(application)
    http_server.listen(8888);
    tornado.ioloop.IOLoop.instance().start()
