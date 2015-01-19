import json
import time
import os.path

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
    #(r'/',         FileHandler),
    (r'/ws',       WSHandler),
    (r'/(.*)',     FileHandler),
    #(r'/(.*)',     tornado.web.StaticFileHandler, {'path': '.' }),
])

if __name__ == '__main__':
    http_server = tornado.httpserver.HTTPServer(application)
    http_server.listen(8888);
    tornado.ioloop.IOLoop.instance().start()
