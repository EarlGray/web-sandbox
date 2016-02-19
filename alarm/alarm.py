from flask import Flask
from signal import SIGTERM
import flask
import os
import sys

the_pid = 0

app = Flask(__name__)
app.debug = True


def forked():
    sys.stdout = open(os.devnull, 'a')
    os.execlp('radio', 'radio', 'roks')


@app.route('/')
def index():
    btncap = 'Stop' if the_pid else 'Start'
    return flask.render_template('alarm.htm', btncap=btncap)


@app.route('/start', methods=['POST'])
def start():
    global the_pid
    print(flask.request.form)
    if the_pid == 0:
        the_pid = os.fork()
        if the_pid == 0:
            forked()
    else:
        os.kill(the_pid, SIGTERM)
        the_pid = 0
    return flask.redirect(flask.url_for('index'))


@app.route('/stop', methods=['POST'])
def stop():
    return flask.redirect(flask.url_for('index'))

app.run(host='0.0.0.0')
