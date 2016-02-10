from flask import Flask
from signal import SIGTERM
import flask
import os
import sys

the_pid = 0

app = Flask(__name__)
app.debug = True


@app.route('/')
def index():
    return flask.render_template('alarm.htm')


@app.route('/start', methods=['POST'])
def start():
    global the_pid
    if the_pid == 0:
        the_pid = os.fork()
        if the_pid == 0:
            sys.stdout = open(os.devnull, 'a')
            os.execlp('radio', 'radio', 'roks')
    return flask.redirect(flask.url_for('index'))


@app.route('/stop', methods=['POST'])
def stop():
    global the_pid
    if the_pid:
        os.kill(the_pid, SIGTERM)
        the_pid = 0
    return flask.redirect(flask.url_for('index'))

app.run(host='0.0.0.0')
