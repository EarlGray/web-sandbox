from flask import Flask, request
import flask
import json

from random import randint


class Game:
    OCEAN = 'O'
    MISS = 'X'
    HIT = 'S'

    WON = 1
    ONGOING = 0
    OVER = -1

    size = 4
    nturns = 8

    def __init__(self, *args):
        board = []
        for _ in xrange(self.size):
            board.append([self.OCEAN] * self.size)

        self.ship_row = randint(0, self.size - 1)
        self.ship_col = randint(0, self.size - 1)
        self.board = board
        self.turn = Game.nturns
        self.state = Game.ONGOING

    def guess(self, row, col):
        if (row, col) == (self.ship_row, self.ship_col):
            self.board[row][col] = Game.HIT
            self.state = Game.WON
            return True

        self.board[row][col] = Game.MISS
        self.turn -= 1
        if self.turn < 1:
            self.state = Game.OVER
        return False

    def get_state(self):
        """ returns Game.WON|Game.ONGOING|Game.OVER """
        return self.state


# Global state
the_next_id = 0
the_games = {}

gamelet = flask.Blueprint('battleship', __name__)


@gamelet.route('/')
def hello():
    url = flask.url_for('battleship.path_create')
    print(url)
    return flask.redirect(url)


@gamelet.route('/new')
def path_new():
    return flask.render_template('new.htm')


@gamelet.route('/create')
def path_create():
    global the_next_id
    game_id = the_next_id
    the_next_id += 1

    the_games[game_id] = Game()
    url = flask.url_for('battleship.path_game', game_id=game_id)
    return flask.redirect(url)


@gamelet.route('/game/<int:game_id>')
def path_game(game_id):
    if game_id not in the_games:
        flask.abort(404)

    game = the_games[game_id]
    kwargs = {
        'game_id': game_id,
        'game_finished': 'true' if (game.state != Game.ONGOING) else 'false',
        'bsize': game.size,
        'board': game.board,
        'n_shells': game.nturns
    }
    return flask.render_template('game.htm', **kwargs)


@gamelet.route('/ajax', methods=['POST'])
def path_ajax():
    def error_message(errmsg):
        return json.dumps({'type': 'err', 'error': errmsg})

    try:
        params = request.form

        game_id = int(params['game_id'])
        if game_id not in the_games:
            return error_message('NoSuchGame')
        game = the_games[game_id]

        event = params['event']

        if event == 'guess':
            if game.get_state() != Game.ONGOING:
                return error_message('GameFinished')

            row = int(params['row'])
            col = int(params['col'])

            hit = game.guess(row, col)
            state = game.get_state()

            respd = {
                'type': 'ok',
                'hit': hit,
                'state': state,
                'guesses': game.turn
            }
            if state == Game.OVER:
                respd['ship_row'] = game.ship_row
                respd['ship_col'] = game.ship_col

            resp = json.dumps(respd)
            # print 'resp = ', resp
            return resp
        else:
            return error_message('UnknownEvent')

    except KeyError, e:
        resp = error_message(str(e))
        print 'resp =', resp
        return resp


if __name__ == '__main__':
    import sys
    conf = {'debug': True}
    if len(sys.argv) == 2:
        with open(sys.argv[1]) as f:
            conf = json.loads(f.read())

    app = Flask(__name__)
    app.debug = conf.get('debug')
    app.register_blueprint(gamelet, url_prefix=conf.get('urlpre'))

    @app.errorhandler(404)
    def path_404(error):
        return flask.render_template('404.htm'), 404

    app.run(host=conf.get('host'))
