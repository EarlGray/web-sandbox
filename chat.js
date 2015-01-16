function WSProtocol(user, wsurl, action) {
    var ws = new WebSocket(wsurl);
    var send = function(data) { ws.send(JSON.stringify(data)); }; 

    ws.onopen = function () {
        console.log('ws.onopen()');
        send({'type': 'login', 'user': user});
    };
    ws.onclose = function (ev) {
        action.ws_closed(ev);
        ws.close();
    };
    ws.onerror = function (err) {
        action.ws_error(err);
        ws.close();
    };
    ws.onmessage = function (ev) {
        var msg = JSON.parse(ev.data);
        switch (msg.type) {
          case 'login':
            if (msg.status == 'ok') action.login_ok(msg.users);
            else action.login_failed(msg.reason);
            break;
          case 'recv': action.msg_received(msg); break;
          case 'sent': action.msg_sent(msg.msgid); break;
          case 'join': action.user_joined(msg.user); break;
          case 'exit': action.user_quit(msg.user); break;
          default: action.unknown(ev.data);
        }
    };

    return {
        'ws': ws,
        'send': send,
    };
}

function ChatNotifications() {
    if (!Notification) {
        console.log('Warning: Notifications not available');
        return;
    }

    if (Notification.permission !== 'granted') {
        Notification.requestPermission();
    }

    var focused = true;
    var enabled = true;

    return {
        'gotFocus': function () { focused = true; },
        'lostFocus': function () { focused = false; },
        'enabled': function (status) { enabled = status; },
        'status': function () { return Notification.permission; },
        'notify': function (who, text) {
            if (!enabled || focused) return;
            new Notification('New message from ' + who, { 'body': text });
        },
    };
}

// Global state
var wsconn;
var user;
var notifs;
var online = false;

function histTimeFormat(date) {
    return date.toTimeString().split(' ')[0];
}

const smiles = [
    { re: /:\)/, style: 'smile' },
    { re: /:\(/, style: 'sad'   },
    { re: /:D/,  style: 'rofl'  },
    { re: /:P/,  style: 'teese' },
    { re: /:X/,  style: 'sealed' },
    { re: /:O/,  style: 'shocked' },
    { re: /:O/,  style: 'shocked' },
    { re: /;\(/, style: 'cry' },
    { re: /:]/,  style: 'blush' },
    { re: /:\|/, style: 'speechless' },
    { re: /\*bye\*/,    style: 'bye' },
    { re: /\*tea\*/,    style: 'tea' },
    { re: /\*sleepy\*/, style: 'sleepy' },
    { re: /\*idea\*/,   style: 'idea' },
    { re: /\*music\*/,  style: 'music' },
    { re: /\*puke\*/,   style: 'puke' },
    { re: /\*angel\*/,  style: 'angel' },
    { re: /\*sunny\*/,  style: 'sunny' },
    { re: /\*sick\*/,   style: 'sick' },
];

function prepareMessage(text) {
    if (typeof(text) != 'string') return text;
    console.log('text = ' + text);

    for (var i = 0; i < smiles.length; ++i) {
        var smile = smiles[i];
        text = text.replace(smile.re, '<div class="' + smile.style + ' smiley"></div>');
    }
    return text;
}

function appendMessage(sender, text, time) {
    var txt = prepareMessage(text);
    var row = $('<tr/>');
    row.append($('<td/>').attr('class', 'histtime').append(histTimeFormat(new Date(time))));
    row.append($('<td/>').attr('class', 'histuser').append(sender));
    row.append($('<td/>').attr('class', 'histmsg').append(txt));
    $('#ulhist').append(row);

    var histdiv = $('#hist');
    histdiv.scrollTop(histdiv[0].scrollHeight);
}
function appendInfo(msg) {
    appendMessage('', $('<span/>').attr('class', 'msginfo').text(msg), Date.now());
}
function appendError(err) {
    appendMessage('', $('<span/>').attr('class', 'msgerr').text(err), Date.now());
}

function adjustHeight() {
    var histdiv = $('#hist');
    histdiv.height($(window).height() - $('#message').outerHeight() - 50);
    histdiv.scrollTop(histdiv[0].scrollHeight);
}

function rstrHeader() {
    $('#rstr').prepend($('<tr/>').append($('<th/>').text('user list').append('<hr/>')));
}
function userJoined(user) {
    var row = $('<tr/>');
    row.append($('<td/>').attr('class', 'rstrtd').text(user));
    $('#rstr').append(row);

    appendInfo('' + user + ' joined');
}

function rstrQuit(user) {
    // remove user from roster
    var tbody = $('#rstr tbody')[0];
    for (var elem in tbody.children) {
        var c = tbody.children[elem];
        if (c === undefined) continue;
        if (c.textContent == user) {
            tbody.removeChild(c);
        }
    }
}
function rstrClear() {
    var tbody = $('#rstr tbody');
    tbody.empty();
    rstrHeader();
}

function userQuit(user) {
    console.log(user + ' quit');
    rstrQuit(user);
    appendInfo('' + user + ' quit');
}

function msgRecv(msg) {
    console.log('msgRecv(' + JSON.stringify(msg) + ')');
    appendMessage(msg.from, msg.text, msg.time);
    notifs.notify(msg.from, msg.text);
}
function msgSent(msgid) {
    console.log('msgSent');
}

function onWSClosed() {
    console.log('onWSClosed');

    if (online) {
        online = false;
        // disable sending
        var msg = $('#msg');
        msg.off('keyup');
        msg.on('keyup', function (ev) {
            if (ev.which == 13) {
                appendError('you are offline');
            }
        });
        rstrClear();
        appendError('connection closed by server');
        appendInfo('trying to reconnect...');
    } else {
        $('#ulhist tbody tr:last td:first').text( histTimeFormat(new Date()) );
    }

    // try to reconnect:
    setTimeout(function () { 
        wsconn = initWS(user);
    }, 5000);
}
function onWSError(err) {
    //console.log('onWSError');
}

function initNotifs() {
    notifs = ChatNotifications();

    switch (notifs.status()) {
      case 'denied':  $('#sett_notif').hide(); break;
      case 'granted': $('#notif_cb')[0].checked = true; break;
    }
    $('#notif_cb').change(function () {
        notifs.enabled(this.checked);
    });

    $(window).focus( function ()   { notifs.gotFocus();  });
    $(window).blur(  function ()   { notifs.lostFocus(); });
}

function onLogin(msg) {
    online = true;

    if ($('#chat').css('display') == 'none') {
        // we came from login screen:
        $('#login').hide();
        $('#chat').show();

        adjustHeight();
        $(window).resize(function (ev) { adjustHeight();     });

        rstrHeader();

        initNotifs();
    }

    userJoined(user);

    var msg = $('#msg');
    msg.off('keyup');
    msg.on('keyup', function (ev) {
        if (ev.which == 13) {
            var m = { 'type': 'sent', 'time': Date.now(), 'text': msg.val() };
            wsconn.send(m);
            appendMessage(user, m.text, m.time);
            msg.val('');
        }
    });

    $('#msg').focus();
}

function onFailedLogin(err) {
    console.log('login failed');
    $('#loginfail').text('Login failed: ' + err);
}

function initWS(user) {
    var url = 'ws://' + window.location.host + '/ws';
    console.log('connecting to ' + url + '...');
    return WSProtocol(user, url, {
        'login_ok': onLogin,
        'login_failed': onFailedLogin, 

        'msg_received': msgRecv,
        'msg_sent': msgSent,

        'user_joined': userJoined,
        'user_quit': userQuit,

        'unknown': function (data) { console.log('unknown message: ' + data); },
        'ws_error': onWSError,
        'ws_closed': onWSClosed,
    });
}

function userFromQuery() {
    var qs = window.location.search.substring(1);
    if (qs == '') return null;

    qs = qs.split('&');
    for (var i = 0; i < qs.length; ++i) {
        var q = qs[i];
        var eqindex = q.indexOf('=');
        if (q.substr(0, eqindex) == 'user')
            return q.substr(eqindex + 1);
    }
    return null;
}
function reloadPage () { window.location = window.location.origin + '?user=' + user; }

