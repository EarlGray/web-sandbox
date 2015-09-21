// Global state
var wsconn;
var user;
var notifs;
var online = false;
var watchdog;

function WSProtocol(user, wsurl, action) {
    var ws = new WebSocket(wsurl);
    ws.id = Date.now() % 1000000;
    var send = function(data) {
        //console.log('ws<' + ws.id + '>.send(' + JSON.stringify(data) + ')');
        ws.send(JSON.stringify(data));
    };

    ws.onopen = function () {
        console.log('ws<' + ws.id + '>.onopen()');
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
        //console.log('ws<' + ws.id + '>.on_message: ' + ev.data);
        watchdog.on_server_activity();

        var msg = JSON.parse(ev.data);
        switch (msg.type) {
          case 'login':
            if (msg.status == 'ok') action.login_ok(msg.users);
            else action.login_failed(msg.reason);
            break;
          case 'recv': action.msg_received(msg); break;
          case 'sent': action.msg_sent(msg); break;
          case 'join': action.user_joined(msg.user); break;
          case 'exit': action.user_quit(msg.user); break;
          case 'ping': send({'type': 'pong'}); break;
          case 'pong': /* console.log('ws<' + ws.id + '>: pong()'); */ break;
          default: action.unknown(ev.data);
        }
    };

    return {
        'ws': ws,
        'send': send,
    };
}

function ChatNotifications() {
    if (!window.Notification) {
        console.log('Warning: Notifications not available');
        var empty = function () {};
        return {
            'gotFocus': empty, 'lostFocus': empty, 'notify': empty,
            'enabled': function () { return false; }, 'status': function () { return 'n/a'; },
        };
    }

    const notif_cookie = 'settings.desktop_notifications';
    var focused = true;
    var enabled = true;

    if ($.cookie) {
        $.cookie.json = true;
        var state = $.cookie(notif_cookie);
        if (state !== undefined)
            enabled = state;
    }

    if (Notification.permission !== 'granted') {
        Notification.requestPermission();
    }

    var mail_url = (function () {
        var sb = $('#smilebtn');
        var saveclass = sb.attr('class');
        sb.attr('class', 'mail');
        var bgimg = sb.css('background-image');
        sb.attr('class', saveclass);
        var url = bgimg.replace(/url\(['"]?/, '').replace(/['"]?\)$/, '');
        return url;
    })();

    return {
        'gotFocus': function () { focused = true; },
        'lostFocus': function () { focused = false; },
        'enabled': function (status) {
            if (status === undefined) return enabled;
            if ($.cookie) $.cookie(notif_cookie, status);
            enabled = status;
        },
        'status': function () {
            //if (!Notification) return null;
            return Notification.permission;
        },
        'notify': function (who, text) {
            //if (!Notification) return;
            if (!enabled || focused) return;
            var title = 'New message from ' + who;
            var params = { 'body': text, 'icon': mail_url };
            new Notification(title, params);
        },
    };
}

function histTimeFormat(date) {
    return date.toTimeString().split(' ')[0];
}

const smiles = [
    { re: /:\)/, style: 'smile', text: ':)' },
    { re: /:\(/, style: 'sad',   text: ':(' },
    { re: /:D/,  style: 'rofl',  text: ':D' },
    { re: /:P/,  style: 'tease', text: ':P' },
    { re: /:X/,  style: 'sealed',text: ':X' },
    { re: /:O/,  style: 'shocked', text: ':O' },
    { re: /;\(/, style: 'cry',   text: ';(' },
    { re: /:]/,  style: 'blush', text: ':]' },
    { re: /:\|/, style: 'speechless', text: ':|' },
    { re: /:\*/,        style: 'kiss', text: ':*' },
    { re: /B\)/,        style: 'cool', text: 'B)' },
    { re: /\*bye\*/,    style: 'bye',  text: '*bye*' },
    { re: /\*hi\*/,     style: 'bye',  text: '*hi*' },
    { re: /\*tea\*/,    style: 'tea',  text: '*tea*' },
    { re: /\*crazy\*/,  style: 'crazy', text: '*crazy*' },
    { re: /\*mail\*/,   style: 'mail', text: '*mail*' },
    { re: /\*sleepy\*/, style: 'sleepy', text: '*sleepy*' },
    { re: /\*nerdy\*/,  style: 'nerdy',  text: '*nerdy*' },
    { re: /\*phone\*/,  style: 'phone',  text: '*phone*' },
    { re: /\*idea\*/,   style: 'idea',   text: '*idea*' },
    { re: /\*music\*/,  style: 'music',  text: '*music*' },
    { re: /\*lips\*/,   style: 'lips',   text: '*lips*' },
    { re: /\*puke\*/,   style: 'puke',   text: '*puke*' },
    { re: /\*angel\*/,  style: 'angel',  text: '*angel*' },
    { re: /\*sunny\*/,  style: 'sunny',  text: '*sunny*' },
    { re: /\*sick\*/,   style: 'sick',  text: '*sick*' },
];
function smileTextByStyle(classes) {
    for (var i = 0; i < smiles.length; ++i) {
        if (classes.indexOf(smiles[i].style) > -1)
            return smiles[i].text;
    }
    return null;
}

function populateSmileTable() {
    /* get smile set */
    var _smiles = smiles.map(function (item) { return item.style; }).sort();
    var _smileset = [];
    while (_smiles.length) {
        var s = _smiles.pop();
        if (_smileset[0] !== s) _smileset.unshift(s);
    }

    /* populate smile table */
    var tbl = $('#smiletbl table');
    while (_smileset.length) {
        var tr = $('<tr/>');     // 5 items per row
        for (var i = 0; i < 5; ++i) {
            if (_smileset.length > 0)
                tr.append($('<td/>').attr('class', _smileset.shift() + ' smiley'));
        }
        tbl.append(tr);
    }
}

function initSmileTable() {
    var btn = $('#smilebtn');
    var tbl = $('#smiletbl');

    var tblClose = function () {
        tbl.hide();
        $('#msg').focus();
    };
    var tblShow = function () {
        var btnpos = btn.offset();
        var pos = {
            top: Math.floor(btnpos.top + btn.height() - tbl.height()),
            left: Math.floor(btnpos.left + btn.outerWidth() + 5),
        };
        tbl.show();
        tbl.offset(pos);
        $('#msg').focus();
    };
    var tblClick = function(clss) {
        var smtxt = smileTextByStyle(clss);

        var msg = $('#msg');
        var cursorpos = msg[0].selectionStart;
        var s = msg.val();
        msg.val( s.slice(0, cursorpos) + smtxt + s.slice(cursorpos) );
        cursorpos += smtxt.length;
        msg[0].setSelectionRange(cursorpos, cursorpos);
    };

    tbl[0].style.display = 'none';

    populateSmileTable();
    $('#smiletbl td').each(function() {
        var clss = $(this).attr('class').split(' ')[0];
        $(this).click(function () { tblClick(clss); });
    });

    btn.click(function() {
        if (tbl[0].style.display == 'none') tblShow();
        else tblClose();
    });
    return { 'show' : tblShow, 'hide': tblClose };
}

function prepareMessage(text) {
    if (typeof(text) != 'string') return text;

    /* handle commands */
    switch (text.split(' ')[0]) {
      case '/clear':
          $('#ulhist').empty();
          return null;
      case '/help':
          appendInfo('/help:<br/>/clear - clear the chat window');
          return null;
    }

    /* handle smiles */
    for (var i = 0; i < smiles.length; ++i) {
        var smile = smiles[i];
        text = text.replace(smile.re, '<div class="' + smile.style + ' smiley"></div>');
    }
    return text;
}

function appendMessage(sender, text, time) {
    var txt = prepareMessage(text);
    if (txt === null) return false; // do not send

    var row = $('<tr/>').attr('id', 'msg' + time);
    if (sender == window.user)
        row.append($('<td/>').attr('class', 'histtime').append('sending'));
    else
        row.append($('<td/>').attr('class', 'histtime').append(histTimeFormat(new Date(time))));

    row.append($('<td/>').attr('class', 'histuser').append(sender));
    row.append($('<td/>').attr('class', 'histmsg').append(txt));
    $('#ulhist').append(row);

    var histdiv = $('#hist');
    histdiv.scrollTop(histdiv[0].scrollHeight);
    if (MathJax) MathJax.Hub.Typeset();
    return true;
}
function appendInfo(msg) {
    appendMessage('', $('<span/>').attr('class', 'msginfo').append(msg), Date.now());
}
function appendError(err) {
    appendMessage('', $('<span/>').attr('class', 'msgerr').text(err), Date.now());
}

function adjustHeight() {
    var histdiv = $('#hist');
    histdiv.height($(window).height() - $('#message').outerHeight() - 50);
    histdiv.scrollTop(histdiv[0].scrollHeight);

    var msgw = $('#message').width() - $('#smilebtn').outerWidth() - $.em2px(1.0);
    $('#msg').outerWidth(msgw);
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
    rstrQuit(user);
    appendInfo('' + user + ' quit');
}

function msgRecv(msg) {
    //console.log('ws<' + wsconn.ws.id + '>: ' + 'msgRecv(' + JSON.stringify(msg) + ')');
    appendMessage(msg.from, msg.text, msg.time);
    notifs.notify(msg.from, msg.text);
}
function msgSent(msg) {
    var tr = $('#msg' + msg.time);
    if (tr) {
        tr.children('.histtime')[0].textContent = histTimeFormat( new Date(msg.time) );
    } else
        console.log('ws<' + wsconn.ws.id + '>: msgSent: ' + JSON.stringify(msg));
}

function onWSClosed() {
    console.log('ws<' + wsconn.ws.id + '>: onWSClosed');

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
    console.log('ws<' + wsconn.ws.id + '>: onWSError');
}

function WSWatchdog(interval, action) {
    var active = false;
    var waiting = false;

    var poll = function () {
        if (waiting && active) {
            active = false;
            if (action.on_sleep) action.on_sleep();
        }
        if (wsconn.ws && wsconn.ws.readyState == WebSocket.OPEN)
            wsconn.send({'type':'ping'});

        waiting = true;
    };

    setInterval(poll, interval);

    return {
        'is_server_available': function () { return active; },
        'on_server_activity': function () {
            waiting = false;
            if (!active) {
                active = true;
                if (action.on_wakeup) action.on_wakeup();
            }
        },
    };
}

function initNotifs() {
    notifs = ChatNotifications();

    switch (notifs.status()) {
      case 'denied':  $('#sett_notif').hide(); break;
      case 'granted': $('#notif_cb')[0].checked = true; break;
      case 'default': $('#notif_cb')[0].checked = false; break;
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
        initNotifs();
        initSmileTable();

        $('#login').hide();
        $('#chat').show();
        $('#notif_cb')[0].checked = notifs.enabled();

        adjustHeight();
        $(window).resize(function (ev) { adjustHeight(); });

        rstrHeader();
    }

    userJoined(user);

    var msg = $('#msg');
    msg[0].disabled = false;
    msg.css('text-align', 'left');
    msg.val('');
    msg.off('keyup');
    msg.on('keyup', function (ev) {
        if (ev.which == 13 && !ev.shiftKey) {
            if (msg.val().trim().length == 0) return;

            var m = { 'type': 'sent', 'time': Date.now(), 'text': msg.val() };
            if (appendMessage(user, m.text, m.time))
                wsconn.send(m);
            msg.val('');
        }
    });

    msg.focus();
}

function onFailedLogin(err) {
    console.log('ws<' + wsconn.ws.id + '>: login failed: ' + err);
    $('#loginfail').text('Login failed: ' + err);
    wsconn.ws.close();
}

function initWS(user) {
    var proto = (location.protocol == 'https:' ? 'wss' : 'ws');
    var url = proto + '://' + window.location.host + '/ws';
    console.log('connecting to ' + url + '...');
    return WSProtocol(user, url, {
        'login_ok': onLogin,
        'login_failed': onFailedLogin,

        'msg_received': msgRecv,
        'msg_sent': msgSent,

        'user_joined': userJoined,
        'user_quit': userQuit,

        'ws_error': onWSError,
        'ws_closed': onWSClosed,

        'unknown': function (data) {
            console.log('ws<' + wsconn.ws.id + '>: unknown message: ' + data);
        },
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

function initChat(username) {
    user = username;
    wsconn = initWS(user);
    watchdog = WSWatchdog(10000, {
        'on_wakeup': function () {
            console.log('server online');
        },
        'on_sleep': function () {
            console.log('server connection lost, reconnecting...');
            var msg = $('#msg');
            msg[0].disabled = true;
            msg.css('text-align', 'right');
            msg.val('offline');
            wsconn.ws.close();
        }
    });
}

function reloadPage () {
    window.location = window.location.origin + '?user=' + user;
}

function initPage() {
    $.em2px = function (em) {
        var px_in_em = parseFloat($('body').css('font-size'));
        return px_in_em * em;
    };

    var quser = userFromQuery();
    if (quser !== null)
        return initChat(quser)

    $('#loginform').on('submit', function (e) {
        e.preventDefault();

        quser = $('#inplogin').val();
        if (quser.length == 0) return;

        initChat(quser);
    });
}
