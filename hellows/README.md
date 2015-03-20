What's this?
============

This is a tiny websocket chat.

Features:
- emoticons;
- desktop notifications;

Client side uses Javascript + jQuery.
Server side uses Python/Tornado:

    $ pip install tornado

Run it like:

    $ python wschat.py

or 

    $ python wschat.py config.json

Then go to [http://localhost:8888](http://localhost:8888) in your browser.


Format of json config
=====================

Any field is optional:

```json
    {
        "port" : 8888,
        "addr" : "192.168.0.0",

        "ssl_options": {
            "certfile": "/path/to/your/public.key",
            "keyfile": "/path/to/your/private.key",
        }
    }
```

If `ssl_options` are enabled, use `https://` instead of `http://`.

