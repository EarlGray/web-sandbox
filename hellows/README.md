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

Then go to [http://localhost:8888]() in your browser.


Format of json config
=====================

Any field is optional:

```json
    {
        /* 8888 by default */
        "port" : 8888,

        /* if this is enabled, use `https` instead of `http` */
        "ssl_options": {
            "certfile": "/path/to/your/public.key",
            "keyfile": "/path/to/your/private.key",
        }
    }
```

