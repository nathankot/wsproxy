# WSProxy

_Websockets proxy server written in Haskell_.

[![Deploy](https://www.herokucdn.com/deploy/button.png)](https://heroku.com/deploy)

Designed to be run as a standalone server for peculiar use-cases. It was built
to serve as a proxy between Heroku server and browser. Since Heroku uses a
random load-balancer, it becomes impossible to establish websocket connections
between client and Heroku app with >1 servers. Drop this proxy in the middle and
it will translate websocket requests to HTTP.

## Setup

For most use cases, it may be sufficient to run wsproxy on a single-dyno Heroku
instance. Like so:

```sh
git clone https://github.com/nathankot/wsproxy
cd wsproxy
heroku create --stack=cedar --buildpack https://github.com/begriffs/heroku-buildpack-ghc.git
git push heroku master
```

##  Configuration

All configuration is done via `ENV` variables. There is no need to modify the
source code to use this.

Variable  | Description                              | Default
--------- | ---------------------------------------- | -------
PORT      | HTTP port to listen on                   | `3636`
SERVER    | Server to upstream websocket messages to | `""`

## Connecting to wsproxy

Connections are opened by sending a message in the form of `Connect:<identity>`.

```js
var ws = new WebSocket("wss://wsproxy.server.com");

ws.onopen = function() {
  var iden = 'username'; // This is the identifier, it can be anything
  ws.send('Connect:' + iden);
}

ws.onmessage = function(o) {
  var msg = o.data;
  if (msg === 'Connection acknowledged') {
    console.log('Websockets connection established');
  }
}
```

## Sending messages downstream

HTTP requests against `wsproxy` get translated to websocket messages. Requests
should look like this:

```
POST https://wsproxy.server.com/push?identity=username&message=new%3Aitem
```

All clients with identities of `username` will now get a websockets message `new:item`

## Sending messages upstream

Websocket messages toward `wsproxy` get translated into HTTP requests if
`SERVER` is defined. A message sent like so:

```js
ws.send("Any message");
```

Will be translated to an HTTP requests against `SERVER` like so:

```
POST https://api.server.com/any/api/endpoint?identity=username&message=Any%20message
```

## License

The MIT License (MIT)

Copyright (c) 2014 Nathan Kot

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
