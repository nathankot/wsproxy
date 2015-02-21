# WSProxy

_Websockets proxy server written in Haskell_.

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
