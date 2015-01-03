# WSProxy

_Websockets proxy server written in Haskell_.

Designed to be run as a standalone server for peculiar use-cases. It was built
to serve as a proxy between Heroku server and browser. Since Heroku uses a
random load-balancer, it becomes impossible to establish websocket connections
between client and Heroku app with >1 servers. Drop this proxy in the middle and
it will translate websocket requests to HTTP.

## Setup

For non-critical usages, it may be sufficient to run wsproxy on a single-dyno
Heroku instance. Like so:

```sh
git clone https://github.com/nathankot/wsproxy
cd wsproxy
heroku create --stack=cedar --buildpack https://github.com/begriffs/heroku-buildpack-ghc.git
git push heroku master
```

##  Configuration

@todo: All configuration is done via `ENV` variables. There is no need to modify the
source code to use this.

