FROM haskell:7.8
MAINTAINER Nathan Kot me@nathankot.com

# The build process is largely based on this:
# https://github.com/begriffs/heroku-buildpack-ghc/blob/master/bin/compile

# Lets work with bash
RUN rm /bin/sh && ln -s /bin/bash /bin/sh

RUN mkdir -p /app
WORKDIR /app

RUN cabal sandbox init --sandbox /app/.cabal-sandbox
RUN cabal update && cabal install cabal-install

ADD ./wsproxy.cabal /app/wsproxy.cabal
ADD ./cabal.config /app/cabal.config
ADD . /app
RUN cabal install -j --only-dependencies
RUN cabal build

CMD cabal run wsproxy
