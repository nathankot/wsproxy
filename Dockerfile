FROM haskell:7.8
MAINTAINER Nathan Kot me@nathankot.com

# The build process is largely based on this:
# https://github.com/begriffs/heroku-buildpack-ghc/blob/master/bin/compile

RUN mkdir -p /sandbox/app
WORKDIR /sandbox/app
RUN cabal update && cabal install cabal-install

# Lets work with bash
RUN rm /bin/sh && ln -s /bin/bash /bin/sh

ADD ./wsproxy.cabal /sandbox/app/wsproxy.cabal
ADD ./cabal.config /sandbox/app/cabal.config
RUN cabal sandbox init --sandbox /sandbox/.cabal-sandbox
RUN cabal update && cabal install -j --only-dependencies --force-reinstalls

ADD . /sandbox/app
RUN cabal clean && cabal build -j

CMD ["cabal", "run", "wsproxy"]
