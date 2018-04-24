FROM haskell
RUN echo 'deb http://ftp.debian.org/debian jessie-backports main' \
	>> /etc/apt/sources.list
RUN apt-get update
RUN apt-get -t jessie-backports install -y capnproto
RUN cabal update
RUN cabal install stylish-haskell
RUN cabal install hlint
