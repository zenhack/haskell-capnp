FROM alpine:3.8
WORKDIR /usr/src/

RUN apk add \
	build-base \
	wget \
	linux-headers \
	ghc \
	cabal

# Build and install a recent version of capnproto (which is not in the alpine
# repos).
RUN wget "https://capnproto.org/capnproto-c++-0.6.1.tar.gz"
RUN tar -xvf *.tar.gz
RUN cd capnproto-*/ && ./configure --prefix=/usr/local && make && make install

# Install the linters up front; this saves a lot of build time during the actual
# CI run.
RUN cabal update
RUN cabal install happy alex
RUN cabal install hlint stylish-haskell
