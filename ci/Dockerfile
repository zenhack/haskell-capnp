FROM alpine:3.8
WORKDIR /usr/src/

# Cabal does not like busybox's wget:
RUN apk add \
	wget

# Install the Haskell tooling.
RUN apk add \
	build-base \
	ghc \
	cabal

# Install the linters up front; this saves a lot of build time during the actual
# CI run.
RUN cabal update
RUN cabal install happy alex
RUN cabal install hlint stylish-haskell

# We're getting an error from cabal re: creating these symlinks automatically,
# so we just do it ourselves. TODO: figure out why this is happening

RUN ln -s /root/.cabal/bin/hlint /usr/local/bin/hlint
RUN ln -s /root/.cabal/bin/stylish-haskell /usr/local/bin/stylish-haskell

# Install stuff needed to build capnproto:
RUN apk add \
	autoconf \
	automake \
	libtool \
	linux-headers

# Build and install a recent version of capnproto; it isn't in the alpine
# repos. Furthermore, we use the calculator-client & server examples as
# part of our test suite, so we need to build that anyway:
RUN wget "https://github.com/capnproto/capnproto/archive/v0.6.1.tar.gz"
RUN tar -xvf *.tar.gz
RUN cd capnproto-*/c++ && \
	autoreconf -i && \
	./configure --prefix=/usr/local && \
	make -j && \
	make install

# Build and install the C++ calculator example client & server, which
# we'll use to validate our own implementations:
RUN cd capnproto-*/c++/samples && \
	capnpc -oc++ calculator.capnp && \
	c++ \
		calculator-client.c++ \
		-std=c++14 \
		calculator.capnp.c++ \
		$(pkg-config --cflags --libs capnp-rpc) \
		-o calculator-client && \
	c++ \
		calculator-server.c++ \
		-std=c++14 \
		calculator.capnp.c++ \
		$(pkg-config --cflags --libs capnp-rpc) \
		-o calculator-server && \
	install -Dm755 calculator-client /usr/local/bin/c++-calculator-client && \
	install -Dm755 calculator-server /usr/local/bin/c++-calculator-server

# Add other tools we use during the run:
RUN apk add \
	git
