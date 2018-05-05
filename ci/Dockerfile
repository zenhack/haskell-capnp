FROM haskell
WORKDIR /usr/src/

# Install various system packages needed for building capnproto.
RUN apt-get update
RUN apt-get install -y wget file make

# Build and install a recent version of capnproto. jessie-backports has 0.5.3,
# which is missing some features used in our test suite.
RUN wget "https://capnproto.org/capnproto-c++-0.6.1.tar.gz"
RUN tar -xvf *.tar.gz
RUN cd capnproto-*/ && ./configure --prefix=/usr/local && make && make install

# Install the linters up front; this saves a lot of build time during the actual
# CI run.
RUN cabal update
RUN cabal install hlint stylish-haskell
