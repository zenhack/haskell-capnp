This directory contains the Dockerfile used to build:

<https://hub.docker.com/r/zenhack/haskell-capnp-ci/>

...which is used by ../.gitlab-ci.yml.

It's just the standard haskell docker image, plus capnproto 0.6.1 and
some linters already installed.
