This directory contains the Dockerfile used to build:

<https://hub.docker.com/r/zenhack/haskell-capnp-ci/>

...which is used by ../.gitlab-ci.yml.

It's just the standard haskell docker image, but with the capnproto
package from jessie-backports installed.
