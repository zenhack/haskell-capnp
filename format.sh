#!/usr/bin/env sh
#
# Format the whole source tree with stylish-haskell. Skip generated output.
set -e
cd "$(dirname $0)"
stylish-haskell -i $(find * -name '*.hs' | grep -v lib/capnp/Capnp/)
