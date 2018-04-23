#!/usr/bin/env sh
#
# Format the whole source tree with stylish-haskell.
set -ex
cd "$(dirname $0)"
stylish-haskell -i $(find * -name '*.hs')
