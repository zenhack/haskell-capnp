#!/usr/bin/env sh
#
# Format the whole source tree with stylish-haskell. Skip generated output.
set -e
cd "$(dirname $0)/.."
stylish-haskell -i $(find * -name '*.hs' \
	| grep -v examples/Capnp/Gen/ \
	| grep -v tests/main/Capnp/Gen/ \
	| grep -v lib/Capnp/Gen/ \
	| grep -v lib/Internal/Gen)
