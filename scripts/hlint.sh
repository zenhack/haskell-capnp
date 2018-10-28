#!/usr/bin/env sh
#
# Run hlint on most of the codebase.
#
# We skip generated files, since we somtimes deliberately use conventions
# in the output that hlint will flag, to make codegen easier.
cd "$(dirname $0)/.."
exec hlint $(find lib examples cmd tests -name '*.hs' \
		| grep -v examples/Capnp/Gen/ \
		| grep -v tests/Capnp/Gen/ \
		| grep -v lib/Capnp/Gen/ \
		| grep -v lib/Internal/Gen)
