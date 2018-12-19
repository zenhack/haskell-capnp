#!/usr/bin/env sh
#
# Run hlint on most of the codebase.
#
# We skip generated files, since we somtimes deliberately use conventions
# in the output that hlint will flag, to make codegen easier.
cd "$(dirname $0)/.."
exec hlint $(find lib examples cmd tests scripts -name '*.hs' | grep -v /gen/)
