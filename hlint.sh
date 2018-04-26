#!/usr/bin/env sh
#
# Run hlint on most of the codebase.
#
# We skip the generated schema, since we deliberately use conventions
# in the output that hlint will flag. We also skip code that is meant
# to be replaced by the generated output, once the generator is complete.
exec hlint $(find * -name '*.hs' \
	| grep -v Data.CapNProto.Core \
	| grep -v Schema.CapNProto)
