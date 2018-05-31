#!/usr/bin/env sh
#
# Regenerate modules for the core capnproto schema.
set -e

# Some helpers for reporting info to the caller:
log() {
	printf '%s\n' "$@" >&2
}

err() {
	log $@
	exit 1
}

# First make sure the compiler plugin is up to date.
log "Rebuilding schema compiler plugin..."
cd "$(dirname $0)"
cabal new-build capnpc-haskell

# We run the code generator from inside lib/, so that it outputs
# modules to the right locations:
cd lib

# Find the compiler plugin executable. It would be nice to just
# use new-run here, but doing so from a subdirectory is a bit fiddly
# and I(zenhack) haven't found a nice way to do it.
exe="$(find ../dist-newstyle -type f -name capnpc-haskell)"

# Make sure we only found one file:
argslen() {
	echo $#
}
case $(argslen $exe) in
	0) err "Error: capnpc-haskell executable not found in dist-newstyle." ;;
	1) : ;; # Just one file; we're okay.
	*) err "Error: more than one capnpc-haskell executable found in dist-newstyle." ;;
esac

# Ok -- do the codegen. Add the compiler plugin to our path and invoke
# capnp compile.
log "Generating schema modules..."
export PATH="$(dirname $exe):$PATH"
capnp compile -I ./schema -ohaskell ./schema/capnp/*.capnp

# vim: set ts=2 sw=2 noet :
