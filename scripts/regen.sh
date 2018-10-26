#!/usr/bin/env sh
#
# Regenerate generated modules.
set -e

# Some helpers for reporting info to the caller:
log() {
	printf '%s\n' "$@" >&2
}

err() {
	log $@
	exit 1
}

repo_root="$(realpath $(dirname $0)/..)"
cd "$repo_root"

# First, make sure our non-schema generated modules are up to date.
log "Generating Internal.Gen"
runhaskell scripts/gen-basic-instances.hs

# Make sure the compiler plugin is up to date.
log "Rebuilding schema compiler plugin..."
cabal new-build capnpc-haskell

# We run the code generator from inside lib/, so that it outputs
# modules to the right locations:
cd lib

# Find the compiler plugin executable. It would be nice to just
# use new-run here, but doing so from a subdirectory is a bit fiddly
# and I(zenhack) haven't found a nice way to do it.
exe="$(find $repo_root/dist-newstyle -type f -name capnpc-haskell)"

# Make sure we only found one file:
argslen() {
	echo $#
}
case $(argslen $exe) in
	0) err "Error: capnpc-haskell executable not found in dist-newstyle." ;;
	1) : ;; # Just one file; we're okay.
	*) err "Error: more than one capnpc-haskell executable found in dist-newstyle." ;;
esac

core_inc=$repo_root/core-schema/

# Ok -- do the codegen. Add the compiler plugin to our path and invoke
# capnp compile.
log "Generating schema modules for main library..."
export PATH="$(dirname $exe):$PATH"
capnp compile \
		-I $core_inc \
		--src-prefix=$core_inc/ \
		-ohaskell \
		$core_inc/capnp/*.capnp

log "Generating schema modules for aircraft.capnp (test suite)..."
cd "$repo_root/tests/main"
capnp compile \
		-I $core_inc \
		--src-prefix=../data/ \
		-ohaskell \
		../data/aircraft.capnp

log "Generating schema modules for echo.capnp (examples)..."
cd "$repo_root/examples"
capnp compile \
		-I $core_inc \
		-ohaskell \
		echo.capnp

log "Linking echo schema into test suite"
for file in $(find Capnp -type f -name '*.hs'); do
		cd "$repo_root/tests/main"
		[ -d $(dirname $file) ] || mkdir -p $(dirname $file)
		ln -s "$repo_root/examples/$file" "$file"
done

# vim: set ts=2 sw=2 noet :
