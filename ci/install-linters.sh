#!/usr/bin/env sh
#
# Install stylish-haskell and hlint. We build them with new-build, so they
# will be more likely to share built dependencies with both each other and
# haskell-capnp.
#
for pkg in stylish-haskell hlint; do
	cd /tmp
	cabal unpack $pkg
	cd $pkg-*
	cabal new-build
	cp $(find dist-newstyle -type f -name $pkg) ~/.cabal/bin/
done
