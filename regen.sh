#!/usr/bin/env sh
#
# Regenerate modules for the core capnproto schema.
#
# This does the following:
#
# 1. Copy the schema from /usr/include/capnp/ into the repo
# 2. Generate code for the schema
# 3. Copy it into place in the module hierarchy.
set -ex
cp /usr/include/capnp/*.capnp lib/Data/Capnp/
capnp compile -o- /usr/include/capnp/*.capnp | cabal new-run capnpc-haskell
find Data/ -type f -name '*.hs' -exec mv \{} lib/\{} \;
rm -r Data/
