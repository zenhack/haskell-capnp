#!/usr/bin/env sh
set -ex

cp ../echo.capnp ./

cat >> echo.capnp <<"EOF"
using Go = import "/go.capnp";
$Go.package("main");
$Go.import("main");
EOF

capnp compile \
	-I $GOPATH/src/zombiezen.com/go/capnproto2/std \
	-ogo \
	echo.capnp
