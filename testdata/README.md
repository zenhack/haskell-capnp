This directory contains data for use with the test suite:

* `aircraft.capnp` is a schema with many useful datatypes
* `schema-codegenreq` is the output of
   `capnp compile /usr/include/capnp/schema.capnp -o-`. It would be nice
   to keep this in textual form and convert it with capnp encode, so it
   could be viewed more easily, but unfortunately it contains
   `AnyPointer`s, so `capnp decode` -> `capnp encode` fails.
