@0x9ad5452150661708;

annotation nullable(field) :Bool;
# Normally, code generated for the high-level API treats null pointers
# the same as the default value for a field. Most of time this is what
# you want, but occasionally you will come across a protocol that cares
# whether a field is really a null pointer. Marking a field with
# $nullable(true) will cause the high level modules to wrap this field
# in a `Maybe`, allowing you to make the distinction.
#
# This has no effect on the low-level code generator; there you should
# use the has_* functions to distinguish, if necessary.
