nifile
======

This is a basic NIF wrapper around the POSIX file IO APIs. Its intended to be a starting place to start benchmarking file IO outside of the builtin Erlang file IO architecture.

There's currently no documentation but nifile.erl should be fairly obvious. Notice that argument ordering is based on the POSIX functions so there are some differences between nifile and the standard Erlang file module.

