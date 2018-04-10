ep
=====

An OTP library

Notes
-----

If I embed the ast of the impl function in the proto module I don't have
access to private functions and I have to rewrite local calls to remote ones
for the ones that are public.

Build
-----

    $ rebar3 compile
