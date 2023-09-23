deps_test
=====

An OTP application

![CI](https://github.com/Cam/deps_test/actions/workflows/ci.yml/badge.svg)

Required
-----

```shell
$ rebar3 -v
 rebar 3.14.4 on Erlang/OTP 22 Erts 10.7.2.1
 ```

Build
-----

    $ make co

Eunit
-----

    $ make eunit

Common Test
-----

    $ make ct

Dialyzer
----

    $ make dialyzer

Test(dialyzer, eunit, ct)
----

    $ make test
