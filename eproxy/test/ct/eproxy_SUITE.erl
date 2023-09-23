-module(eproxy_SUITE).

-include("eproxy_ct.hrl").

-compile(export_all).

all() ->
    [handle, echo].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(eproxy),
    new_meck(),
    Config.

end_per_suite(Config) ->
    del_meck(),
    application:stop(eproxy),
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

new_meck() ->
%%    ok = meck:new(eproxy, [passthrough, non_strict, no_link]),
    ok.

expect() ->
%%    ok = meck:expect(eproxy, test, fun() -> {ok, 1} end).
    ok.
del_meck() ->
    ok.

handle(_Config) ->
    expect(),
    ok.

echo(_) ->
    ok.