-module(deps_test_SUITE).

-include("deps_test_ct.hrl").

-compile(export_all).

all() ->
    [handle].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(deps_test),
    new_meck(),
    Config.

end_per_suite(Config) ->
    del_meck(),
    application:stop(deps_test),
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

new_meck() ->
    ok = meck:new(deps_test, [passthrough, non_strict, no_link]),
    ok.

expect() ->
    ok = meck:expect(deps_test, test, fun() -> {ok, 1} end).
del_meck() ->
    meck:unload().

handle(_Config) ->
    expect(),
    ?assertEqual({ok,1}, deps_test:test()),
    ok.