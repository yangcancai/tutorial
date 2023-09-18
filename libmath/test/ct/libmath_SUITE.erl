%%%-------------------------------------------------------------------
%%% @author Cam

%%% Copyright (c) 2021 by yangcancai(yangcancai0112@gmail.com), All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%       https://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
   
%%% @doc
%%%
%%% @end
%%% Created : 2023-09-07T16:18:33+00:00
%%%-------------------------------------------------------------------

-module(libmath_SUITE).

-author("Cam").

-include("libmath_ct.hrl").

-compile(export_all).

all() ->
    [handle].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(libmath),
    new_meck(),
    Config.

end_per_suite(Config) ->
    del_meck(),
    application:stop(libmath),
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

new_meck() ->
    ok = meck:new(libmath, [non_strict, no_link]),
    ok.

expect() ->
    ok = meck:expect(libmath, test, fun() -> {ok, 1} end).

del_meck() ->
    meck:unload().

handle(_Config) ->
    expect(),
    ?assertEqual({ok, 1}, libmath:test()),
    ok.
