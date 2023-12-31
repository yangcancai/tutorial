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
%%% Created : 2023-08-20T11:48:23+00:00
%%%-------------------------------------------------------------------

-module(eproxy_app).

-author("Cam").

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ets:new(eproxy_counters, [named_table, public, set]),
    cool_tools_logger:start_default_log(true),
    cool_tools_logger:unlimited_logger_formatter(disk_log),
    cool_tools_logger:set_global_loglevel(debug),
    {ok, P} = eproxy_sup:start_link(),
    eproxy:start(),
    {ok, P}.

stop(_State) ->
    ok.

%% internal functions
