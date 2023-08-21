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

-module(eproxy).

-author("Cam").

-export([start/0]).

start() ->
  {ok, S} = gen_tcp:listen(12345, [binary, {packet,line}, {active, false}]),
  erlang:spawn(fun() -> start(S) end).
start(Listen)  ->
  {ok, S1} = gen_tcp:accept(Listen) ,
  Pid = erlang:spawn(fun () ->
        loop(S1)
               end),
  ok = gen_tcp:controlling_process(S1, Pid),
  ok.

loop(S) ->
  ok = inet:setopts(S, [{active, once}]),
  receive
    {tcp, S, Data}  ->
      ok = gen_tcp:send(S, Data),
      loop(S);
    {tcp_closed, S} ->
      io:format("socket closed : ~p ~n",[S])
  end.