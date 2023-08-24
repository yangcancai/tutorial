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
-record(state, {remote, step = handshake, buffer = <<>>}).
start() ->
  {ok, S} = gen_tcp:listen(12345, [binary, {packet, 0}, {active, false}]),
  erlang:spawn(fun() -> start(S) end).
start(Listen)  ->
  {ok, S1} = gen_tcp:accept(Listen) ,
  Pid = erlang:spawn(fun () ->
        loop(S1, #state{})
               end),
  ok = gen_tcp:controlling_process(S1, Pid),
  start(Listen).

loop(S, #state{step = Step, remote = Remote, buffer = Buffer} = State) ->
  ok = inet:setopts(S, [{active, once}]),
  receive
    {tcp, Remote, Data} ->
       ok = gen_tcp:send(S, Data),
      loop(S, State);
    {tcp, S, Data} when Step /= handshake ->
      ok = gen_tcp:send(Remote, Data),
      loop(S, State);
    {tcp, S, Data} ->
        case parse_packet(Data, Buffer) of
             {done, NewBuffer}  ->
                 case check_packet(NewBuffer) of
                   {ok, Host, Port, NewPacket} ->
                     {ok, RemoteSocket} = gen_tcp:connect(Host, Port, [binary, {active, true}]),
                      ok = gen_tcp:send(RemoteSocket, NewPacket),
                     loop(S, State#state{buffer = <<>>, step = connected, remote = RemoteSocket});
                   {error, _} ->
                      gen_tcp:close(S)
                 end;
          {more, NewBuffer} ->
              loop(S, State#state{buffer = NewBuffer})
        end;
    {tcp_closed, Remote} ->
      gen_tcp:close(S);
    {tcp_closed, S} ->
      gen_tcp:close(Remote),
      io:format("socket closed : ~p ~n",[S])
  end.


parse_packet(Data, Buffer) ->
  NewBuffer = <<Buffer/binary, Data/binary>>,
  case binary:match(NewBuffer, <<"\r\n\r\n">>) of
   nomatch ->
     {more, NewBuffer};
    _->
     {done, NewBuffer}
  end.

-spec check_packet(Buffer :: binary()) ->
  {ok, Host :: list(), Port :: integer(), NewBuffer :: binary()} | {error, tuple()}.
check_packet(<<"GET", _/binary>> = Buffer) ->
  do_parse_remote_info(Buffer);
check_packet(<<"POST", _/binary>> = Buffer) ->
  do_parse_remote_info(Buffer);
check_packet(<<"PUT", _/binary>> = Buffer) ->
  do_parse_remote_info(Buffer);
  check_packet(<<"DELETE", _/binary>> = Buffer) ->
    do_parse_remote_info(Buffer);
check_packet(<<"OPTIONS", _/binary>> = Buffer) ->
  do_parse_remote_info(Buffer);
check_packet(<<"HEAD", _/binary>> = Buffer) ->
  do_parse_remote_info(Buffer);
check_packet(<<"TRACE", _/binary>> = Buffer) ->
  do_parse_remote_info(Buffer);
check_packet(_Buffer) ->
    {error, invalid_packet}.

do_parse_remote_info(Buffer) ->
   [Header, Body] = binary:split(Buffer, <<"\r\n\r\n">>),
   [FirstLine | RestHeader] = binary:split(Header, <<"\r\n">>, [global]),
   [Method, RemoteUrl | FirstLineRest] = binary:split(FirstLine, <<" ">>, [global]),
    case uri_string:parse(RemoteUrl) of
        #{host := Host, port := Port, path := Path, scheme := <<"http">>}  ->
          {ok, erlang:binary_to_list(Host), Port, new_packet([Method, Path | FirstLineRest], RestHeader, Body)};
        #{host := Host, path := Path, scheme := <<"http">>} ->
          {ok, erlang:binary_to_list(Host), 443, new_packet([Method, Path| FirstLineRest], RestHeader, Body)};
        Other ->
          {error, {parse_remote_info_invalid, RemoteUrl, Other}}
    end.

new_packet(FirstLine, RestHeader, Body) ->
  %% Proxy开头的header去掉
  NewFirstLine = lists:join(<<" ">>, FirstLine),
  NewHeader = lists:foldl(fun new_packet/2, <<>>, [erlang:list_to_binary(NewFirstLine) | RestHeader]),
  <<NewHeader/binary, "\r\n", Body/binary>>.

new_packet(<<"Proxy",_/binary>>, Acc) ->
  Acc;
new_packet(Header, Acc) ->
  <<Acc/binary, Header/binary, "\r\n">>.

