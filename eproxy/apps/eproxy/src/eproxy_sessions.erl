%%%-------------------------------------------------------------------
%%% @author cam
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 8月 2023 23:19
%%%-------------------------------------------------------------------
-module(eproxy_sessions).
-author("cam").

-behaviour(gen_server).

%% API
-export([start_link/1]).
-include_lib("kernel/include/logger.hrl").
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(eproxy_sessions_state, {socket, remote, step = handshake, buffer = <<>>}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(S :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(S) ->
  gen_server:start_link(?MODULE, [S], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #eproxy_sessions_state{}} | {ok, State :: #eproxy_sessions_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([S]) ->
  inet:setopts(S, [{active, once}]),
  ?LOG_INFO("new connect comming ~p", [S]),
  {ok, #eproxy_sessions_state{socket = S}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #eproxy_sessions_state{}) ->
  {reply, Reply :: term(), NewState :: #eproxy_sessions_state{}} |
  {reply, Reply :: term(), NewState :: #eproxy_sessions_state{}, timeout() | hibernate} |
  {noreply, NewState :: #eproxy_sessions_state{}} |
  {noreply, NewState :: #eproxy_sessions_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #eproxy_sessions_state{}} |
  {stop, Reason :: term(), NewState :: #eproxy_sessions_state{}}).
handle_call(_Request, _From, State = #eproxy_sessions_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #eproxy_sessions_state{}) ->
  {noreply, NewState :: #eproxy_sessions_state{}} |
  {noreply, NewState :: #eproxy_sessions_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #eproxy_sessions_state{}}).
handle_cast(_Request, State = #eproxy_sessions_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #eproxy_sessions_state{}) ->
  {noreply, NewState :: #eproxy_sessions_state{}} |
  {noreply, NewState :: #eproxy_sessions_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #eproxy_sessions_state{}}).
handle_info({tcp, S, Data}, State = #eproxy_sessions_state{socket = S, step = handshake, buffer = Buffer}) ->
  inet:setopts(S, [{active, once}]),
case eproxy:parse_packet(Data, Buffer) of
             {done, NewBuffer}  ->
                 case eproxy:check_packet(NewBuffer) of
                   %% http 代理协议
                   {ok, Host, Port, NewPacket} ->
                     {ok, RemoteSocket} = gen_tcp:connect(Host, Port, [binary, {active, true}]),
                      ok = gen_tcp:send(RemoteSocket, NewPacket),
                       {noreply, State#eproxy_sessions_state{buffer = <<>>, step = connected, remote = RemoteSocket}};
                   %% https 代理协议
                   {ok, Host, Port} ->
                     {ok, RemoteSocket} = gen_tcp:connect(Host, Port, [binary, {active, true}]),
                     ok = gen_tcp:send(S, <<"HTTP/1.1 200 OK\r\n\r\n">>),
                       {noreply, State#eproxy_sessions_state{buffer = <<>>, step = connected, remote = RemoteSocket}};
                   {error, _} ->
                      gen_tcp:close(S),
                       {stop, normal, State}
                 end;
          {more, NewBuffer} ->
              {noreply, State#eproxy_sessions_state{buffer = NewBuffer}}
        end;
handle_info({tcp, S, Data}, State = #eproxy_sessions_state{socket = S,remote = Remote}) ->
    inet:setopts(S, [{active, once}]),
    ok = gen_tcp:send(Remote, Data) ,
    {noreply, State};
handle_info({tcp, Remote, Data}, State = #eproxy_sessions_state{socket = S,remote = Remote}) ->
     ok = gen_tcp:send(S, Data) ,
    {noreply, State};
handle_info({tcp_closed, S}, State = #eproxy_sessions_state{socket = S, remote = Remote}) ->
    case Remote of
        undefined ->
            ignore;
        _->
            gen_tcp:close(Remote)
    end,
  {noreply, State};
handle_info({tcp_closed, Remote}, State = #eproxy_sessions_state{socket = S, remote = Remote}) ->
    gen_tcp:close(S),
    {noreply, State};
handle_info(_Info, State = #eproxy_sessions_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #eproxy_sessions_state{}) -> term()).
terminate(_Reason, _State = #eproxy_sessions_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #eproxy_sessions_state{},
    Extra :: term()) ->
  {ok, NewState :: #eproxy_sessions_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #eproxy_sessions_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
