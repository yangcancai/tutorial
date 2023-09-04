%%%-------------------------------------------------------------------
%%% @author cam
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 8月 2023 23:19
%%%-------------------------------------------------------------------
-module(eproxy_accept).
-author("cam").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(eproxy_accept_state, {socket}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(ListenSocket :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ListenSocket) ->
  gen_server:start_link(?MODULE, [ListenSocket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #eproxy_accept_state{}} | {ok, State :: #eproxy_accept_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ListenSocket]) ->
  gen_server:cast(self(), accept),
  {ok, #eproxy_accept_state{socket = ListenSocket}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #eproxy_accept_state{}) ->
  {reply, Reply :: term(), NewState :: #eproxy_accept_state{}} |
  {reply, Reply :: term(), NewState :: #eproxy_accept_state{}, timeout() | hibernate} |
  {noreply, NewState :: #eproxy_accept_state{}} |
  {noreply, NewState :: #eproxy_accept_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #eproxy_accept_state{}} |
  {stop, Reason :: term(), NewState :: #eproxy_accept_state{}}).
handle_call(_Request, _From, State = #eproxy_accept_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #eproxy_accept_state{}) ->
  {noreply, NewState :: #eproxy_accept_state{}} |
  {noreply, NewState :: #eproxy_accept_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #eproxy_accept_state{}}).
handle_cast(accept, State = #eproxy_accept_state{socket = S}) ->
  {ok, S1} = gen_tcp:accept(S),
  {ok, P } = supervisor:start_child(eproxy_sessions_sup, [S1]),
  ok = gen_tcp:controlling_process(S1, P),
  gen_server:cast(self(), accept),
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #eproxy_accept_state{}) ->
  {noreply, NewState :: #eproxy_accept_state{}} |
  {noreply, NewState :: #eproxy_accept_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #eproxy_accept_state{}}).
handle_info(_Info, State = #eproxy_accept_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #eproxy_accept_state{}) -> term()).
terminate(_Reason, _State = #eproxy_accept_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #eproxy_accept_state{},
    Extra :: term()) ->
  {ok, NewState :: #eproxy_accept_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #eproxy_accept_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
