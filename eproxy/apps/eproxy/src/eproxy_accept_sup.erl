%%%-------------------------------------------------------------------
%%% @author cam
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 8月 2023 23:19
%%%-------------------------------------------------------------------
-module(eproxy_accept_sup).
-author("cam").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init([]) ->
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => simple_one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  AChild = #{id => 'eproxy_accept',
    start => {'eproxy_accept', start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => ['eproxy_accept']},

  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
