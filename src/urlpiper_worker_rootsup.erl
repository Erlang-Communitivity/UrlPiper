%%%-------------------------------------------------------------------
%%% @author Bill Barnhill <>
%%% @copyright (C) 2012, Bill Barnhill
%%% @doc
%%%  Supervisor for urlpiper
%%% @end
%%% Created : 26 Jan 2012 by Bill Barnhill <>
%%%-------------------------------------------------------------------
-module(urlpiper_worker_rootsup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD_MODULE, urlpiper_worker_sup).
-define(CHILD_NAME, urlpiper_worker_sup).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    erlang:unlink(Pid),
    {ok, Pid}.


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    io:format("urlpiper_worker_rootsup started as ~p~n", [self()]),
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Shutdown = 2000,
    ChildSpec1 = {?CHILD_NAME, {?CHILD_MODULE, start_link, []},
	      transient, Shutdown, supervisor, [?CHILD_MODULE]},
    {ok, {SupFlags, [ChildSpec1]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


