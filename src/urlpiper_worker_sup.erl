%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/EPLICENSE.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Bill Barnhill.
%% Portions created by Bill Barnhill are Copyright 2012, Bill 
%% Barnhill. All Rights Reserved.''
%% 

%%%-------------------------------------------------------------------
%%% @author Bill Barnhill <>
%%% @copyright (C) 2012, Bill Barnhill
%%% @doc
%%%
%%% @end
%%% Created : 15 Jan 2012 by Bill Barnhill <>
%%%-------------------------------------------------------------------

-module(urlpiper_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,start_link/2, start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link(Url) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Url) ->
    supervisor:start_link({global, {?SERVER, Url}}, ?MODULE, [Url]).
start_link(Url,Format) ->
    supervisor:start_link({global, {?SERVER, Url}}, ?MODULE, [Url, Format]).
start_link(Url, Format, Interval) ->
    supervisor:start_link({global, {?SERVER, Url}}, ?MODULE, [Url, Format, Interval]).

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
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args=[_Url]) ->
    start_child(Args);
init(Args=[_Url, _Format]) ->
    start_child(Args);
init(Args=[_Url, _Format, _Interval]) ->
    start_child(Args).

start_child(Args) ->
    io:format("urlpiper_worker_sup started for as ~p with args: ~p~n", [self(),Args]),
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    ChildSpec = {urlpiper_worker, {urlpiper_worker, start_link, Args},
	      Restart, Shutdown, Type, ['urlpiper_worker']},

    {ok, {SupFlags, [ChildSpec]}}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
